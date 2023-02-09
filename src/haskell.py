from subprocess import run
from pathlib import Path
import json
import pprint
from dataclasses import dataclass
from pydantic import BaseModel
from src.marco import Marco, RuleSet
from src.haskell_types import *

Point: TypeAlias = tuple[int, int]
Span: TypeAlias = tuple[Point, Point]
Fid = NewType("FunctionMetaId", str)
Vid = NewType("VariableMetaId", int)
Rid = NewType("RuleId", int)
pp = pprint.PrettyPrinter(indent=2).pprint

counter = Int(0)

def pretty(node, indent=0):
    print(' ' * indent, node['tag'])
    pretty(node['contents'][1], indent + 2)


def get_location(ann: dict[str, Any]) -> Span:
    from_point = ann.get('loc', {'from': False}).get('from', False)
    to_point = ann.get('loc', {'to': False}).get('to', False)
    if from_point is False or to_point is False:
        raise ValueError("Tried to extract SrcSpan from an invalid location")
    else:
        return (from_point['line'], from_point['col']), (to_point['line'], to_point['col'])



replacting_var = "Replacing var"
replacting_literal = "Replacing literal"
replacting_function = "Replacing function"
replacting_type = "Replacing type"
replace_to_bool = "Replacing to bool"
no_suggestion = "No suggestion"

@dataclass
class FunctionMeta:
    """ Metadata for functions during tc"""
    fid: Fid
    function_var: TypeVar
    arg_vars: list[TypeVar]
    return_var: TypeVar


@dataclass
class VariableMeta:
    """ Metadata for variables during tc"""
    vid: Vid
    name: str
    line: int
    col: int
    module: str
    internal_name: str
    callstack: list[Fid]
    is_fresh: bool
    is_func: bool


@dataclass
class Rule:
    """ A rule is one constraint associated with a location"""
    rid: Rid
    clause: BoolRef
    callstack: list[Fid]
    loc: Span
    implicit: bool
    check_var: list[TypeVar]
    suggestion: str


# @dataclass
class Slice(BaseModel):
    slice_id: int
    loc: Span
    appears: list[int]


# @dataclass
class TError(BaseModel):
    error_id: int
    mus_list: list[RuleSet]
    mcs_list: list[RuleSet]
    slices: list[Slice]


class System:
    def __init__(self, code_dir):
        self.code_dir = code_dir
        self.project_dir = Path(__file__).parent.parent
        self.parser_bin = str(self.project_dir / "bin" / "haskell-tool-exe.exe")
        self.reset()

    def reset(self):
        result = run([self.parser_bin, self.code_dir], shell=True, check=True, capture_output=True)
        self.parsed_data = json.loads(result.stdout)
        self.asts = [c['ast'] for c in self.parsed_data['contents']]
        self.variable_counter: int = 0
        self.rule_counter: int = 0
        self.rules: list[Rule] = []
        self.variable_table: list[VariableMeta] = []
        self.function_table: list[FunctionMeta] = []
        self.propagate_counter = 0
        self.errors: list[TError] = []
        self.signatures= {}

    def propagate(self) -> int:
        v = self.propagate_counter
        self.propagate_counter += 1
        return v

    def make_function(self, fid: Fid, function_var: TypeVar, arg_vars: list[TypeVar], return_var: TypeVar) -> Int:
        self.function_table.append(FunctionMeta(
            fid=fid,
            function_var=function_var,
            arg_vars=arg_vars,
            return_var=return_var
        ))
        return fid

    def make_variable(self, name: str, line: int, col: int, module: str, callstack: list[Fid]) -> TypeVar:
        vid = self.variable_counter
        self.variable_counter += 1
        internal_name = f'{name}.{vid}'
        self.variable_table.append(
            VariableMeta(
                vid=Vid(vid),
                name=name,
                line=line,
                col=col,
                module=module,
                internal_name=internal_name,
                callstack=callstack,
                is_fresh=False,
                is_func=False
            )
        )
        var = Const(internal_name, Type)
        return var

    def lookup_variable(
            self,
            name: str,
            line: Optional[int] = None,
            col: Optional[int] = None,
            callstack: Optional[list[Fid]] = None,
            module: Optional[str] = None) -> (bool, Optional[TypeVar]):
        if line is None or col is None or callstack == []:
            vs = [v for v in self.variable_table if v.callstack == [] and v.name == name]
            return (False, None) if vs == [] else (True, Const(vs[0].internal_name, Type))
        else:
            vs = [v for v in self.variable_table if v.line == line and v.col == col and v.name == name]
            return (False, None) if vs == [] else (True, Const(vs[0].internal_name, Type))

    def show_variables(self):
        print("Variables:")
        for v in self.variable_table:
            print(v)
        print("")

    def show_functions(self):
        print("Function contexts:")
        for v in self.function_table:
            print(v)
        print("")

    def show_rules(self):
        print("Rules:")
        for v in self.rules:
            print(v)
        print("")

    def fresh(self, callstack: list[Fid], is_func=False) -> TypeVar:
        vid = self.variable_counter
        self.variable_counter += 1
        internal_name = f'fresh.{vid}'
        self.variable_table.append(
            VariableMeta(
                name=internal_name,
                callstack=callstack,
                internal_name=internal_name,
                module="",
                line=0,
                col=0,
                vid=Vid(vid),
                is_fresh=True,
                is_func=is_func
            ))
        var: TypeVar = Const(internal_name, Type)
        return var

    def fresh_n(self, n: int, callstack: list[Fid]) -> list[TypeVar]:
        fresh_vars = []
        for i in range(n):
            fresh_vars.append(self.fresh(callstack))
        return fresh_vars

    def add_rule(self, clause: BoolRef, callstack: list[Fid], loc: Span, check_var: list[TypeVar],
                 suggestion:str=no_suggestion, implicit=False, ):
        rid = Rid(self.rule_counter)
        self.rule_counter += 1
        self.rules.append(
            Rule(callstack=callstack,
                 clause=clause, loc=loc, rid=rid, check_var=check_var, implicit=implicit, suggestion=suggestion)
        )

    def generate_fun_intro(self, arg_vars: list[TypeVar], body: BoolRef, fun_ref: TypeVar):
        if len(arg_vars) == 2:
            [arg1, arg2] = arg_vars
            _body = body
        else:
            [arg1, *args_rest] = arg_vars
            arg2 = self.fresh([])
            _body = And(
                arg2 == Type.Propagated(self.propagate()),
                arg1 != arg2,
                self.generate_fun_intro(args_rest, body, arg2)
            )
        proxy = self.fresh([])
        clause = ForAll(
            proxy,
            Implies(
                is_func(proxy, fun_ref),
                ForAll([arg1, arg2], Implies(
                    proxy == fun(arg1, arg2),
                    _body
                ))
            )
        )
        return clause

    def generate_fun_elim(self, arg_vars: list[TypeVar], body: list[BoolRef], fun_ref: TypeVar):
        if len(arg_vars) == 2:
            [arg1, arg2] = arg_vars
            _body = body
        else:
            [arg1, *args_rest] = arg_vars
            arg2 = self.fresh([])
            _body = self.generate_fun_elim(args_rest, body, arg2)

        proxy = self.fresh([])
        return [is_func(proxy, fun_ref), proxy == fun(arg1, arg2), *_body]

    def solve(self, solver, rids: set[int]) -> (bool, list[BoolRef]):
        defs = []
        active_rules = [r for r in self.rules if not r.implicit and r.rid in rids] + [r for r in self.rules if
                                                                                      r.implicit]
        for c in self.function_table:
            universal_vars = c.arg_vars + [c.return_var]
            universal_var_names = [i.decl().name() for i in universal_vars]
            existential_vars = [Const(v.internal_name, Type) for v in self.variable_table if
                                c.fid in v.callstack and v.internal_name not in universal_var_names]
            body = And([r.clause for r in active_rules if c.fid in r.callstack]) if existential_vars == [] else Exists(
                existential_vars,
                And([r.clause for r in active_rules if c.fid in r.callstack])
            )
            f_intro = self.generate_fun_intro([*c.arg_vars, c.return_var], body, c.function_var)
            f_elim = self.generate_fun_elim(
                [*c.arg_vars, c.return_var],
                [r.clause for r in active_rules if c.fid in r.callstack],
                c.function_var
            )
            # pp(f_elim)
            defs.append(f_intro)
            for elim in f_elim:
                defs.append(elim)

        tlds = [r.clause for r in active_rules if r.callstack == []]

        solver.add(defs)
        solver.add(tlds)

        # pp(tlds)
        if solver.check().r == 1:
            return (True, defs + tlds)
        else:
            return (False, [])

    def type_check(self) -> list[TError]:
        self.reset()
        for ast in self.asts:
            self.check_node(ast, Type.Unit, [])

        succeed, _ = self.solve(Solver(), {r.rid for r in self.rules})
        if succeed:
            print('sat')
            return []
        else:
            print('unsat')

            def sat_fun(ids):
                solver = Solver()
                s, _ = self.solve(solver, ids)
                return s

            marco = Marco(rules={r.rid for r in self.rules if not r.implicit}, sat_fun=sat_fun)
            marco.run()
            marco.analyse()
            t_errors = []
            for i, island in enumerate(marco.islands):
                slices = []
                for (ruleId, appears) in island.rule_likelihood:
                    rule = [r for r in self.rules if r.rid == ruleId][0]
                    slices.append(Slice(slice_id=rule.rid, loc=rule.loc, appears=appears))
                t_error = TError(
                    error_id=i,
                    mus_list=island.mus_list,
                    mcs_list=island.mcs_list,
                    slices=slices)
                t_errors.append(t_error)
            self.errors = t_errors
            return t_errors

    def get_name_var(self, node, callstack: list[Fid]) -> (TypeVar, str):
        [ann, ident] = node['contents']
        if ident[0] == "_":
            print("Type hole")
            hole_var = self.fresh(callstack=callstack)
            self.holes.append({'ident': ident, 'var': hole_var})
            return hole_var

        elif ann['scope']['type'] == 'ValueBinder':
            success, var = self.lookup_variable(
                ident,
                line=ann['loc']['from']['line'],
                col=ann['loc']['from']['col'],
                callstack=callstack
            )
            if success:
                return var
            else:
                var = self.make_variable(
                    ident,
                    line=ann['loc']['from']['line'],
                    col=ann['loc']['from']['col'],
                    module='Test',
                    callstack=callstack
                )
                return var

        elif ann['scope']['type'] == 'GlobalSymbol':
            success, var = self.lookup_variable(ident)
            if success:
                return var
            else:
                var = self.make_variable(
                    ident,
                    line=0,
                    col=0,
                    module='Test',
                    callstack=[]
                )
                return var

        elif ann['scope']['type'] == 'LocalValue':
            success, var = self.lookup_variable(
                ident,
                line=ann['scope']['loc']['line'],
                col=ann['scope']['loc']['col'],
            )
            if success:
                return var
            else:
                var = self.make_variable(
                    ident,
                    line=ann['scope']['loc']['line'],
                    col=ann['scope']['loc']['col'],
                    module='Test',
                    callstack=callstack
                )
                return var
        else:
            print(node)
            raise NotImplementedError

    def check_node(self, node, term: TypeVar, callstack: list[Fid], implicit=False):
        match node:
            case {'tag': 'Module', 'contents': [ann, _, _, _, decls]}:
                for decl in decls:
                    self.check_node(decl, term, callstack, implicit=implicit)

            case {'tag': 'PatBind', 'contents': [ann, pat, rhs, _]}:
                var = self.fresh(callstack)
                self.check_node(pat, var, callstack, implicit=True)
                self.check_node(rhs, var, callstack, implicit=implicit)

            case {'tag': 'FunBind', 'contents': [ann, matches]}:
                [_, fname, fargs, _, _] = matches[0]['contents']
                var_fun = self.get_name_var(fname, callstack)
                fun_name = var_fun.decl().name()
                callstack = [*callstack, fun_name]
                var_args = [self.fresh(callstack) for _ in fargs]
                var_rhs = self.fresh(callstack)
                self.make_function(fid=Fid(fun_name), function_var=var_fun, arg_vars=var_args, return_var=var_rhs)
                # typesig = self.signatures.get(fun_name, False)
                # if typesig:
                #

                for match in matches:
                    [ann, _, args, rhs, wheres] = match['contents']
                    for arg, var_arg in zip(args, var_args):
                        self.check_node(arg, var_arg, callstack, implicit=implicit)
                    self.check_node(rhs, var_rhs, callstack, implicit=implicit)

            case {'tag': 'TypeSig', 'contents': [ann, names, sig]}:
                sig_var = self.fresh(callstack)
                self.check_node(sig, sig_var, callstack, implicit=implicit)
                for name in names:
                    name_var = self.get_name_var(name, callstack)
                    name = name_var.decl().name()
                    self.add_rule(name_var == sig_var, callstack, get_location(ann),
                                  check_var=[name_var, sig_var],
                                  suggestion=replacting_type,
                                  implicit=True)

            case {'tag': 'UnGuardedRhs', 'contents': [ann, exp]}:
                self.check_node(exp, term, callstack, implicit=implicit)

            # Exp types:
            case {'tag': 'Lit', 'contents': [ann, lit]}:
                self.check_node(lit, term, callstack, implicit=implicit)
            case {'tag': 'Con', 'contents': [ann, qname]}:
                if qname['tag'] == 'UnQual' and qname['contents'][1]['contents'][1] in ['True', 'False']:
                    self.add_rule(term == t_bool, callstack, get_location(ann),
                                  check_var=[term],
                                  suggestion=replacting_literal,
                                  implicit=True)
                else:
                    raise NotImplementedError()

            case {'tag': 'Var', 'contents': [ann, qname]}:
                # pp.pprint(qname)
                if qname['tag'] == 'UnQual' and qname['contents'][1]['contents'][1] == 'undefined':
                    bottom_val = self.fresh(callstack)
                    self.add_rule(term == bottom_val, callstack, get_location(ann),
                                  check_var=[term],
                                  suggestion=no_suggestion,
                                  implicit=implicit)
                else:
                    name = self.fresh([])
                    self.add_rule(name == term, callstack, get_location(ann),
                                  check_var=[term],
                                  suggestion=replacting_var,
                                  implicit=implicit
                                  )
                    self.check_node(qname, name, callstack, implicit=implicit)

            case {'tag': 'App', 'contents': [ann, exp1, exp2]}:
                var1 = self.fresh(callstack, is_func=True)
                var2 = self.fresh(callstack)

                self.check_node(exp2, var2, callstack, implicit=implicit)

                self.add_rule(var1 == fun(var2, term), callstack=callstack,
                              check_var=[var1, fun(var2, term)],
                              suggestion=replacting_function,
                              loc=get_location(exp1['contents'][0]))
                name = self.fresh(callstack)

                self.add_rule(is_func(var1, name), callstack=callstack, loc=get_location(ann),
                              check_var=[],
                              suggestion=no_suggestion,
                              implicit=True)
                self.check_node(exp1, name, callstack, implicit=implicit)

            case {'tag': "If", 'contents': [ann, cond, leftbranch, rightbranch]}:
                var_cond = self.fresh(callstack)
                self.check_node(cond, var_cond, callstack, )
                self.add_rule(var_cond == t_bool, callstack, get_location(cond['contents'][0]),
                              check_var=[var_cond],
                              suggestion=replace_to_bool,
                              implicit=implicit
                              )
                self.check_node(leftbranch, term, callstack, implicit)
                self.check_node(rightbranch, term, callstack, implicit)

            # Lit nodes:
            case {'tag': 'Char', 'contents': [ann, _, _]}:
                self.add_rule(term == Type.Char, callstack, get_location(ann),
                              check_var=[term],
                              suggestion=replacting_literal,
                              implicit=implicit)

            case {'tag': 'String', 'contents': [ann, _, _]}:
                self.add_rule(term == list_of(Type.Char), callstack, get_location(ann),
                              check_var=[term],
                              suggestion=replacting_literal,
                              implicit=implicit)

            case {'tag': 'Int', 'contents': [ann, _, _]}:
                self.add_rule(term == Type.Int, callstack, get_location(ann),
                              check_var=[term],
                              suggestion=replacting_literal,
                              implicit=implicit)

            case {'tag': 'Frac', 'contents': [ann, _, _]}:
                self.add_rule(term == Type.Float, callstack, get_location(ann),
                              check_var=[term],
                              suggestion=replacting_literal,
                              implicit=implicit)

            # Types
            case {'tag': 'TyCon', 'contents': [ann, qname]}:
                if qname['tag'] == 'UnQual':
                    type_literal = qname['contents'][1]['contents'][1]
                    match type_literal:
                        case "Int":
                            self.add_rule(term == t_int, callstack, get_location(ann),
                                          suggestion=replacting_type,
                                          check_var=[term],
                                          implicit=implicit)
                        case "Char":
                            self.add_rule(term == t_char, callstack, get_location(ann),
                                          check_var=[term],
                                          suggestion=replacting_type,
                                          implicit=implicit)
                        case "String":
                            self.add_rule(term == list_of(t_char), callstack, get_location(ann),
                                          check_var=[term],
                                          suggestion=replacting_type,
                                          implicit=implicit)
                        case "Float":
                            self.add_rule(term == t_float, callstack, get_location(ann),
                                          check_var=[term],
                                          suggestion=replacting_type,
                                          implicit=implicit)
                        case "Bool":
                            self.add_rule(term == t_bool, callstack, get_location(ann),
                                          check_var=[term],
                                          suggestion=replacting_type,
                                          implicit=implicit)
                        case _:
                            raise NotImplementedError
                else:
                    raise NotImplementedError

            case {'tag': 'TyFun', 'contents': [ann, t1, t2]}:
                var1 = self.fresh(callstack)
                var2 = self.fresh(callstack)
                var_fun = self.fresh(callstack)
                self.check_node(t1, var1, callstack, implicit=implicit)
                self.check_node(t2, var2, callstack, implicit=implicit)
                # needle = self.fresh(callstack)
                # self.add_rule(needle == fun(var1, var2), callstack=callstack,check_var=[], loc=get_location(ann), implicit=True)
                self.add_rule(var_fun == fun(var1, var2), callstack=callstack,
                              check_var=[var_fun, fun(var1, var2)],
                              suggestion=replacting_type,
                              loc=get_location(ann))

                self.add_rule(is_func(var_fun, term), callstack=callstack, loc=get_location(ann),
                              check_var=[],
                              implicit=True)
                # ForAll(v, Implies(is_func))

            # Patterns
            case {'tag': 'PVar', 'contents': [ann, name]}:
                var = self.get_name_var(name, callstack)
                self.add_rule(var == term, callstack, get_location(ann),
                              check_var=[var, term],
                              implicit=True)

            case {'tag': 'PLit', 'contents': [ann, _, lit]}:
                self.check_node(lit, term, callstack, implicit)

            case {'tag': 'PApp', 'contents': [ann, pname, pargs]}:
                if pname['tag'] == 'UnQual' and pname['contents'][1]['contents'][1] in ['True', 'False']:
                    self.add_rule( term == t_bool, callstack, get_location(ann),
                                   check_var=[term],
                                   suggestion=replacting_literal,
                                   implicit=implicit)
            # Namings
            case {'tag': 'UnQual', 'contents': [ann, name]}:
                var = self.get_name_var(name, callstack)
                self.add_rule(var == term, callstack, get_location(ann),
                              check_var=[var, term],
                              suggestion=no_suggestion,
                              implicit=True)

            case {'tag': 'Special', 'contents': [ann, specialcon]}:
                match specialcon['tag']:
                    case "UnitCon":
                        self.add_rule(term == Type.Unit, callstack, get_location(ann),
                                      check_var=[term],
                                      suggestion=replacting_literal,
                                      implicit=implicit)
                    case "ListCon":
                        raise NotImplementedError("ListCon")
                    case "FunCon":
                        raise NotImplementedError("FunCon")
                    case "Cons":
                        raise NotImplementedError("Cons")
                    case "ExprHole":
                        raise NotImplementedError("ExprHole")

            case _:
                print("Unknown node type: ", node.get('type'))
                pp(node)
                raise NotImplementedError

    def inference(self, error_id: int, mcs_id: int) -> list[str]:
        error = [e for e in self.errors if e.error_id == error_id][0]
        mcs = [mcs for mcs in error.mcs_list if mcs.setId == mcs_id][0]
        mss = [r for r in self.rules if r.rid not in mcs.rules]
        mss_ids = {r.rid for r in mss}
        solver = Solver()
        result, asserts = self.solve(solver, mss_ids)
        # print([r for r in self.rules if r.rid in mcs.rules])
        model = solver.model()
        text = (Path(self.code_dir) / 'Test.hs').read_text()
        lines = text.splitlines()

        def is_propagated(value: TypeVar) -> bool:
            return value.decl().name() == "Propagated"

        def get_propagation_id(value: TypeVar) -> int:
            return value.arg(0)

        def walk(var_table: list[tuple[TypeVar, TypeVar]], needles: list[TypeVar]):
            # print(needles)
            results = []
            for n in needles:
                if is_app(n) and n.decl().name() == "Fun":
                    arg1_types = walk(var_table, [n.arg(0)])
                    arg2_types = walk(var_table, [n.arg(1)])
                    for arg1 in arg1_types:
                        for arg2 in arg2_types:
                            results.append(fun(arg1, arg2))
                elif n.decl().name()[0].isupper():
                    results.append(n)
                elif n.decl().name()[0].islower():
                    for (a, b) in var_table:
                        if a.__str__() == n.decl().name():
                            if is_app(b) and b.decl().name() == "Fun":
                                arg1_types = walk(var_table, [b.arg(0)])
                                arg2_types = walk(var_table, [b.arg(1)])
                                for arg1 in arg1_types:
                                    for arg2 in arg2_types:
                                        results.append(fun(arg1, arg2))
                            elif b.decl().name()[0].islower():
                                results += walk(var_table, [b])
                            elif b.decl().name()[0].isupper():
                                results.append(b)

            return results


        func_mappings = []
        for clause in asserts:
            if is_app(clause) and clause.decl().name() == 'is_func':
                func_mappings.append((clause.arg(1), clause.arg(0)))

        propagations: dict[int, list[TypeVar]] = {}
        for v in model:
            if is_app(model[v]) and is_propagated(model[v]):
                propagation_id = model[v].arg(0)
                old_value = propagations.get(propagation_id, [])
                propagations[propagation_id] = old_value + [Const(v.name(), Type)]

        for v in model:
            value = model[v]
            if is_app(value) and value.decl().name() == "Fun":
                if is_propagated(value.arg(1)):
                    propagation_id = get_propagation_id(value.arg(1))
                    propagated_vars = propagations[propagation_id]
                    for p in propagated_vars:
                        func_mappings.append((Const(v.name(), Type), Type.Fun(value.arg(0), p)))
                else:
                    func_mappings.append((Const(v.name(), Type), value))
            elif v.name() == "is_func":
                continue
            elif hasattr(value, 'decl') and value.decl().name() == "Propagated":
                continue
            elif hasattr(value, 'name')  and value.name() == "Propagated":
                continue
            elif hasattr(value, 'decl')  and value.decl().name() == "Any":
                continue
            elif hasattr(value, 'name') and value.name() == "Any":
                continue
            else:
                func_mappings.append((v, value))
        suggestions = []
        for rids in mcs.rules:
            rule = [r for r in self.rules if r.rid == rids][0]
            var_of_interest = [v for v in rule.check_var]
            types = walk(func_mappings, var_of_interest)
            loc_from, loc_to = rule.loc
            from_line  = loc_from[0] - 1
            from_col = loc_from[1] - 1
            to_col = loc_to[1] -1
            part = lines[from_line][from_col:to_col]



            if rule.suggestion == replacting_var:
                if rule.callstack == []:
                    s = f"Replace the variable '{part}' to a different variable"
                else:
                    fun_name = rule.callstack[0].split('.')[0]

                    s = f"Replace the variable '{part}' to a different expression to make the function {fun_name} agree with its instantiations"
                suggestions.append(s)
            elif rule.suggestion == replacting_type:
                s = f"Replace the type annotation '{part}' to {types[0]}"
                suggestions.append(s)
            elif rule.suggestion == replacting_function:
                s =  f"Replace the expression '{part}'  to function of type {types[0]}"
                suggestions.append(s)
            elif rule.suggestion == replacting_literal:
                s = f"Replace the expression '{part}' to a instance of type {types[0]}"
                suggestions.append(s)
            elif rule.suggestion == replace_to_bool:
                s = f"Replace the expression '{part}' to an instance of type Bool, such as True/False"
                suggestions.append(s)
            else:
                continue
        return suggestions


if __name__ == "__main__":
    system = System(code_dir=str(Path(__file__).parent.parent / "example"))
    e = system.type_check()
    output = system.inference(0, 1)
