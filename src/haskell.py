from subprocess import run
from pathlib import Path
import json
from z3 import *
import pprint
from dataclasses import dataclass
from typing import TypeAlias, NewType, Optional, Any

TypeVar: TypeAlias = DatatypeRef
Point: TypeAlias = tuple[int, int]
Span: TypeAlias = tuple[Point, Point]
Fid = NewType("FunctionMetaId", str)
Vid = NewType("VariableMetaId", int)
Rid = NewType("RuleId", int)
pp = pprint.PrettyPrinter(indent=2)

Type = Datatype('Type')
Type.declare('Char')
Type.declare('Int')
Type.declare('Unit')
Type.declare('Float')
Type.declare('List', ('elem', Type))
Type.declare('Tup2', ('part1', Type), ('part2', Type))
Type.declare('Tup3', ('part1', Type), ('part2', Type), ('part3', Type))
Type.declare('Tup4', ('part1', Type), ('part2', Type), ('part3', Type), ('part4', Type))
Type.declare('Fun', ('arg', Type), ('ret', Type))
Type = Type.create()

t_char = Type.Char
t_int = Type.Int
t_float = Type.Float
t_func = Type.Fun

apply = Function('apply', Type, Type, Type, BoolSort())


def list_of(t: TypeVar) -> TypeVar:
    """ Make a list type of type t"""
    return Type.List(t)


def fun_of(*ts) -> TypeVar:
    """ Make a function type of type Fun(t[0], t[1], ...)"""
    print(ts)
    match len(ts):
        case 0:
            raise ValueError
        case 1:
            return ts[0]
        case 2:
            return Type.Fun(ts[0], ts[1])
        case _:
            return Type.Fun(ts[0], fun_of(*ts[1:]))


def get_location(ann: dict[str, Any]) -> Span:
    from_point = ann.get('loc', {'from': False}).get('from', False)
    to_point = ann.get('loc', {'to': False}).get('to', False)
    if from_point is False or to_point is False:
        raise ValueError("Tried to extract SrcSpan from an invalid location")
    else:
        return (from_point['line'], from_point['col']), (to_point['line'], to_point['col'])


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


@dataclass
class Rule:
    """ A rule is one constraint associated with a location"""
    rid: Rid
    clause: BoolRef
    callstack: list[Fid]
    loc: Span


class System:
    def __init__(self):
        self.project_dir = Path(__file__).parent.parent
        parser_bin = str(self.project_dir / "bin" / "haskell-tool-exe.exe")
        code_dir = str(self.project_dir / "example")
        result = run([parser_bin, code_dir], shell=True, check=True, capture_output=True)
        self.parsed_data = json.loads(result.stdout)
        self.asts = [c['ast'] for c in self.parsed_data['contents']]
        # self.solver = Solver()
        self.variable_counter: int = 0
        self.rule_counter: int = 0
        self.rules: list[Rule] = []
        self.variable_table: list[VariableMeta] = []
        self.function_table: list[FunctionMeta] = []

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
                is_fresh=False
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

    def fresh(self, callstack: list[Fid]) -> TypeVar:
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
                is_fresh=True
            ))
        var: TypeVar = Const(internal_name, Type)
        return var

    def fresh_n(self, n: int, callstack: list[Fid]) -> list[TypeVar]:
        fresh_vars = []
        for i in range(n):
            fresh_vars.append(self.fresh(callstack))
        return fresh_vars

    def add_rule(self, clause: BoolRef, callstack: list[Fid], loc: Span):
        rid = Rid(self.rule_counter)
        self.rule_counter += 1
        self.rules.append(Rule(callstack=callstack, clause=clause, loc=loc, rid=rid))

    def make_clause(self, proxies: list[TypeVar], arg_vars: list[TypeVar], function_var: TypeVar, return_var: TypeVar,
                    body: BoolRef) -> BoolRef:
        """Proxies: Used in the apply relation, and universal var.
        Generally the universal vars are [proxy, first var, rest vars]
        """
        if len(proxies) == 1 and len(arg_vars) == 1:
            proxy = proxies[0]
            arg = arg_vars[0]
            return ForAll([proxy, arg, return_var],
                          Implies(
                              And(proxy == function_var, apply(proxy, arg, return_var)),
                              body
                          )
                          )
        else:
            [proxy, *rest_proxy] = proxies
            [arg, *rest_args] = arg_vars
            result = self.fresh([])
            return ForAll([proxy, arg, result],
                          Implies(
                              And(proxy == function_var, apply(proxy, arg, result)),
                              self.make_clause(rest_proxy, rest_args, result, return_var, body)
                          ))

    def solve(self, rids: set[Rid]) -> bool:
        defs = []
        active_rules = [r for r in self.rules if r.rid in rids]
        solver = Solver()
        for c in self.function_table:
            function_proxies = self.fresh_n(len(c.arg_vars), [])
            universal_vars = c.arg_vars + [c.return_var]
            universal_var_names = [i.decl().name() for i in universal_vars]
            existential_vars = [Const(v.internal_name, Type) for v in self.variable_table if
                                c.fid in v.callstack and v.internal_name not in universal_var_names]
            body = Exists(
                existential_vars,
                And([r.clause for r in active_rules if c.fid in r.callstack])
            )
            defs.append(self.make_clause(
                proxies=function_proxies,
                arg_vars=c.arg_vars,
                function_var=c.function_var,
                return_var=c.return_var,
                body=body
            ))

        tlds = [r.clause for r in active_rules if r.callstack == []]
        solver.add(defs)
        solver.add(tlds)
        return solver.check().r == 1

        # print('defs:')
        # print(defs)
        # print('tlds:')
        # print(tlds)
        # self.solver.add(defs)
        # self.solver.add(tlds)
        # print('=====================')
        # user_defined_vars = [v.internal_name for v in self.variable_table if not v.is_fresh]
        # if self.solver.check().r == 1:
        #     m = self.solver.model()
        #     for val in m:
        #         if val.name() in user_defined_vars:
        #             print(val, "=", m[val])
        # else:
        #     print('The code is not well-typed')

    def type_check(self):
        for ast in self.asts:
            self.check_node(ast, Type.Unit, [])

    def get_name_var(self, node, callstack: list[Fid]) -> (TypeVar, str):
        pp.pprint(node)
        match node:
            case {'tag': 'Ident', 'contents': [ann, ident]}:
                if ann['scope']['type'] == 'ValueBinder':
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

    def check_node(self, node, term: TypeVar, callstack: list[Fid]):
        match node:
            case {'tag': 'Module', 'contents': [ann, _, _, _, decls]}:
                for decl in decls:
                    self.check_node(decl, term, callstack)

            case {'tag': 'PatBind', 'contents': [ann, pat, rhs, _]}:
                var = self.fresh(callstack)
                self.check_node(pat, var, callstack)
                self.check_node(rhs, var, callstack)

            case {'tag': 'FunBind', 'contents': [ann, matches]}:
                for m in matches:
                    self.check_node(m, term, callstack)

            case {'tag': 'TypeSig', 'contents': [ann, names, sig]}:
                sig_var = self.fresh(callstack)
                self.check_node(sig, sig_var, callstack)
                for name in names:
                    name_var = self.get_name_var(name, callstack)
                    self.add_rule(name_var == sig_var, callstack, get_location(ann))

            case {'tag': 'Match', 'contents': [ann, name, args, rhs, wheres]}:
                var_fun = self.get_name_var(name, callstack)
                fun_name = var_fun.decl().name()

                callstack = [*callstack, fun_name]

                var_args = [self.fresh(callstack) for _ in args]
                var_rhs = self.fresh(callstack)

                self.make_function(fid=Fid(fun_name), function_var=var_fun, arg_vars=var_args, return_var=var_rhs)
                for arg, var_arg in zip(args, var_args):
                    self.check_node(arg, var_arg, callstack)
                self.check_node(rhs, var_rhs, callstack)

            case {'tag': 'UnGuardedRhs', 'contents': [ann, exp]}:
                self.check_node(exp, term, callstack)

            # Exp types:
            case {'tag': 'Lit', 'contents': [ann, lit]}:
                self.check_node(lit, term, callstack)

            case {'tag': 'Var', 'contents': [ann, qname]}:
                self.check_node(qname, term, callstack)

            case {'tag': 'App', 'contents': [ann, exp1, exp2]}:
                var1 = self.fresh(callstack)
                var2 = self.fresh(callstack)
                self.check_node(exp1, var1, callstack)
                self.check_node(exp2, var2, callstack)
                self.add_rule(apply(var1, var2, term), callstack, get_location(ann))

            # Lit nodes:
            case {'tag': 'Char', 'contents': [ann, _, _]}:
                self.add_rule(term == Type.Char, callstack, get_location(ann))

            case {'tag': 'String', 'contents': [ann, _, _]}:
                self.add_rule(term == list_of(Type.Char), callstack, get_location(ann))

            case {'tag': 'Int', 'contents': [ann, _, _]}:
                self.add_rule(term == Type.Int, callstack, get_location(ann))

            case {'tag': 'Frac', 'contents': [ann, _, _]}:
                self.add_rule(term == Type.Float, callstack, get_location(ann))

            # Types
            case {'tag': 'TyCon', 'contents': [ann, qname]}:
                if qname['tag'] == 'UnQual':
                    type_literal = qname['contents'][1]['contents'][1]
                    match type_literal:
                        case "Int":
                            self.add_rule(term == t_int, callstack, get_location(ann))
                        case "Char":
                            self.add_rule(term == t_char, callstack, get_location(ann))
                        case "String":
                            self.add_rule(term == list_of(t_char), callstack, get_location(ann))
                        case "Float":
                            self.add_rule(term == t_float, callstack, get_location(ann))
                else:
                    raise NotImplementedError

            # Patterns
            case {'tag': 'PVar', 'contents': [ann, name]}:
                var = self.get_name_var(name, callstack)
                self.add_rule(var == term, callstack, get_location(ann))

            # Namings
            case {'tag': 'UnQual', 'contents': [ann, name]}:
                var = self.get_name_var(name, callstack)
                self.add_rule(var == term, callstack, get_location(ann))

            case _:
                print("Unknown node type: ", node.get('type'))
                pp.pprint(node)
                raise NotImplementedError


if __name__ == "__main__":
    system = System()
    system.type_check()
    system.show_variables()
    system.show_functions()

    system.solve(system.rules)
