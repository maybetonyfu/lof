from subprocess import run
import json
from typing import Any, TypeAlias, NewType
from src.prolog import *
from devtools import debug

Point: TypeAlias = tuple[int, int]
Span: TypeAlias = tuple[Point, Point]
Fid = NewType("FunctionId", str)
Vid = NewType("VariableId", int)
Rid = NewType("RuleId", int)

t_unit = atom('unit')
t_bool = atom('bool')
t_char = atom('char')
t_int = atom('int')
t_float = atom('float')
T = var('T')


def list_of(elem: Term):
    return struct('list', elem)


def fun_of(head: Term, body: Term):
    return struct('function', head, body)


class FunctionMeta(BaseModel):
    """ Metadata for functions during tc"""
    fid: Fid
    function_var: Term
    arg_vars: list[Term]
    return_var: Term


class VariableMeta(BaseModel):
    """ Metadata for variables during tc"""
    vid: Vid
    name: str
    line: int
    col: int
    module: str
    term: Term
    callstack: list[Fid]


class Rule(BaseModel):
    """ A rule is one constraint associated with a location"""
    rid: Rid
    body: Term
    head: str


def get_location(ann: dict[str, Any]) -> Span:
    from_point = ann.get('loc', {'from': False}).get('from', False)
    to_point = ann.get('loc', {'to': False}).get('to', False)
    if from_point is False or to_point is False:
        raise ValueError("Tried to extract SrcSpan from an invalid location")
    else:
        return (from_point['line'], from_point['col']), (to_point['line'], to_point['col'])


class System:
    def __init__(self, code_dir):
        self.code_dir = code_dir
        self.project_dir = Path(__file__).parent.parent
        self.parser_bin = str(self.project_dir / "bin" / "haskell-tool-exe.exe")
        result = run([self.parser_bin, self.code_dir], shell=True, check=True, capture_output=True)
        self.parsed_data = json.loads(result.stdout)
        self.asts = [c['ast'] for c in self.parsed_data['contents']]
        self.variable_counter: int = 0
        self.function_counter: int = 0
        self.rule_counter: int = 0
        self.variable_table: list[VariableMeta] = []
        self.function_table: list[FunctionMeta] = []
        self.rules: list[Rule] = []

    def make_function(self, fid: Fid, function_var: Term, arg_vars: list[Term], return_var: Term) -> Fid:
        self.function_table.append(FunctionMeta(
            fid=fid,
            function_var=function_var,
            arg_vars=arg_vars,
            return_var=return_var
        ))
        return fid

    def make_variable(self, name: str, line: int, col: int, module: str, callstack: list[Fid]) -> Term:
        vid = self.variable_counter
        self.variable_counter += 1
        internal_name = f'_{name}_{vid}'
        term = var(internal_name)
        self.variable_table.append(
            VariableMeta(
                vid=Vid(vid),
                name=name,
                line=line,
                col=col,
                module=module,
                term=term,
                callstack=callstack
            )
        )
        return var(internal_name)

    def _lookup_variable(
            self,
            name: str,
            line: Optional[int] = None,
            col: Optional[int] = None,
            callstack: Optional[list[Fid]] = None,
            module: Optional[str] = None) -> (bool, Optional[Term]):
        if line is None or col is None or callstack == []:
            vs = [v for v in self.variable_table if v.callstack == [] and v.name == name]
            return (False, None) if vs == [] else (True, vs[0].term)
        else:
            vs = [v for v in self.variable_table if v.line == line and v.col == col and v.name == name]
            return (False, None) if vs == [] else (True, vs[0].term)

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


    def fresh(self) -> Term:
        vid = self.variable_counter
        self.variable_counter += 1
        internal_name = f'_fresh_{vid}'
        term = var(internal_name)
        return term

    def fresh_n(self, n: int) -> list[Term]:
        fresh_vars = []
        for i in range(n):
            fresh_vars.append(self.fresh())
        return fresh_vars

    def add_rule(self, body: Term, head: str):
        rid = Rid(self.rule_counter)
        self.rule_counter += 1
        self.rules.append(
            Rule(body=body, rid=rid, head=head)
        )

    def type_check(self):
        for ast in self.asts:
            self.check_node(ast, atom('true'), '')

    def get_name(self, node, toplevel: bool) -> str:
        [ann, ident] = node['contents']
        if toplevel or ann['scope']['type'] == 'GlobalSymbol':
            return ident

        elif ann['scope']['type'] == 'ValueBinder':
            line = ann['loc']['from']['line']
            col = ann['loc']['from']['col']
            return f"{ident}_{line}_{col}"

        elif ann['scope']['type'] == 'LocalValue':
            line = ann['scope']['loc']['line']
            col = ann['scope']['loc']['col']
            return f"{ident}_{line}_{col}"

        else:
            print(node)
            raise NotImplementedError

    def check_node(self, node, term: Term, head: str, toplevel: bool=False):
        match node:
            case {'tag': 'Module', 'contents': [ann, _, _, _, decls]}:
                for decl in decls: self.check_node(decl, term, head, toplevel=True)

            case {'tag': 'PatBind', 'contents': [ann, pat, rhs, _]}:
                match pat:
                    case {'tag': 'PVar', 'contents': [ann, name]}:
                        var_name = self.get_name(name, toplevel)
                        self.check_node(rhs, T, var_name)
                    case _:
                        raise NotImplementedError(f"PatBind with {pat.get('tag')} is not supported")

            case {'tag': 'FunBind', 'contents': [ann, matches]}:
                [_, fname, fargs, _, _] = matches[0]['contents']
                fun_name = self.get_name(fname, toplevel)
                # callstack = [*callstack, fun_name]
                var_args = self.fresh_n(len(fargs))
                var_rhs = self.fresh()
                # self.make_function(fid=Fid(fun_name), function_var=var_fun, arg_vars=var_args, return_var=var_rhs)
                #
                for match in matches:
                    [ann, _, args, rhs, wheres] = match['contents']
                    for arg, var_arg in zip(args, var_args):
                        self.check_node(arg, var_arg, head=fun_name)
                    self.check_node(rhs, var_rhs, head=fun_name)

            case {'tag': 'TypeSig', 'contents': [ann, names, sig]}:
                pass
                # sig_var = self.fresh()
                # self.check_node(sig, sig_var, callstack)
                # for name in names:
                #     name_var = self.get_name(name, toplevel)
                #     name: str = name_var.value
                #     self.add_rule(name_var == sig_var)

            case {'tag': 'UnGuardedRhs', 'contents': [ann, exp]}:
                self.check_node(exp, term, head)

            # Exp types:
            case {'tag': 'Lit', 'contents': [ann, lit]}:
                self.check_node(lit, term, head)

            case {'tag': 'Con', 'contents': [ann, qname]}:
                if qname['tag'] == 'UnQual' and qname['contents'][1]['contents'][1] in ['True', 'False']:
                    self.add_rule(term == t_bool, head=head)
                else:
                    raise NotImplementedError()

            case {'tag': 'Var', 'contents': [ann, qname]}:
                pass
                # if qname['tag'] == 'UnQual' and qname['contents'][1]['contents'][1] == 'undefined':
                #     self.add_rule(atom('true'))
                # else:
                #     name_var: term = self.fresh([])
                #     self.add_rule(name_var == term)
                #     self.check_node(qname, name_var, callstack)

            case {'tag': 'App', 'contents': [ann, exp1, exp2]}:
                pass
                # var1 = self.fresh(callstack)
                # var2 = self.fresh(callstack)
                # self.check_node(exp2, var2, callstack)
                # self.add_rule(var1 == fun_of(var2, term))
                # name_var = self.fresh(callstack)
                # self.check_node(exp1, name_var, callstack)

            case {'tag': "If", 'contents': [ann, cond, leftbranch, rightbranch]}:
                pass
                # var_cond = self.fresh(callstack)
                # self.check_node(cond, var_cond, callstack, )
                # self.add_rule(var_cond == t_bool)
                # self.check_node(leftbranch, term, callstack)
                # self.check_node(rightbranch, term, callstack)

            # Lit nodes:
            case {'tag': 'Char', 'contents': [ann, _, _]}:
                self.add_rule(term == t_char, head=head)

            case {'tag': 'String', 'contents': [ann, _, _]}:
                self.add_rule(term == list_of(t_char), head=head)

            case {'tag': 'Int', 'contents': [ann, _, _]}:
                self.add_rule(term == t_int, head=head)

            case {'tag': 'Frac', 'contents': [ann, _, _]}:
                self.add_rule(term == t_float, head=head)

            # Types
            case {'tag': 'TyCon', 'contents': [ann, qname]}:
                if qname['tag'] == 'UnQual':
                    type_literal = qname['contents'][1]['contents'][1]
                    match type_literal:
                        case "Int":
                            self.add_rule(term == t_int, head=head)
                        case "Char":
                            self.add_rule(term == t_char, head=head)
                        case "String":
                            self.add_rule(term == list_of(t_char), head=head)
                        case "Float":
                            self.add_rule(term == t_float, head=head)
                        case "Bool":
                            self.add_rule(term == t_bool, head=head)
                        case _:
                            raise NotImplementedError
                else:
                    raise NotImplementedError

            case {'tag': 'TyFun', 'contents': [ann, t1, t2]}:
                var1 = self.fresh()
                var2 = self.fresh()
                var_fun = self.fresh()
                self.check_node(t1, var1, head)
                self.check_node(t2, var2, head)

                self.add_rule(var_fun == fun_of(var1, var2))

            # Patterns
            case {'tag': 'PVar', 'contents': [ann, name]}:
                var = self.get_name(name, toplevel)
                self.add_rule(var == term)

            case {'tag': 'PLit', 'contents': [ann, _, lit]}:
                self.check_node(lit, term, head)

            case {'tag': 'PApp', 'contents': [ann, pname, pargs]}:
                if pname['tag'] == 'UnQual' and pname['contents'][1]['contents'][1] in ['True', 'False']:
                    self.add_rule(term == t_bool)

            # Namings
            case {'tag': 'UnQual', 'contents': [ann, name]}:
                var = self.get_name(name, toplevel)
                self.add_rule(var == term)

            case {'tag': 'Special', 'contents': [ann, specialcon]}:
                match specialcon['tag']:
                    case "UnitCon":
                        self.add_rule(term == t_unit)
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
                debug(node)
                raise NotImplementedError


if __name__ == "__main__":
    system = System(code_dir=str(Path(__file__).parent.parent / "example"))
    system.type_check()
    for r in system.rules:
        print(r)