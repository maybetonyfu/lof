from subprocess import run
import json
from typing import Any, TypeAlias
from src.prolog import Prolog, Term, atom, var, struct, Clause, PlInterface
from devtools import debug
from src.marco import Marco, RuleSet, TCError
from pydantic import BaseModel
from enum import Enum
from pathlib import Path

Point: TypeAlias = tuple[int, int]
Span: TypeAlias = tuple[Point, Point]

t_unit = atom('unit')
t_bool = atom('bool')
t_char = atom('char')
t_int = atom('int')
t_float = atom('float')
T = var('T')

no_loc: Span = ((-1, -1), (-1, -1))


def type_from(value: str | dict):
    match value:
        case '_':
            return 'any'
        case 'int':
            return 'Int'
        case 'char':
            return 'Char'
        case 'float':
            return 'Float'
        case 'bool':
            return 'Bool'
        case 'unit':
            return '()'
        case {'functor': 'list', 'args': [x]}:
            x_type = type_from(x)
            return f'[{x_type}]'

        case {'functor': 'function', 'args': args}:
            args_type = [type_from(x) for x in args]
            return ' -> '.join(args_type)
        case _:
            if value[0].isupper():
                return value[0].lower() + value[1:]
            else:
                raise NotImplementedError(value)


def list_of(elem: Term):
    return struct('list', elem)


def fun_of(*terms: Term):
    match len(terms):
        case 0:
            raise ValueError("fun_of needs at least one argument")
        case 1:
            return terms[0]
        case _:
            return struct('function', terms[0], fun_of(*terms[1:]))


class RuleType(Enum):
    Def = "Def"
    Type = "Type"
    App = "App"
    Lit = "Lit"
    Var = "Var"
    Ambient = "Ambient"


class Rule(BaseModel):
    """ A rule is one constraint associated with a location"""
    rid: int
    body: Term
    head: str
    loc: Span
    ambient: bool
    watch: list[Term]
    type: RuleType
    src_text: str | None


class Slice(BaseModel):
    slice_id: int
    loc: Span
    appears: list[int]


class TError(BaseModel):
    error_id: int
    mus_list: list[RuleSet]
    mcs_list: list[RuleSet]
    slices: list[Slice]


class TypeSig(BaseModel):
    var: str
    type: str


def get_location(ann: dict[str, Any]) -> Span:
    if ann.get('loc', False):
        from_point = ann.get('loc', {'from': False}).get('from', False)
        to_point = ann.get('loc', {'to': False}).get('to', False)
        if from_point is False or to_point is False:
            raise ValueError("Tried to extract SrcSpan from an invalid location")
        else:
            return (from_point['line'], from_point['col']), (to_point['line'], to_point['col'])
    else:
        return no_loc


class System:
    def __init__(self, code_dir, prolog):
        self.code_dir = code_dir
        self.prolog: Prolog = prolog
        self.project_dir = Path(__file__).parent.parent
        self.parser_bin = str(self.project_dir / "bin" / "haskell-tool-exe.exe")
        result = run([self.parser_bin, self.code_dir], shell=True, check=True, capture_output=True)
        self.parsed_data = json.loads(result.stdout)
        self.asts = [c['ast'] for c in self.parsed_data['contents']]
        self.files = [c['file'] for c in self.parsed_data['contents']]
        self.file_content = ''
        self.variable_counter: int = 0
        self.variables: set[str] = set()
        self.rule_counter: int = 0
        self.free_vars: dict[str, list[Term]] = {}
        self.rules: list[Rule] = []
        self.tc_errors: list[TCError] = []

    def reset(self):
        self.__init__(self.code_dir, self.prolog)

    def fresh(self) -> Term:
        vid = self.variable_counter
        self.variable_counter += 1
        internal_name = f'FreshU{vid}'
        term = var(internal_name)
        return term

    def bind(self, h: str) -> Term:
        vid = self.variable_counter
        self.variable_counter += 1
        internal_name = f'FreshB{vid}'
        term = var(internal_name)
        if old := self.free_vars.get(h, False):
            self.free_vars[h] = old + [term]
        else:
            self.free_vars[h] = [term]
        return term

    def bind_n(self, n: int, h: str) -> list[Term]:
        fresh_vars = []
        for i in range(n):
            fresh_vars.append(self.bind(h))
        return fresh_vars

    def _add_rule(self, body: Term, head: str, ambient: bool, ann: dict, watch: list[Term], rule_type: RuleType):
        rid = self.rule_counter
        self.rule_counter += 1
        loc: Span = get_location(ann)
        from_line = loc[0][0] - 1
        from_col = loc[0][1] - 1
        to_col = loc[1][1] - 1
        if ambient:
            src_text = None
        else:
            src_text = self.file_content.splitlines()[from_line][from_col:to_col]
        self.rules.append(
            Rule(
                body=body,
                rid=rid,
                head=head,
                ambient=ambient,
                loc=get_location(ann),
                watch=watch,
                type=rule_type,
                src_text=src_text
            )
        )

    def add_rule(self, body: Term, head: str, ann: dict, watch: list[Term], rule_type: RuleType):
        self._add_rule(body, head, False, ann, watch, rule_type)

    def add_ambient_rule(self, body: Term, head: str):
        self._add_rule(body, head, True, {}, [], RuleType.Ambient)

    def solve(self, rules: set[int]) -> bool | list:
        self.prolog.set_queries([])
        self.prolog.set_clauses([])
        clause_map = {v: [] for v in self.variables}
        active_rules = [r for r in self.rules if r.rid in rules or r.ambient]
        for r in active_rules:
            clause_map[r.head] = clause_map.get(r.head) + [r.body]

        for head, body in clause_map.items():
            frees = Term.array(*self.free_vars.get(head, []))
            self.prolog.add_clause(Clause(head=struct('type_of_' + head, T, frees), body=body))

        for h in self.variables:
            var_term = var('_' + h)
            frees = Term.array(*self.free_vars.get(h, []))
            self.prolog.add_query(struct('type_of_' + h, var_term, frees))

        return self.prolog.run()

    def solve_bool(self, rules: set[int]) -> bool:
        return self.solve(rules) != False

    def type_check(self) -> list[TCError]:
        self.reset()
        for ast, file in zip(self.asts, self.files):
            file_content = (Path(self.code_dir) / file).read_text()
            self.file_content = file_content
            self.check_node(ast, atom('true'), '')
        r = self.solve({r.rid for r in self.rules if not r.ambient})
        if r:
            return []
        else:
            marco = Marco(rules={r.rid for r in self.rules if not r.ambient}, sat_fun=self.solve_bool)
            marco.run()
            marco.analyse()
            self.tc_errors = marco.tc_errors
            return marco.tc_errors

    def infer_type(self, error_id: int, mss_id: int) -> list[TypeSig]:
        tc_error = self.tc_errors[error_id]
        mss = [mss for mss in tc_error.mss_list if mss.setId == mss_id][0]
        result = self.solve({r.rid for r in self.rules if r.rid in mss.rules})

        types = [TypeSig(var=key, type=type_from(value)) for key, value in result[0].items()]
        return types

    def get_name(self, node, toplevel: bool) -> str:
        match node:
            case {'tag': 'Ident', 'contents': [ann, ident]}:
                print(node)
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
            case {'tag': 'UnQual', 'contents': [ann, name]}:
                return self.get_name(name, toplevel)

    def check_node(self, node, term: Term, head: str, toplevel: bool = False):
        match node:
            case {'tag': 'Module', 'contents': [ann, _, _, _, decls]}:
                for decl in decls: self.check_node(decl, term, head, toplevel=True)

            case {'tag': 'PatBind', 'contents': [ann, pat, rhs, _]}:
                match pat:
                    case {'tag': 'PVar', 'contents': [ann, name]}:
                        var_name = self.get_name(name, toplevel)
                        var_pat = self.bind(var_name)
                        self.variables.add(var_name)
                        self.add_rule(T == var_pat, var_name, ann, [var_pat],
                                      RuleType.Def)
                        self.check_node(rhs, var_pat, var_name)
                    case _:
                        raise NotImplementedError(f"PatBind with {pat.get('tag')} is not supported")

            case {'tag': 'FunBind', 'contents': [ann, matches]}:
                [_, fname, fargs, _, _] = matches[0]['contents']
                fun_name = self.get_name(fname, toplevel)
                self.variables.add(fun_name)
                var_args = self.bind_n(len(fargs), fun_name)
                var_rhs = self.bind(fun_name)
                var_fun = self.bind(fun_name)
                self.add_rule(T == var_fun, fun_name, matches[0]['contents'][1]['contents'][0], [var_fun], RuleType.Def)
                self.add_ambient_rule(var_fun == fun_of(*var_args, var_rhs), fun_name)
                for match in matches:
                    [ann, _, args, rhs, wheres] = match['contents']
                    for arg, var_arg in zip(args, var_args):
                        self.check_node(arg, var_arg, head=fun_name)
                    self.check_node(rhs, var_rhs, head=fun_name)

            case {'tag': 'TypeSig', 'contents': [ann, names, sig]}:
                for name in names:
                    fun_name = self.get_name(name, toplevel)
                    self.variables.add(fun_name)
                    fun_var = self.bind(fun_name)
                    self.add_ambient_rule(fun_var == T, fun_name)
                    self.check_node(sig, fun_var, head=fun_name)

            case {'tag': 'UnGuardedRhs', 'contents': [ann, exp]}:
                self.check_node(exp, term, head)

            # Exp types:
            case {'tag': 'Lit', 'contents': [ann, lit]}:
                term_var = self.bind(head)
                self.add_ambient_rule(term == term_var, head)
                self.check_node(lit, term_var, head)

            case {'tag': 'Con', 'contents': [ann, qname]}:
                if qname['tag'] == 'UnQual' and qname['contents'][1]['contents'][1] in ['True', 'False']:
                    self.add_rule(term == t_bool, head, ann, watch=[term], rule_type=RuleType.Lit)
                else:
                    raise NotImplementedError()

            case {'tag': 'Var', 'contents': [ann, qname]}:
                var_name = self.get_name(qname, toplevel)
                if var_name == 'undefined':
                    pass
                else:
                    var_term = var('_' + var_name)
                    fresh_var = self.fresh()
                    self.variables.add(var_name)
                    new_var = self.bind(head)
                    self.add_ambient_rule(term == new_var, head)
                    self.add_rule(new_var == var_term, head, ann, watch=[new_var], rule_type=RuleType.Var)
                    self.add_ambient_rule(struct('type_of_' + var_name, var_term, fresh_var), head)

            case {'tag': 'App', 'contents': [ann, exp1, exp2]}:
                [var1, var2, var_result] = self.bind_n(3, head)
                self.add_ambient_rule(term == var_result, head)
                self.add_rule(var1 == fun_of(var2, term), head, ann, watch=[var_result], rule_type=RuleType.App)
                self.check_node(exp1, var1, head)
                self.check_node(exp2, var2, head)

            case {'tag': "If", 'contents': [ann, cond, leftbranch, rightbranch]}:
                var_cond = self.bind(head)
                var_proxy = self.bind(head)
                self.add_ambient_rule(var_proxy == t_bool, head)
                self.add_rule(var_cond == t_bool, head, cond['contents'][0], watch=[var_proxy], rule_type=RuleType.Var)
                self.check_node(cond, var_cond, head)
                self.check_node(leftbranch, term, head)
                self.check_node(rightbranch, term, head)

            case {'tag': 'Case', 'contents': [ann, exp, alts]}:
                matched_var = self.bind(head)
                rhs_var = self.bind(head)
                self.check_node(exp, matched_var, head)
                for alt in alts:
                    [alt_ann, pat, rhs, binds] = alt
                    self.check_node(pat, matched_var, head)
                    self.check_node(rhs, rhs_var, head)

            # Lit nodes:
            case {'tag': 'Char', 'contents': [ann, _, _]}:
                self.add_rule(term == t_char, head, ann, watch=[term], rule_type=RuleType.Lit)

            case {'tag': 'String', 'contents': [ann, _, _]}:
                self.add_rule(term == list_of(t_char), head, ann, watch=[term], rule_type=RuleType.Lit)

            case {'tag': 'Int', 'contents': [ann, _, _]}:
                self.add_rule(term == t_int, head, ann, watch=[term], rule_type=RuleType.Lit)

            case {'tag': 'Frac', 'contents': [ann, _, _]}:
                self.add_rule(term == t_float, head, ann, watch=[term], rule_type=RuleType.Lit)

            # Types
            case {'tag': 'TyCon', 'contents': [ann, qname]}:
                if qname['tag'] == 'UnQual':
                    type_literal = qname['contents'][1]['contents'][1]
                    match type_literal:
                        case "Int":
                            self.add_rule(term == t_int, head, ann, watch=[term], rule_type=RuleType.Type)

                        case "Char":
                            self.add_rule(term == t_char, head, ann, watch=[term], rule_type=RuleType.Type)

                        case "String":
                            self.add_rule(term == list_of(t_char), head, ann, watch=[term], rule_type=RuleType.Type)

                        case "Float":
                            self.add_rule(term == t_float, head, ann, watch=[term], rule_type=RuleType.Type)

                        case "Bool":
                            self.add_rule(term == t_bool, head, ann, watch=[term], rule_type=RuleType.Type)

                        case _:
                            raise NotImplementedError
                else:
                    raise NotImplementedError

            case {'tag': 'TyFun', 'contents': [ann, t1, t2]}:
                [var1, var2] = self.bind_n(2, head)
                self.check_node(t1, var1, head)
                self.check_node(t2, var2, head)
                self.add_rule(term == fun_of(var1, var2), head, ann, watch=[term], rule_type=RuleType.Type)

            # Patterns
            case {'tag': 'PVar', 'contents': [ann, name]}:
                p_name = self.get_name(name, toplevel)
                p_var = var('_' + p_name)
                self.add_ambient_rule(p_var == term, head)
                self.variables.add(p_name)

            case {'tag': 'PLit', 'contents': [ann, _, lit]}:
                self.check_node(lit, term, head)

            case {'tag': 'PApp', 'contents': [ann, pname, pargs]}:
                if pname['tag'] == 'UnQual' and pname['contents'][1]['contents'][1] in ['True', 'False']:
                    self.add_rule(term == t_bool, head, ann, watch=[term], rule_type=RuleType.Lit)

            case {'tag': 'Special', 'contents': [ann, specialcon]}:
                match specialcon['tag']:
                    case "UnitCon":
                        self.add_rule(term == t_unit, head, ann, watch=[term], rule_type=RuleType.Lit)
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
    with Prolog(interface=PlInterface.File, file="teach.pl") as prolog:
        system = System(
            code_dir=str(Path(__file__).parent.parent / "example"),
            prolog=prolog
        )
        system.type_check()
        for r in system.rules:
            print(r)
