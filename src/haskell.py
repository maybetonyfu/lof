from enum import Enum
from subprocess import run
import json
from typing import Any, TypeAlias, Iterator
from src.prolog import Prolog, Term, atom, var, struct, Clause, PlInterface
# from devtools import debug
from src.maybe import Maybe, nothing, just
from src.marco import Marco, Error
from pydantic import BaseModel
from pathlib import Path

Point: TypeAlias = tuple[int, int]
Span: TypeAlias = tuple[Point, Point]

t_unit = atom('unit')
t_bool = atom('bool')
t_char = atom('char')
t_int = atom('int')
t_float = atom('float')
T = var('T')


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
            arg0 = args[0]
            arg0_type = ('(' + type_from(arg0) + ')') if type(arg0) == dict and arg0[
                'functor'] == 'function' else type_from(arg0)
            arg_rest = args[1:]
            args_type = [arg0_type] + [type_from(x) for x in arg_rest]
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
    Decl = 'Decl'
    Var = 'Var'
    App = 'App'
    Lit = 'Lit'
    Type = 'Type'
    Ambient = 'Ambient'


class RuleMeta(BaseModel):
    var_string: Maybe[str]
    watch: Maybe[Term]
    type: RuleType
    head: str
    loc: Span
    src_text: Maybe[str]


class Rule(BaseModel):
    """ A rule is one constraint associated with an id and a meta object"""
    rid: int
    body: Term
    meta: RuleMeta

    def is_ambient(self) -> bool:
        return self.meta.type == RuleType.Ambient or self.meta.type == RuleType.Decl


class Slice(BaseModel):
    slice_id: int
    loc: Span
    appears: list[int]


class TypeSig(BaseModel):
    var: str
    type: str


class FixType(Enum):
    Type = 'Type'
    Term = 'Term'


class Fix(BaseModel):
    fix_type: FixType
    original_text: str
    inferred_type: str
    is_mismatch_decl: bool
    mismatch_decl: str | None
    mismatch_usage_type: str | None
    mismatch_usage_loc: Span | None


class Decl(BaseModel):
    name: str
    type: str | None
    loc: Span


class Cause(BaseModel):
    suggestions: list[Fix]
    decls: list[Decl]
    locs: list[Span]


class Diagnosis(BaseModel):
    causes: list[Cause]
    decls: list[Decl]
    locs: list[Span]


def get_location(ann: dict[str, Any]) -> tuple[str, Span]:
    if ann.get('loc', False):
        from_point: dict[str, int] = ann.get('loc', {'from': False}).get('from', False)
        to_point: dict[str, int] = ann.get('loc', {'to': False}).get('to', False)
        file: str = ann.get('loc', {'file': False}).get('file', False)
        if from_point is False or to_point is False or file is False:
            raise ValueError("Tried to extract SrcSpan from an invalid location")
        else:
            return file, ((from_point['line'], from_point['col']), (to_point['line'], to_point['col']))
    else:
        raise ValueError('Invalid annotation: ', ann)


class System:
    project_dir = Path(__file__).parent.parent
    parser_bin = str(project_dir / "bin" / "haskell-parser.exe")

    def __init__(self, code_dir, prolog):
        self.code_dir = code_dir
        self.prolog: Prolog = prolog
        self.prelude = ['length', 'map', 'id', 'head', 'tail']
        self.file_content: dict[str, str] = {'': ''}
        self.variable_counter: int = 0
        self.variables: set[str] = set()  # Variables that are heads in clauses
        self.free_vars: dict[str, list[Term]] = {}  # (head, Intermediate variables)
        self.rules: list[Rule] = []
        self.tc_errors: list[Error] = []

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

    def _add_rule(self, body: Term, head: str, ann: Maybe[dict], watch: Maybe[Term], rule_type: RuleType,
                  var_string: Maybe[str]):
        rid = len(self.rules)
        if ann.is_just:
            file, loc = get_location(ann.value)
            from_line = loc[0][0] - 1
            from_col = loc[0][1] - 1
            to_col = loc[1][1] - 1
            file_content: str = self.file_content[file]
            src_text = just(file_content.splitlines()[from_line][from_col:to_col])
        else:
            src_text = nothing
            loc = ((-1, -1), (-1, -1))

        self.rules.append(
            Rule(
                body=body,
                rid=rid,
                meta=RuleMeta(
                    watch=watch,
                    head=head,
                    loc=loc,
                    var_string=var_string,
                    src_text=src_text,
                    type=rule_type
                )
            )
        )

    def add_rule(self, body: Term, head: str, ann: dict, watch: Term, rule_type: RuleType,
                 var_string: Maybe[str] = nothing):
        self._add_rule(body, head, just(ann), just(watch), rule_type, var_string)

    def add_ambient_rule(self, body: Term, head: str):
        self._add_rule(body, head, nothing, nothing, RuleType.Ambient, nothing)

    def solve(self, rules: set[int]) -> bool | list:
        self.prolog.set_queries([])
        self.prolog.set_clauses([])
        clause_map = {v: [] for v in self.variables}
        active_rules = [r for r in self.rules if r.rid in rules or r.is_ambient()]
        for r in active_rules:
            clause_map[r.meta.head] = clause_map.get(r.meta.head) + [r.body]

        for head, body in clause_map.items():
            frees = Term.array(*self.free_vars.get(head, []))
            self.prolog.add_clause(Clause(head=struct('type_of_' + head, T, frees), body=body))

        for h in self.variables:
            var_term = var('_' + h)
            frees = Term.array(*self.free_vars.get(h, []))
            self.prolog.add_query(struct('type_of_' + h, var_term, frees))

        return self.prolog.run()

    def solve_bool(self, rules: set[int]) -> bool:
        return self.solve(rules) is not False

    def diagnose(self) -> Iterator[Diagnosis]:
        for error in self.tc_errors:
            all_rule_ids = set().union(*[mus.rules for mus in error.mus_list])
            all_rules = [self.rules[rid] for rid in all_rule_ids]
            all_decl_names = {r.meta.head for r in all_rules}
            all_decls = [Decl(
                name=decl_name,
                loc=[rule.meta.loc for rule in self.rules if
                     rule.meta.head == decl_name and rule.meta.type == RuleType.Decl][0],
                type=None) for decl_name in all_decl_names]

            causes = []
            for mcs in error.mcs_list:
                types: dict[str, str] = self.infer_type(error.error_id, mcs.setId)
                mcs_rules = [self.rules[rid] for rid in mcs.rules]
                suggestions = []
                usages: dict[str, Rule] = {r.meta.var_string.value: r for r in all_rules
                                           if r.meta.type == RuleType.Var
                                           and r.meta.var_string.is_just
                                           and r.meta.var_string.value in all_decl_names
                                           }

                for rule in mcs_rules:

                    is_mismatch_decl = rule.meta.head in usages.keys()
                    if is_mismatch_decl:
                        usage_rule = usages[rule.meta.head]
                        mismatch_usage_loc = usage_rule.meta.loc
                        mismatch_usage_type = types[usage_rule.meta.watch.value.value]
                    else:
                        mismatch_usage_loc = None
                        mismatch_usage_type = None

                    suggestions.append(Fix(
                        fix_type=FixType.Type if rule.meta.type == RuleType.Type else FixType.Term,
                        inferred_type=types[rule.meta.watch.value.value],
                        original_text=rule.meta.src_text.value,
                        is_mismatch_decl=is_mismatch_decl,
                        mismatch_decl=rule.meta.head if is_mismatch_decl else None,
                        mismatch_usage_loc=mismatch_usage_loc if is_mismatch_decl else None,
                        mismatch_usage_type=mismatch_usage_type if is_mismatch_decl else None,
                    ))

                cause = Cause(
                    decls=list(map(lambda decl: Decl(name=decl.name, loc=decl.loc, type=types[decl.name]), all_decls)),
                    suggestions=suggestions,
                    locs=[r.meta.loc for r in mcs_rules]
                )
                causes.append(cause)

            diagnosis = Diagnosis(decls=all_decls, causes=causes, locs=[r.meta.loc for r in all_rules])
            yield diagnosis

        # diagnosis = []

        # for error in self.tc_errors:
        #     decls: dict[str, dict[str, Any]] = {}
        #     usages: dict[str, tuple[Span, str]] = {}
        #     typing = {}
        #     locs = {}
        #     has_decl = {}
        #     fix_combos = {}
        #     for mcs in error.mcs_list:
        #         types: list[TypeSig] = self.infer_type(error.error_id, mcs.setId)
        #         typing[mcs.setId] = types
        #
        #     for mcs in error.mcs_list:
        #         types = typing[mcs.setId]
        #         for rule_id in mcs.rules:
        #             rule = self.rules[rule_id]
        #             loc = rule.meta.loc
        #             if rule.type.rule_type == 'Decl':
        #                 if decls.get(rule.head, False):
        #                     decls[rule.head] = {'type': 'decl-usage', 'locs': [*decls[rule.head]['locs'], loc],
        #                                         'scope': [*decls[rule.head]['scope'], error.error_id]}
        #                 else:
        #                     decls[rule.head] = {'type': 'decl-usage', 'locs': [loc], 'scope': [error.error_id]}
        #
        #             elif rule.type.rule_type == 'Var':
        #                 watch: str = rule.watch[0].value
        #                 type = [type.type for type in types if type.var == watch][0]
        #                 usages[rule.type.var_term.value[1:]] = (loc, type)
        #
        #     for decl in decls:
        #         if not usages.get(decl, False):
        #             decls[decl]['type'] = 'decl-decl'
        #
        #     for mcs in error.mcs_list:
        #         fixes: list[Fix] = []
        #         types = typing[mcs.setId]
        #         cause_by_locs = []
        #         has_decl[mcs.setId] = False
        #         for rule_id in mcs.rules:
        #             rule = self.rules[rule_id]
        #             if rule.type.rule_type == "Decl":
        #                 has_decl[mcs.setId] = True
        #             cause_by_locs.append(rule.loc)
        #             text = rule.src_text
        #             fix_type = 'Type' if rule.type.rule_type == 'Type' else 'Term'
        #             is_mismatch_decl = rule.head in decls.keys()
        #             mismatch_usage_type = None
        #             mismatch_usage_loc = None
        #             if is_mismatch_decl:
        #                 if decls[rule.head]['type'] == 'decl-usage':
        #                     mismatch_usage_loc = usages[rule.head][0]
        #                     mismatch_usage_type = usages[rule.head][1]
        #
        #             fixes.append(Fix(
        #                 fix_type=fix_type,
        #                 original_text=text,
        #                 inferred_type=[t.type for t in types if t.var == rule.watch[0].value][0],
        #                 is_mismatch_decl=is_mismatch_decl,
        #                 mismatch_decl=rule.head,
        #                 mismatch_usage_type=mismatch_usage_type,
        #                 mismatch_usage_loc=mismatch_usage_loc
        #             ))
        #         fix_combos[mcs.setId] = fixes
        #         locs[mcs.setId] = cause_by_locs
        #
        #     causes = []
        #     # print(decls)
        #     for key in typing.keys():
        #         # print(with_types)
        #         if not has_decl[key]:
        #             with_types: list[tuple[str, list[Span], str]] = [
        #                 (decl,
        #                  meta['locs'],
        #                  [t for t in typing[key] if t.var == '_' + decl][0].type) for decl, meta
        #                 in decls.items()]
        #
        #             causes.append(Cause(
        #                 typing=typing[key],
        #                 decls=with_types,
        #                 suggestions=fix_combos[key],
        #                 locs=locs[key]
        #             ))
        #
        #     all_rules = set()
        #     for mus in error.mus_list:
        #         for rid in mus.rules:
        #             all_rules.add(rid)
        #     diagnosis.append(
        #         Diagnosis(causes=causes,
        #                   all_locs=[self.rules[rid].loc for rid in all_rules if
        #                             self.rules[rid].type.rule_type != 'Decl']))

    def type_check(self) -> list[Diagnosis]:
        self.reset()

        result = run([System.parser_bin, self.code_dir], shell=True, check=True, capture_output=True)
        parsed_data = json.loads(result.stdout)
        asts = [c['ast'] for c in parsed_data['contents']]
        files = [c['file'] for c in parsed_data['contents']]

        for ast, file in zip(asts, files):
            self.file_content[file] = (Path(self.code_dir) / file).read_text()
            self.check_node(ast, atom('true'), '')
        r = self.solve({r.rid for r in self.rules if not r.is_ambient()})

        if r:
            return []
        else:
            marco = Marco(rules={r.rid for r in self.rules if not r.is_ambient()}, sat_fun=self.solve_bool)
            marco.run()
            marco.analyse()
            self.tc_errors = marco.tc_errors
            return list(self.diagnose())

    def infer_type(self, error_id: int, mss_id: int) -> dict[str, str]:
        tc_error = self.tc_errors[error_id]
        mss = [mss for mss in tc_error.mss_list if mss.setId == mss_id][0]
        result = self.solve({r.rid for r in self.rules if r.rid in mss.rules})
        types = {key[1:] if key.startswith('_') else key: type_from(value) for key, value in result[0].items()}
        return types

    def get_name(self, node, toplevel: bool) -> str:
        match node:
            case {'tag': 'Ident', 'contents': [ann, ident]}:
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
                        self.add_rule(T == var_pat, var_name, ann, var_pat, rule_type=RuleType.Decl)
                        self.check_node(rhs, var_pat, var_name)
                    case _:
                        raise NotImplementedError(f"PatBind with {pat.get('tag')} is not supported")

            case {'tag': 'FunBind', 'contents': [ann, matches]}:
                [ann, fname, fargs, _, _] = matches[0]['contents']
                fun_name = self.get_name(fname, toplevel)
                self.variables.add(fun_name)
                var_args = self.bind_n(len(fargs), fun_name)
                var_rhs = self.bind(fun_name)
                var_fun = self.bind(fun_name)
                self.add_rule(T == var_fun, fun_name, ann, watch=var_fun, rule_type=RuleType.Decl)
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
                    self.add_rule(fun_var == T, fun_name, name['contents'][0], watch=fun_var, rule_type=RuleType.Decl)
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
                    self.add_rule(term == t_bool, head, ann, watch=term, rule_type=RuleType.Lit)
                else:
                    raise NotImplementedError()

            case {'tag': 'Var', 'contents': [ann, qname]}:
                var_name = self.get_name(qname, toplevel)
                if var_name == 'undefined':
                    pass
                else:
                    var_term = var('_' + var_name)
                    fresh_var = self.fresh()
                    if var_name not in self.prelude:
                        self.variables.add(var_name)

                    new_var = self.bind(head)
                    self.add_ambient_rule(term == new_var, head)
                    self.add_rule(new_var == var_term, head, ann, watch=new_var,
                                  rule_type=RuleType.Var, var_string=just(var_name))

                    self.add_ambient_rule(struct('type_of_' + var_name, var_term, fresh_var), head)

            case {'tag': 'App', 'contents': [ann, exp1, exp2]}:
                [var1, var2, var_result] = self.bind_n(3, head)
                self.add_ambient_rule(term == var_result, head)
                self.add_rule(var1 == fun_of(var2, term), head, ann, watch=var_result, rule_type=RuleType.App)
                self.check_node(exp1, var1, head)
                self.check_node(exp2, var2, head)

            case {'tag': "If", 'contents': [ann, cond, leftbranch, rightbranch]}:
                var_cond = self.bind(head)
                var_proxy = self.bind(head)
                self.add_ambient_rule(var_proxy == t_bool, head)
                self.add_rule(var_cond == t_bool,
                              head,
                              cond['contents'][0],
                              watch=var_proxy,
                              rule_type=RuleType.Lit)
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

            case {'tag': 'Let', 'contents': [ann, binds, exp]}:
                decls = binds['contents'][1]
                for decl in decls:
                    self.check_node(decl, atom('false'), head)
                self.check_node(exp, term, head)

            case {'tag': 'List', 'contents': [ann, exps]}:
                elem_var = self.bind(head)
                self.add_rule(term == list_of(elem_var), head, ann, watch=term, rule_type=RuleType.Lit)

                for exp in exps:
                    self.check_node(exp, elem_var, head)

            # Lit nodes:
            case {'tag': 'Char', 'contents': [ann, _, _]}:
                self.add_rule(term == t_char, head, ann, watch=term, rule_type=RuleType.Lit)

            case {'tag': 'String', 'contents': [ann, _, _]}:
                self.add_rule(term == list_of(t_char), head, ann, watch=term, rule_type=RuleType.Lit)

            case {'tag': 'Int', 'contents': [ann, _, _]}:
                self.add_rule(term == t_int, head, ann, watch=term, rule_type=RuleType.Lit)

            case {'tag': 'Frac', 'contents': [ann, _, _]}:
                self.add_rule(term == t_float, head, ann, watch=term, rule_type=RuleType.Lit)

            # Types
            case {'tag': 'TyCon', 'contents': [ann, qname]}:
                if qname['tag'] == 'UnQual':
                    type_literal = qname['contents'][1]['contents'][1]
                    match type_literal:
                        case "Int":
                            self.add_rule(term == t_int, head, ann, watch=term, rule_type=RuleType.Type)

                        case "Char":
                            self.add_rule(term == t_char, head, ann, watch=term, rule_type=RuleType.Type)

                        case "String":
                            self.add_rule(term == list_of(t_char), head, ann, watch=term,
                                          rule_type=RuleType.Type)

                        case "Float":
                            self.add_rule(term == t_float, head, ann, watch=term, rule_type=RuleType.Type)

                        case "Bool":
                            self.add_rule(term == t_bool, head, ann, watch=term, rule_type=RuleType.Type)

                        case _:
                            raise NotImplementedError
                else:
                    raise NotImplementedError

            case {'tag': 'TyFun', 'contents': [ann, t1, t2]}:
                [var1, var2] = self.bind_n(2, head)
                self.check_node(t1, var1, head)
                self.check_node(t2, var2, head)
                self.add_rule(term == fun_of(var1, var2), head, ann, watch=term, rule_type=RuleType.Type)

            case {'tag': 'TyList', 'contents': [ann, tnode]}:
                tvar = self.bind(head)
                self.add_rule(term == list_of(tvar), head, ann, watch=term, rule_type=RuleType.Type)
                self.check_node(tnode, tvar, head)

            case {'tag': 'TyVar', 'contents': [ann, name]}:
                raise NotImplementedError("Type var is coming very soon")

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
                    self.add_rule(term == t_bool, head, ann, watch=term, rule_type=RuleType.Lit)

            case {'tag': 'Special', 'contents': [ann, special_con]}:
                match special_con['tag']:
                    case "UnitCon":
                        self.add_rule(term == t_unit, head, ann, watch=term, rule_type=RuleType.Lit)
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
                print(json.dumps(node))
                raise NotImplementedError


if __name__ == "__main__":
    with Prolog(interface=PlInterface.File, file="program.pl") as prolog:
        system = System(
            code_dir=str(Path(__file__).parent.parent / "example"),
            prolog=prolog
        )
        system.type_check()
