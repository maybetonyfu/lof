import string
from collections import defaultdict
from enum import Enum
from itertools import chain
from subprocess import run
import ujson
from typing import Any, TypeAlias, Iterator
from airium import Airium
from src.encoder import encode, decode
from string import ascii_lowercase
from src.prolog import Prolog, Term, atom, var, struct, Clause, PlInterface, cons, nil, Kind, wildcard, \
    prolog_list_to_list, struct_extern, unify
from src.maybe import Maybe, nothing, just
from src.marco import Marco, Error
from pydantic import BaseModel
from pathlib import Path
from platform import platform

Point: TypeAlias = tuple[int, int]
Span: TypeAlias = tuple[Point, Point]
Loc: TypeAlias = tuple[str, Span]


def gather_type_synonym(module_ast: dict) -> [str]:
    decls = module_ast['contents'][4]
    type_decls = [d for d in decls if d['tag'] == 'TypeDecl']
    type_synonyms = []

    def get_declhead_name(decl_head: dict) -> str:
        match decl_head:
            case {'tag': 'DHead', 'contents': [_, name]}:
                return name['contents'][1]
            case {'tag': 'DHInfix', 'contents': [_, name, _]}:
                return name['contents'][1]
            case {'tag': 'DHParen', 'contents': [_, decl_head]}:
                return get_declhead_name(decl_head)
            case {'tag': 'DHApp', 'contents': [_, decl_head, _]}:
                return get_declhead_name(decl_head)

    for td in type_decls:
        td_head = td['contents'][1]
        type_synonyms.append(get_declhead_name(td_head))
    return type_synonyms


def get_module_name(file_path: Path | str) -> str:
    if isinstance(file_path, str):
        file_path = Path(file_path)
    return '_'.join(list(file_path.parts))[0:-3]


def combine_module_ident(module_name: str, ident: str) -> str:
    return f'_hsmd_{module_name}_hsmd_{ident}'


def module_to_prolog_path(module_name: str, base_path: Path) -> Path:
    return base_path / Path(module_name.replace('_', '/')).with_suffix('.pl')


def decode_decl_name(name: str):
    parts = name.split('_')
    module = []
    loc = []
    ident = ''
    open = False
    for part in parts:
        if part == 'hsmd' and not open:
            open = True
        elif part == 'hsmd' and open:
            open = False
        elif open:
            module.append(part)
        elif not open and part in string.digits:
            loc.append(part)
        else:
            ident = part
    return decode(ident)


def adt(con: Term, args: list[Term]) -> Term:
    def unwrap(terms: list[Term]):
        if len(terms) == 0:
            return nil
        else:
            return cons(terms[0], unwrap(terms[1:]))

    return struct('adt', cons(con, unwrap(args)))


# Special Atoms
t_unit: Term = atom('unit')
t_bool: Term = adt(atom('bool'), [])
t_char: Term = atom('char')
t_int: Term = atom('int')
t_float: Term = atom('float')
missing_instance: Term = atom('missing_instance')

# Special Vars
instance_name: Term = var('InstanceName')
T: Term = var('T')


def require_class(class_name: str, instance_var: Term, utility_var: Term = wildcard) -> Term:
    return struct('require_class_' + class_name, instance_var, utility_var)


def instance_of(class_name: str, instance_var: Term, utility_var: Term = wildcard) -> Term:
    return struct('instance_of_' + class_name, instance_var, utility_var)


def type_of(variable_name: str, type_var: Term, utility_var: Term = wildcard) -> Term:
    return struct('type_of_' + variable_name, type_var, utility_var)


def require(cls: str) -> [Term]:
    return [T == struct('require', var('Classes')),
            struct('member1', atom('class_' + cls), var('Classes')),
            ]


class CallGraph:
    BOUND = 'bound'
    FREE = 'free'

    def __init__(self):
        self.graph: dict[str, set[tuple[str, str]]] = defaultdict(set)
        self.closures: dict[str, set[tuple[str, str]]] = defaultdict(set)

    def is_top_level(self, var_: str):
        for parent, children in self.closures.items():
            if var_ in [v[0] for v in children] and parent == 'module':
                return True
        return False

    def add_call(self, caller: str, callee: str, alias: str):
        self.graph[caller].add((callee, alias))

    def add_closure(self, parent: str, child: str, closure_type: str):
        self.closures[parent].add((child, closure_type))

    def var_type(self, var_: str):
        for children in self.closures.values():
            for child, v_type in children:
                if var_ == child:
                    return v_type
        return self.FREE

    def get_all_defined_names(self) -> set[str]:
        names: set[str] = set()
        for parent, children in self.closures.items():
            if parent != 'module':
                names.add(parent)
            names.update([c[0] for c in children])
        return names

    def show(self):
        print("Call Graph:")
        for caller, callees in self.graph.items():
            print(caller, '->', callees)

        print("Closures:")
        for parent, children in self.closures.items():
            print(parent, '->', children)


class Type:
    letters = ascii_lowercase

    def __init__(self, json: dict | str, name: str):
        self.index = 0
        self.degree = 0
        self.mapping: dict[str: str] = {}
        self.type_classes: dict[str, set[str]] = defaultdict(set)
        self.name = name
        self.type = self.from_json(json)

    def make_letter(self):
        letter = self.letters[self.index]
        self.index += 1
        return letter

    def from_json(self, value: dict | str):
        self.degree += 1
        match value:
            case '_':
                return self.make_letter()
            case 'nil':
                return ''
            case 'int':
                return 'Int'
            case 'char':
                return 'Char'
            case 'float':
                return 'Float'
            case 'unit':
                return '()'
            case {'functor': 'adt', 'args': [{'functor': '[|]', 'args': ['list', x]}]}:
                x_type = self.from_json(x).strip()
                return f'[{x_type}]'

            case {'functor': 'adt', 'args': [{'functor': '[|]', 'args': ['tuple', x]}]}:
                args = prolog_list_to_list(x)
                return '(' + ', '.join([self.from_json(arg) for arg in args[:-1]]) + ')'

            case {'functor': 'adt', 'args': [{'functor': '[|]', 'args': ['function', x]}]}:
                args = prolog_list_to_list(x)

                def is_function(prolog_json: dict | str):
                    return (
                            type(prolog_json) == dict and
                            prolog_json.get('functor') == 'adt' and
                            prolog_json.get('args', [{}])[0].get('args', [{}])[0] == 'function'
                    )

                arg_types = []
                for i, arg in enumerate(args[:-1]):
                    if is_function(arg) and i != len(args) - 2:
                        arg_types.append('(' + self.from_json(arg) + ')')
                    else:
                        arg_types.append(self.from_json(arg))

                return ' -> '.join(arg_types)
            case {'functor': 'adt', 'args': x}:
                adt_list = prolog_list_to_list(x[0])
                [functor, *args] = adt_list
                arg_str = [self.from_json(functor)]
                for arg in args:

                    if isinstance(arg, dict) and arg.get('functor') == 'adt' and arg.get('args', [{}])[0].get(
                            'functor') not in ['tuple', 'function', 'list']:
                        arg_str.append(f'({self.from_json(arg)})')
                    else:
                        arg_str.append(self.from_json(arg))
                return ' '.join(arg_str)

            case {'functor': 'require', 'args': [class_pl_list]}:
                class_list = prolog_list_to_list(class_pl_list)
                classes = {cls[len('class_'):] for cls in class_list[:-1]}
                classes = {c.split('_')[-1] for c in classes}
                letter = self.from_json(class_list[-1])
                self.type_classes[letter] = self.type_classes[letter].union(classes)
                return letter
            case {'functor': '[|]', 'args': args}:
                return ' '.join([self.from_json(arg) for arg in args])
            case _:
                if value[0].isupper():  # Prolog Variables
                    if self.mapping.get(value, False):
                        letter = self.mapping[value]
                    else:
                        letter = self.make_letter()
                        self.mapping[value] = letter
                    return letter
                elif value[0].islower():  # Prolog Atoms
                    return value[0].upper() + value[1:]
                else:
                    raise NotImplementedError(value)

    def __str__(self):
        if len(self.type_classes) == 0:
            return self.type
        else:
            context = []
            for letter, classes in self.type_classes.items():
                for cls in classes:
                    context.append(f'{cls} {letter}')
            return ', '.join(context) + ' => ' + self.type

    def __repr__(self):
        return self.__str__()


def list_of(elem: Term) -> Term:
    return adt(atom('list'), [elem])


def fun_of(*terms: Term):
    match len(terms):
        case 0:
            raise ValueError("fun_of needs at least one argument")
        case 1:
            return terms[0]
        case _:
            return adt(atom('function'), [terms[0], fun_of(*terms[1:])])


def tuple_of(*terms: Term):
    match len(terms):
        case 0:
            raise ValueError("tuple_of needs at least one argument")
        case 1:
            return terms[0]
        case _:
            return adt(atom('tuple'), [terms[0], tuple_of(*terms[1:])])


class RuleType(Enum):
    Decl = 'Decl'
    Var = 'Var'
    App = 'App'
    Lit = 'Lit'
    Type = 'Type'
    Ambient = 'Ambient'
    Class = "Class"
    Tuple = "Tuple"
    Pattern = "Pattern"
    TypeClass = "TypeClass"


class HeadType(Enum):
    TypeOf = "TypeOf"
    InstanceOf = "InstanceOf"
    SynonymOf = "SynonymOf"


class Head(BaseModel):
    name: str
    type: HeadType
    instance_id: None | int
    from_module: str | None
    synonym_vars: list[str]

    @classmethod
    def type_of(cls, variable_name: str):
        return cls(
            name=variable_name,
            type=HeadType.TypeOf,
            instance_id=None,
            from_module=None,
            synonym_vars=[])

    @classmethod
    def synonym_of(cls, variable_name: str, synonym_vars: list[str]):
        return cls(
            name=variable_name,
            type=HeadType.SynonymOf,
            instance_id=None,
            from_module=None,
            synonym_vars=synonym_vars)

    @classmethod
    def instance_of(cls, inst_name: str, from_module: str, instance_id: int | None = None):
        return cls(
            name=inst_name,
            type=HeadType.InstanceOf,
            instance_id=instance_id,
            from_module=from_module,
            synonym_vars=[])


class RuleMeta(BaseModel):
    var_string: Maybe[str]
    watch: Maybe[Term]
    type: RuleType
    head: Head
    loc: Loc
    src_text: Maybe[str]


class Rule(BaseModel):
    """ A rule is one constraint associated with an id and a Meta object"""
    rid: int
    body: Term
    meta: RuleMeta

    def is_ambient(self) -> bool:
        return self.meta.type == RuleType.Ambient or self.meta.type == RuleType.Decl


class Decl(BaseModel):
    name: str
    display_name: str
    type: str | None
    loc: Loc


class Suggestion(BaseModel):
    text: str
    title: str
    score: int
    fix_size: int


class Cause(BaseModel):
    suggestions: list[Suggestion]
    decls: list[Decl]
    locs: list[Loc]
    score: int
    fix_size: int


class Diagnosis(BaseModel):
    causes: list[Cause]
    decls: list[Decl]
    locs: list[Loc]


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


class TypeClass(BaseModel):
    name: str
    super_classes: list[str]
    module: str


class System:
    project_dir = Path(__file__).parent.parent
    parser_bin = str(project_dir / "bin" / "haskell-parser.exe") if platform() == 'Windows' else str(
        project_dir / "bin" / "haskell-parser")

    def __init__(self, base_dir: Path, file_id: int, hs_file: Path, module_name: str, ast, prolog_instance, synonyms: list[str]):
        self.synonyms : list[str] = synonyms
        self.ast: dict = ast
        self.base_dir: Path = base_dir
        self.file_id: int = file_id
        self.hs_file_path: Path = hs_file
        self.hs_path_rel: Path = hs_file.relative_to(base_dir)
        self.current_module_name = '_'.join(module_name.split('.'))
        self.include_prelude = True
        self.prolog: Prolog = prolog_instance
        self.imports = [] if self.hs_file_path == self.base_dir / "Prelude.hs" else [
            (self.base_dir / 'Prelude.pl').as_posix()]
        self.file_content: str | None = None
        self.variable_counter: int = 0
        self.instance_counter: int = 0
        self.lambda_counter: int = 0
        self.free_vars: dict[str, list[Term]] = defaultdict(list)  # (head, Intermediate variables).
        self.rules: list[Rule] = []
        self.tc_errors: list[Error] = []
        self.classes: list[TypeClass] = []
        self.call_graph: CallGraph = CallGraph()

    def reset(self):
        self.__init__(
            base_dir=self.base_dir,
            file_id=self.file_id,
            hs_file=self.hs_file_path,
            module_name=self.current_module_name,
            ast=self.ast,
            prolog_instance=self.prolog,
            synonyms=self.synonyms)

    def fresh(self) -> Term:
        vid = self.variable_counter
        self.variable_counter += 1
        internal_name = f'_F{vid}'
        term = var(internal_name)
        return term

    def bind(self, h: str) -> Term:
        vid = self.variable_counter
        self.variable_counter += 1
        internal_name = f'Fresh_{self.file_id}_{vid}'
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

    def lambda_name(self) -> str:
        name = f'lambda{self.lambda_counter}'
        self.lambda_counter += 1
        return combine_module_ident(self.current_module_name, name)

    def get_instance_id(self) -> int:
        self.instance_counter += 1
        return self.instance_counter

    def _add_rule(self, body: Term, head: Head, ann: Maybe[dict], watch: Maybe[Term], rule_type: RuleType,
                  var_string: Maybe[str]):
        rid = len(self.rules)
        if ann.is_just:
            file, span = get_location(ann.value)
            from_line = span[0][0] - 1
            from_col = span[0][1] - 1
            to_col = span[1][1] - 1
            loc = (file, span)
            src_text = just(self.file_content.splitlines()[from_line][from_col:to_col])
        else:
            src_text = nothing
            loc = ('Dummy.hs', ((-1, -1), (-1, -1)))

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

    def add_rule(self, body: Term, head: Head, ann: dict, watch: Term, rule_type: RuleType,
                 var_string: Maybe[str] = nothing):
        self._add_rule(body, head, just(ann), just(watch), rule_type, var_string)

    def add_ambient_rule(self, body: Term, head: Head):
        self._add_rule(body, head, nothing, nothing, RuleType.Ambient, nothing)

    def diagnose(self) -> Iterator[Diagnosis]:
        for error in list(reversed(self.tc_errors)):
            all_rule_ids = set().union(*[mus.rules for mus in error.mus_list])
            all_rules = [self.rules[rid] for rid in all_rule_ids]
            all_decl_names = {r.meta.head.name for r in all_rules if
                              r.meta.head.type == HeadType.TypeOf
                              # Maybe this is not necessary?
                              and r.meta.type == RuleType.Decl
                              }
            all_decls = [Decl(
                name=decl_name,
                display_name=decode_decl_name(decl_name),
                loc=[rule.meta.loc for rule in self.rules if
                     rule.meta.head.name == decl_name and rule.meta.type == RuleType.Decl][0],
                type=None) for decl_name in all_decl_names]

            causes = []
            for mcs in error.mcs_list:
                types: dict[str, Type] = self.infer_type(error.error_id, mcs.setId)
                mcs_rules = [self.rules[rid] for rid in mcs.rules]
                suggestions = []
                usages: dict[str, Rule] = {r.meta.var_string.value: r for r in all_rules
                                           if r.meta.type == RuleType.Var
                                           and r.meta.var_string.is_just
                                           and r.meta.var_string.value in all_decl_names
                                           }

                for rule in mcs_rules:
                    is_mismatch_decl = rule.meta.head.name in usages.keys()
                    is_type_class_missing = rule.meta.type == RuleType.TypeClass
                    is_type_change = rule.meta.type == RuleType.Type
                    a = Airium(source_minify=True)
                    score = 0
                    with a.div(klass='suggestion'):
                        if is_type_class_missing:
                            a.span(_t="Make sure the proper instance of the type class")
                            a.span(_t=rule.meta.src_text.value, klass='code type primary')
                            a.span(_t='is implemented.')
                        elif is_type_change and not is_mismatch_decl:
                            a.span(_t='Change')
                            a.span(_t=rule.meta.src_text.value, klass='code type primary')
                            a.span(_t='to')
                            a.span(_t=str(types[rule.meta.watch.value.value]), klass='code type')
                            score = types[rule.meta.watch.value.value].degree + 8
                        elif is_type_change and is_mismatch_decl:
                            a.span(_t='Change')
                            a.span(_t=rule.meta.src_text.value, klass='code type primary')
                            a.span(_t='to a different type')
                            a.span(_t=', because that the function')
                            a.span(_t=decode_decl_name(rule.meta.head.name), klass='code term')
                            a.span(_t="is used as")
                            a.span(_t=str(types[usages[rule.meta.head.name].meta.watch.value.value]), klass='code type')
                            score = types[usages[rule.meta.head.name].meta.watch.value.value].degree + 8
                        elif not is_type_change and not is_mismatch_decl:
                            a.span(_t='Change')
                            a.span(_t=rule.meta.src_text.value, klass='code term primary')
                            a.span(_t='to an instance of')
                            a.span(_t=str(types[rule.meta.watch.value.value]), klass='code type')
                            score = types[rule.meta.watch.value.value].degree
                        elif not is_type_change and is_mismatch_decl:
                            a.span(_t='Change')
                            a.span(_t=rule.meta.src_text.value, klass='code term primary')
                            a.span(_t='to a different expression')
                            a.span(_t=', because that the function')
                            a.span(_t=decode_decl_name(rule.meta.head.name), klass='code term')
                            a.span(_t="is used as")
                            a.span(_t=str(types[usages[rule.meta.head.name].meta.watch.value.value]), klass='code type')
                            score = types[usages[rule.meta.head.name].meta.watch.value.value].degree

                    suggestions.append(
                        Suggestion(
                            title=rule.meta.src_text.value,
                            text=str(a),
                            score=score,
                            fix_size=len(rule.meta.src_text.value)))

                locs = [r.meta.loc for r in mcs_rules]
                cause = Cause(
                    decls=list(
                        map(lambda decl: Decl(name=decode(decl.name), loc=decl.loc, type=str(types[decl.name]),
                                              display_name=decl.display_name),
                            all_decls)),
                    suggestions=suggestions,
                    locs=locs,
                    fix_size=sum([s.fix_size for s in suggestions]),
                    score=sum([s.score for s in suggestions])
                )
                causes.append(cause)

            diagnosis = Diagnosis(decls=all_decls,
                                  causes=sorted(causes, key=lambda c: (c.score, c.fix_size)),
                                  locs=[r.meta.loc for r in all_rules])
            yield diagnosis

    def solve_bool(self, rules: set[int]) -> bool:
        return self.solve(rules) is not False

    def generate_only(self):
        self.prolog.reset()
        self.generate_type_classes()
        self.generate_typing_clauses({r.rid for r in self.rules})
        self.generate_type_synonym_clauses({r.rid for r in self.rules})
        self.generate_instance_clauses({r.rid for r in self.rules})
        self.prolog.generate_file()
        self.generate_goals()

    def solve(self, rules: set[int]) -> bool | list:
        self.prolog.reset()
        self.generate_type_classes()
        self.generate_typing_clauses(rules)
        self.generate_type_synonym_clauses({r.rid for r in self.rules})
        self.generate_instance_clauses(rules)
        self.prolog.generate_file()
        self.generate_goals()
        return self.prolog.run_file()

    def generate_type_classes(self):
        for tc in self.classes:
            class_full_name = combine_module_ident(tc.module, tc.name)
            super_classes = tc.super_classes
            super_class_rules = [instance_of(super_class, T) for super_class in super_classes]
            self.prolog.add_multifile(f'instance_of_{class_full_name}/2')
            self.prolog.add_clause(
                Clause(head=require_class(tc.name, T),
                       body=[ instance_of(class_full_name, T)] + super_class_rules
                       )
            )
            self.prolog.add_clause(
                Clause(head=instance_of(class_full_name, T), body=require(class_full_name))
            )

    def generate_type_synonym_clauses(self, rules: set[int]):
        current_file_name = str(self.hs_file_path.relative_to(self.base_dir))
        active_rules = [r for r in self.rules if (r.rid in rules)
                        or r.is_ambient()
                        or r.meta.loc[0] != current_file_name]
        all_synonym_rules = [r for r in active_rules if r.meta.head.type == HeadType.SynonymOf]
        synonyms = {r.meta.head.name for r in all_synonym_rules}
        for synonym in synonyms:
            synonym_rules = [r for r in all_synonym_rules if r.meta.head.name == synonym]
            synonym_vars = [var(f'TypeVar_{v}' ) for v in synonym_rules[0].meta.head.synonym_vars]
            synonym_head = struct('synonym_of_' + synonym, T, Term.array(*synonym_vars))
            self.prolog.add_clause(
                Clause(head=synonym_head, body=[r.body for r in synonym_rules]))

    def generate_typing_clauses(self, rules: set[int]):
        current_file_name = str(self.hs_file_path.relative_to(self.base_dir))
        defined_names = self.call_graph.get_all_defined_names()
        clause_map: dict[str, list[Term]] = {v: [] for v in defined_names}
        active_rules = [r for r in self.rules if (r.rid in rules)
                        or r.is_ambient()
                        or r.meta.loc[0] != current_file_name]

        for r in active_rules:
            if r.meta.head.name != 'module' and r.meta.head.type == HeadType.TypeOf:
                clause_map[r.meta.head.name] = clause_map[r.meta.head.name] + [r.body]

        for head, body in clause_map.items():
            frees = Term.array(*self.free_vars.get(head, []))
            self.prolog.add_clause(Clause(head=type_of(head, T, frees), body=body))

        # Imports
        if self.hs_file_path.stem != 'Prelude':
            self.prolog.set_modules(self.imports)
        else:
            self.prolog.set_modules([i for i in self.imports if i != (self.base_dir / 'Prelude.pl').as_posix()])

    def generate_instance_clauses(self, rules: set[int]):
        active_rules = [r for r in self.rules if r.rid in rules
                        or r.is_ambient()]
        classes = {r.meta.head.name for r in active_rules if r.meta.head.type == HeadType.InstanceOf}
        for cls in classes:
            module: str = [r for r in active_rules
                           if r.meta.head.type == HeadType.InstanceOf
                           and r.meta.head.name == cls][0].meta.head.from_module
            if get_module_name(self.hs_path_rel) != module:
                self.prolog.add_use_module(module_to_prolog_path(module, self.base_dir))
            instance_ids = {r.meta.head.instance_id for r in active_rules
                            if r.meta.head.type == HeadType.InstanceOf
                            and r.meta.head.name == cls
                            and r.meta.head.instance_id is not None}

            for instance_id in instance_ids:
                per_instance_rule = [r.body for r in active_rules
                                     if r.meta.head.type == HeadType.InstanceOf
                                     and r.meta.head.name == cls
                                     and r.meta.head.instance_id == instance_id]

                clause_head = struct_extern(f'user_{module}', f'instance_of_{cls}', T, wildcard)
                self.prolog.add_clause(Clause(head=clause_head, body=per_instance_rule))

    def generate_goals(self):
        def traverse_call_stack(
                caller_: str,
                callees: set[tuple[str, str]],
                self_var: Term,
                original_caller: str,
                seen: set[str]
        ):
            if caller_ in seen:
                return
            else:
                seen.add(caller_)
                var_lookup = {}

                def replace_free_var(v: Term) -> Term:
                    for callee_, alias_ in callees:
                        if caller_ == original_caller:
                            return v
                        elif v.value == alias_:
                            if self.call_graph.var_type(callee_) == CallGraph.FREE:
                                if alias_ in var_lookup:
                                    return var_lookup[alias_]
                                else:
                                    new_var = self.fresh()
                                    var_lookup[alias_] = new_var
                                    return new_var
                            else:
                                return var('_F_' + callee_ + '_' + original_caller)
                    return wildcard

                caller_free_vars = self.free_vars[caller_] if caller_ == original_caller else [replace_free_var(v) for v
                                                                                               in
                                                                                               self.free_vars[caller_]]
                self.prolog.add_query(type_of(caller_, self_var, Term.array(*caller_free_vars)))

                for callee, alias in callees:
                    if self.call_graph.var_type(callee) == CallGraph.BOUND and caller_ == original_caller:
                        self.prolog.add_query(
                            var('_F_' + callee + '_' + original_caller) == replace_free_var(var(alias)))

                    if callee in self.call_graph.graph.keys():
                        traverse_call_stack(
                            callee,
                            self.call_graph.graph[callee],
                            replace_free_var(var(alias)),
                            original_caller,
                            seen.copy()
                        )
                    else:
                        self.prolog.add_query(
                            type_of(callee, replace_free_var(var(alias)), wildcard)
                        )

        for caller, _ in self.call_graph.closures['module']:
            traverse_call_stack(
                caller,
                self.call_graph.graph[caller],
                var('_' + caller),
                caller,
                set()
            )

    def marshal(self):
        self.file_content = self.hs_file_path.read_text()
        self.check_node(self.ast, atom('true'), Head.type_of('module'))

    def type_check(self) -> list[Diagnosis]:
        self.tc_errors = []
        prolog_result = self.solve({r.rid for r in self.rules if
                                    (not r.is_ambient()) and
                                    (self.base_dir / r.meta.loc[0] == self.hs_file_path)})

        if prolog_result:
            return []
        else:
            marco = Marco(rules={r.rid for r in self.rules if (not r.is_ambient()) and
                                 (self.base_dir / r.meta.loc[0] == self.hs_file_path)
                                 }, sat_fun=self.solve_bool)
            marco.run()
            marco.analyse()
            self.tc_errors = marco.tc_errors
            return list(self.diagnose())

    def infer_type(self, error_id: int, mcs_id: int) -> dict[str, Type]:
        tc_error = self.tc_errors[error_id]
        current_mcs = [mcs for mcs in tc_error.mcs_list if mcs.setId == mcs_id][0].rules
        other_mcses = [error.mcs_list[0].rules for error in self.tc_errors if error.error_id != error_id]
        all_mcses = current_mcs.union(*other_mcses)
        result = self.solve({r.rid for r in self.rules if r.rid not in all_mcses})
        types = {decode(key[1:]) if key.startswith('_') else decode(key): Type(value, key) for key, value in
                 result[0].items()}
        return types

    def get_module(self, node) -> str:
        # Handle some builtin names, mostly cons operator and error (bottom value):
        if node['contents'][1]['tag'] == 'Ident' and node['contents'][1]['contents'][1] == 'error':
            return 'Builtin'
        match node:
            case {'tag': 'UnQual', 'contents': [{'scope': {"type": 'LocalValue', **_args}, **_keyargs}, _]}:
                return self.current_module_name
            case {'tag': 'UnQual',
                  'contents': [{'scope': {"type": 'GlobalSymbol', "Symbol": symbol, **_args}, **_keyargs}, _]}:
                return symbol['module'].replace('.', '_')
            case _:
                raise NotImplementedError()

    def get_qname(self, node, toplevel: bool) -> str:
        match node:
            case {'tag': 'Qual', 'contents': [ann, mod_name, name]}:
                module_name = get_module_name(mod_name)
                identifier = self.get_name(name, toplevel)
                return combine_module_ident(module_name, identifier)

            case {'tag': 'UnQual', 'contents': [ann, name]}:
                module_name = self.get_module(node)
                identifier = self.get_name(name, toplevel)
                return combine_module_ident(module_name, identifier)

            case {'tag': 'Special', 'contents': [ann, name]}:
                match name['tag']:
                    case 'Cons':
                        return encode(':')
                    case _:
                        raise NotImplementedError()

    def get_name(self, node, toplevel: bool) -> str:
        match node:
            case {'tag': 'Symbol', 'contents': [ann, symbol]}:
                return encode(symbol)

            case {'tag': 'Ident', 'contents': [ann, ident]}:
                if toplevel or ann['scope']['type'] == 'GlobalSymbol':
                    return encode(ident)

                elif ann['scope']['type'] == 'ValueBinder':
                    line = ann['loc']['from']['line']
                    col = ann['loc']['from']['col']
                    return f"{encode(ident)}_{line}_{col}"

                elif ann['scope']['type'] == 'LocalValue':
                    line = ann['scope']['loc']['line']
                    col = ann['scope']['loc']['col']
                    return f"{encode(ident)}_{line}_{col}"

                elif ident == 'error':
                    return "error"

                else:
                    print(node)
                    raise NotImplementedError

    def get_context(self, node_ast) -> list[tuple[str, str, dict]]:
        def get_assertion(assertion_):
            if assertion_['tag'] == 'ParenA':
                assertion_ = assertion_['contents'][1]
            type_app = assertion_['contents'][1]
            assert (type_app['tag'] == 'TyApp')
            type_con = type_app['contents'][1]
            type_var = type_app['contents'][2]
            assert (type_con['tag'] == 'TyCon')
            assert (type_var['tag'] == 'TyVar')

            class_name: str = self.get_qname(type_con['contents'][1], True)
            var_name = type_var['contents'][1]['contents'][1]
            return class_name, var_name, assertion_['contents'][0]

        match node_ast:
            case {'tag': 'CxSingle', 'contents': [ann, assertion]}:
                return [get_assertion(assertion)]

            case {'tag': 'CxTuple', 'contents': [_, assertions]}:
                return [get_assertion(assertion) for assertion in assertions]

            case {'tag': 'CxEmpty', 'contents': [_, contexts]}:
                return []
            case _:
                print(ujson.dumps(node_ast))
                raise NotImplementedError("Not a context node")

    def get_head_name(self, node_ast, results: list[str]) -> list[str]:
        # Node is DeclHead
        match node_ast:
            case {'tag': 'DHead', 'contents': [_, head_name]}:
                head_name = self.get_name(head_name, toplevel=True)
                return [head_name, *results]
            case {'tag': 'DHApp', "contents": [_, h, tv]}:
                assert (tv['tag'] == 'UnkindedVar')
                tv_name = self.get_name(tv['contents'][1], toplevel=True)
                results = [tv_name, *results]
                return self.get_head_name(h, results)
            case _:
                raise NotImplementedError()

    def unwrap_ty_app(self, ty: dict):
        match ty:
            case {'tag': 'TyApp', 'contents': [ann, t1, t2]}:
                return [*self.unwrap_ty_app(t1), t2]
            case _:
                return [ty]

    def type_con_is_synonym(self, type_con: dict) -> Maybe[str]:
        if type_con.get('tag') == 'TyCon':
            qname = type_con['contents'][1]
            match qname:
                case {'tag': 'UnQual', 'contents': [_, {'tag': _, 'contents': [_, name]}]}:
                    if name in self.synonyms:
                        return just(name)
                    else:
                        return nothing
                case _:
                    return nothing
        elif type_con.get('tag') == 'TyApp':
            head = type_con['contents'][1]
            return self.type_con_is_synonym(head)
        else:
            return nothing

    def check_node(self, node, term: Term, head: Head, toplevel: bool = False):
        match node:
            case {'tag': 'LanguagePragma', 'contents': [ann, pragmas]}:
                for pragma in pragmas:
                    pragma_name = pragma['contents'][1]
                    match pragma_name:
                        case 'NoImplicitPrelude':
                            self.imports = [i for i in self.imports if i != (self.base_dir / 'Prelude.pl').as_posix()]
                        case _:
                            raise NotImplementedError()

            case {'tag': 'Module', 'contents': [ann, _, pragmas, imports, decls]}:
                for pragma in pragmas:
                    self.check_node(pragma, term, head, toplevel=True)
                for im in imports:
                    module_name: str = im['importModule'][1]
                    prolog_file_ = '/'.join(module_name.split('.')) + '.pl'
                    prolog_file_path = (self.base_dir / prolog_file_).as_posix()
                    term = Term(value={'functor': 'use_module', 'args': [
                        Term(value=prolog_file_path, kind=Kind.String)
                    ]}, kind=Kind.Struct)
                    self.imports.append(prolog_file_path)
                    self.add_ambient_rule(term, head)

                for decl in decls:
                    self.check_node(decl, term, head, toplevel=True)

            case {'tag': 'TypeDecl', 'contents': [ann, decl_head, type_decl_rhs]}:
                heads = self.get_head_name(decl_head, [])
                type_syn_name: str = heads[0]
                v_rhs = self.bind(type_syn_name)
                syn_head = Head.synonym_of(type_syn_name, heads[1:])
                self.check_node(type_decl_rhs, v_rhs, syn_head, toplevel=True)
                self.add_ambient_rule(
                    unify(T, v_rhs),
                    syn_head
                )

            case {'tag': 'InstDecl', 'contents': [ann, _, instRule, instDecl]}:
                def get_instance_head(ast_) -> tuple[str, str, dict | None]:
                    match ast_:
                        case {'tag': 'IHCon', 'contents': [ih_ann, qname]}:
                            class_name_ = self.get_qname(qname, True)
                            module_name = self.get_module(qname)
                            return class_name_, module_name, None
                        case {'tag': 'IHInfix', 'contents': [ih_ann, ih_type, qname]}:
                            raise NotImplementedError()
                        case {'tag': 'IHParen', 'contents': [ih_ann, ih_head]}:
                            return get_instance_head(ih_head)
                        case {'tag': 'IHApp', 'contents': [ih_ann, ih_head, ih_type]}:
                            [class_name_, module_name, _] = get_instance_head(ih_head)
                            return class_name_, module_name, ih_type

                if instRule['tag'] == 'IRule':
                    [_, _, context, ins_head] = instRule['contents']
                    super_classes: list[tuple[str, str, dict]] = [] if context is None else self.get_context(context)
                    class_name_, module_name, type_ast = get_instance_head(ins_head)
                    rule_head = Head.instance_of(class_name_, module_name, self.get_instance_id())
                    self.check_node(type_ast, T, rule_head, toplevel=True)
                    for super_class in super_classes:
                        [super_class_name, super_class_var_name, super_class_loc] = super_class
                        super_class_name = super_class_name.split('_')[-1]

                        super_class_var = var(f"TypeVar_{super_class_var_name}")
                        self.add_ambient_rule(
                            struct('nonvar', super_class_var),
                            rule_head
                        )
                        self.add_ambient_rule(
                            require_class(super_class_name, super_class_var, wildcard),
                            rule_head,
                        )

                elif instRule['tag'] == 'IParen':
                    self.check_node({'tag': 'InstDecl', 'contents': [ann, None, instRule['contents'][1], instDecl]},
                                    term, head)

            case {"tag": "ClassDecl", 'contents': [ann, context, decl_head, _, decls]}:
                [class_name, *vs_names] = self.get_head_name(decl_head, [])

                super_classes: list[tuple[str, str, dict]] = [] if context is None else self.get_context(context)
                self.classes.append(TypeClass(
                    name=class_name,
                    module=self.current_module_name,
                    super_classes=[cls[0] for cls in super_classes]))

                assert (len(vs_names) == 1)
                var_name = vs_names[0]
                for decl in decls:
                    assert (decl['tag'] == 'ClsDecl')
                    type_decl = decl['contents'][1]
                    names = type_decl['contents'][1]

                    self.check_node(type_decl, wildcard, Head.type_of('module'), toplevel)
                    for name in names:
                        name = combine_module_ident(self.current_module_name, self.get_name(name, toplevel))
                        name_var = self.bind(name)
                        self.call_graph.add_closure('module', name, CallGraph.FREE)
                        self.add_ambient_rule(var('TypeVar_' + var_name) == name_var, Head.type_of(name))
                        self.add_ambient_rule(
                            require_class(class_name, var('TypeVar_' + var_name), wildcard),
                            Head.type_of(name))

            case {'tag': "DataDecl", 'contents': [ann, _, _, head, con_decls, derived_classes]}:
                [name, *vs_names] = self.get_head_name(head, [])
                name = name[0].lower() + name[1:]
                vs = [var('TypeVar_' + v) for v in vs_names]
                adt_var = atom(name) if len(vs) == 0 else adt(atom(name), vs)

                def get_first_instance_constant(ast):
                    match ast:
                        case {'tag': 'IRule', 'contents': [_, _, _, istHead]}:
                            assert istHead['tag'] == 'IHCon'
                            return istHead['contents'][1]
                        case {'tag': 'IParen', 'contents': [_, ir]}:
                            return get_first_instance_constant(ir)

                for derivings in derived_classes:
                    [_, _, istRules] = derivings
                    for istRule in istRules:
                        qname = get_first_instance_constant(istRule)
                        class_name = self.get_name(qname, toplevel=True)
                        module_name = self.get_module(qname)
                        self.add_ambient_rule(
                            T == adt_var,
                            Head.instance_of(
                                class_name,
                                from_module=module_name,
                            ))

                for con_decl in con_decls:
                    decl = con_decl[3]
                    if decl['tag'] == 'ConDecl':
                        [ann, name, types] = decl['contents']
                        con_name = combine_module_ident(self.current_module_name, self.get_name(name, toplevel=True))
                        self.call_graph.add_closure('module', con_name, CallGraph.FREE)
                        con_var = self.bind(con_name)
                        arg_vars = self.bind_n(len(types), con_name)

                        for t_ast, t_var in zip(types, arg_vars):
                            self.check_node(t_ast, t_var, Head.type_of(con_name))
                        self.add_rule(T == con_var, Head.type_of(con_name), ann, watch=con_var, rule_type=RuleType.Decl)
                        self.add_ambient_rule(con_var == fun_of(*arg_vars, adt_var), Head.type_of(con_name))
                    else:
                        raise NotImplementedError()

            case {'tag': 'PatBind', 'contents': [ann, pat, rhs, wheres]}:
                match pat:
                    case {'tag': 'PVar', 'contents': [ann, name]}:
                        var_name = combine_module_ident(self.current_module_name, self.get_name(name, toplevel))
                        var_pat = self.bind(var_name)
                        self.call_graph.add_closure(head.name, var_name, CallGraph.FREE)
                        self.add_rule(T == var_pat, Head.type_of(var_name), ann, var_pat, rule_type=RuleType.Decl)
                        self.check_node(rhs, var_pat, Head.type_of(var_name))

                        if wheres is not None:
                            self.check_node(wheres, self.fresh(), Head.type_of(var_name))
                    case _:
                        raise NotImplementedError(f"PatBind with {pat.get('tag')} is not supported")

            case {'tag': 'FunBind', 'contents': [ann, matches]}:
                [ann, f_name, f_args, _, _] = matches[0]['contents']
                fun_name = combine_module_ident(self.current_module_name, self.get_name(f_name, toplevel))
                var_args = self.bind_n(len(f_args), fun_name)
                var_rhs = self.bind(fun_name)
                var_fun = self.bind(fun_name)
                self.call_graph.add_closure(head.name, fun_name, CallGraph.FREE)

                self.add_rule(T == var_fun, Head.type_of(fun_name), ann, watch=var_fun, rule_type=RuleType.Decl)
                self.add_ambient_rule(var_fun == fun_of(*var_args, var_rhs), Head.type_of(fun_name))

                for match in matches:
                    [ann, _, args, rhs, wheres] = match['contents']
                    for arg, var_arg in zip(args, var_args):
                        self.check_node(arg, var_arg, head=Head.type_of(fun_name))
                    self.check_node(rhs, var_rhs, head=Head.type_of(fun_name))
                    if wheres is not None:
                        self.check_node(wheres, self.fresh(), Head.type_of(fun_name))

            case {'tag': 'TypeSig', 'contents': [ann, names, sig]}:
                for name in names:
                    fun_name = combine_module_ident(self.current_module_name, self.get_name(name, toplevel))
                    fun_var = self.bind(fun_name)
                    self.add_rule(fun_var == T, Head.type_of(fun_name), sig['contents'][0], watch=fun_var,
                                  rule_type=RuleType.Type)
                    self.check_node(sig, fun_var, head=Head.type_of(fun_name))

            case {'tag': 'BDecls', 'contents': [ann, decls]}:
                for decl in decls:
                    self.check_node(decl, atom('false'), head)

            case {'tag': 'UnGuardedRhs', 'contents': [ann, exp]}:
                self.check_node(exp, term, head)

            case {'tag': 'GuardedRhss', 'contents': [ann, rhss]}:
                for rhs in rhss:
                    self.check_node({
                        'tag': 'GuardedRhs',
                        'contents': rhs
                    }, term, head)

            case {'tag': 'GuardedRhs', 'contents': [ann, stmts, exp]}:
                self.check_node(exp, term, head)
                for stmt in stmts:
                    match stmt:
                        case {'tag': 'Qualifier', 'contents': [ann, exp]}:
                            pattern_var = self.bind(head.name)
                            self.check_node(exp, pattern_var, head)
                            self.add_rule(pattern_var == t_bool, head, ann, watch=pattern_var,
                                          rule_type=RuleType.Pattern)

                        case {'tag': 'Generator', 'contents': [ann, pat, exp]}:
                            raise NotImplementedError("Pattern guards are not supported")

            # Exp types:
            case {'tag': 'Lit', 'contents': [ann, lit]}:
                self.check_node(lit, term, head)

            case {'tag': 'Con', 'contents': [ann, qname]}:
                self.check_node({'tag': 'Var', 'contents': [ann, qname]}, term, head)

            case {'tag': 'Var', 'contents': [ann, qname]}:
                var_name = self.get_qname(qname, toplevel)
                new_var = self.bind(head.name)
                self.call_graph.add_call(head.name, var_name, new_var.value)
                self.add_rule(new_var == term,
                              head, ann,
                              watch=term,
                              rule_type=RuleType.Var, var_string=just(var_name))

            case {'tag': 'Tuple', 'contents': [ann, _, exps]}:
                node_vars = self.bind_n(len(exps), head.name)
                self.add_rule(term == tuple_of(*node_vars), head, ann, watch=term, rule_type=RuleType.Tuple)
                for exp, node_var in zip(exps, node_vars):
                    self.check_node(exp, node_var, head)

            case {'tag': 'InfixApp', 'contents': [ann, exp1, op, exp2]}:
                self.check_node({
                    'tag': 'App',
                    'contents': [
                        ann,
                        {'tag': 'App', 'contents': [ann, op, exp1]},
                        exp2
                    ]
                }
                    , term, head)

            case {'tag': 'App', 'contents': [ann, exp1, exp2]}:
                [var1, var2, var_result] = self.bind_n(3, head.name)
                self.add_ambient_rule(term == var_result, head)
                self.add_rule(var1 == fun_of(var2, term), head, ann, watch=var_result, rule_type=RuleType.App)
                self.check_node(exp1, var1, head)
                self.check_node(exp2, var2, head)

            case {'tag': 'RightSection', 'contents': [ann, op, right]}:
                [v_op, v_left, v_right, v_result] = self.bind_n(4, head.name)
                self.add_ambient_rule(
                    v_op == fun_of(v_left, v_right, v_result),
                    head
                )
                self.add_rule(
                    term == fun_of(v_left, v_result),
                    head, ann, watch=term, rule_type=RuleType.App
                )
                self.check_node(op, v_op, head)
                self.check_node(right, v_right, head)

            case {'tag': 'LeftSection', 'contents': [ann, left, op]}:
                [v_op, v_left, v_right, v_result] = self.bind_n(4, head.name)
                self.add_ambient_rule(
                    v_op == fun_of(v_left, v_right, v_result),
                    head
                )
                self.add_rule(
                    term == fun_of(v_right, v_result),
                    head, ann, watch=term, rule_type=RuleType.App
                )
                self.check_node(op, v_op, head)
                self.check_node(left, v_left, head)

            case {'tag': 'Lambda', 'contents': [ann, pats, exp]}:
                fun_name = self.lambda_name()
                var_args = self.bind_n(len(pats), fun_name)
                var_rhs = self.bind(fun_name)
                self.check_node(exp, var_rhs, Head.type_of(fun_name))
                for pat, pat_var in zip(pats, var_args):
                    self.check_node(pat, pat_var, Head.type_of(fun_name))
                self.add_ambient_rule(T == fun_of(*var_args, var_rhs), Head.type_of(fun_name))
                self.call_graph.add_closure(head.name, fun_name, CallGraph.FREE)
                lambda_var = self.bind(head.name)
                self.call_graph.add_call(head.name, fun_name, lambda_var.value)
                self.add_rule(lambda_var == term, head, ann, watch=term, rule_type=RuleType.App)

            case {'tag': 'Do', 'contents': [_, stmts]}:
                for stmt in stmts[:-1]:
                    self.check_node(stmt, term, head)
                assert stmts[-1]['tag'] == 'Qualifier'
                self.check_node(stmts[-1]['contents'][1], term, head)

            case {'tag': 'Paren', 'contents': [_, exp]}:
                self.check_node(exp, term, head)

            case {'tag': "If", 'contents': [ann, cond, left_branch, right_branch]}:
                var_cond = self.bind(head.name)
                var_proxy = self.bind(head.name)
                self.add_ambient_rule(var_proxy == t_bool, head)
                self.add_rule(var_cond == t_bool,
                              head,
                              cond['contents'][0],
                              watch=var_proxy,
                              rule_type=RuleType.Lit)
                self.check_node(cond, var_cond, head)
                self.check_node(left_branch, term, head)
                self.check_node(right_branch, term, head)

            case {'tag': 'Case', 'contents': [ann, exp, alts]}:
                matched_var = self.bind(head.name)
                rhs_var: Term = self.bind(head.name)
                self.check_node(exp, matched_var, head)
                self.add_rule(rhs_var == term, head, ann, watch=rhs_var, rule_type=RuleType.Lit)
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
                elem_var = self.bind(head.name)
                self.add_rule(term == list_of(elem_var), head, ann, watch=term, rule_type=RuleType.Lit)
                for exp in exps:
                    self.check_node(exp, elem_var, head)

            # Statements
            case {'tag': 'Generator', 'contents': [ann, pat, exp]}:
                v_exp = self.bind(head.name)
                v_pat = self.bind(head.name)
                self.check_node(exp, v_exp, head)
                monad_var = self.bind(head.name)
                self.add_ambient_rule(struct('adt', cons(monad_var, wildcard)) == term, head)

                self.add_ambient_rule(
                    adt(monad_var, [v_pat]) == v_exp, head)
                self.check_node(pat, v_pat, head)

            case {'tag': 'Qualifier', 'contents': [ann, exp]}:
                v_exp = self.bind(head.name)
                self.check_node(exp, v_exp, head)
                monad_var = self.bind(head.name)
                self.add_ambient_rule(struct('adt', cons(monad_var, wildcard)) == term, head)
                self.add_ambient_rule(
                    struct('adt', cons(monad_var, wildcard)) == v_exp, head)

            case {'tag': 'LetStmt', 'contents': [ann, exp]}:
                raise NotImplementedError("LetStmt")

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
                if (is_synonym := self.type_con_is_synonym(node)) and is_synonym.is_just:
                    self.add_ambient_rule(
                        struct('synonym_of_' + is_synonym.value, term, wildcard),
                        head
                    )
                else:

                    if qname['tag'] == 'Special':
                        match qname['contents'][1]:
                            case {'tag': 'UnitCon', 'contents': _}:
                                # self.add_rule(term == t_unit, head, ann, watch=term, rule_type=RuleType.Type)
                                self.add_ambient_rule(term == t_unit, head)

                            case {'tag': 'ListCon', 'contents': _}:
                                # self.add_rule(term == atom('list'), head, ann, watch=term, rule_type=RuleType.Type)
                                self.add_ambient_rule(term == atom('list'), head)

                            case _:
                                raise Exception("Unknown special type: " + qname['contents'][1])
                    else:
                        type_literal: str = qname['contents'][1]['contents'][1]
                        type_name: str = type_literal[0].lower() + type_literal[1:]
                        self.add_ambient_rule(term == atom(type_name), head)

            case {'tag': 'TyApp', 'contents': [ann, t1, t2]}:
                is_synonym: Maybe[str] = self.type_con_is_synonym(t1)
                if is_synonym.is_just:
                    items = self.unwrap_ty_app(node)[1:]
                    vs = self.bind_n(len(items), head.name)
                    self.add_ambient_rule(
                        struct('synonym_of_' + is_synonym.value, term, Term.array(*vs)),
                        head
                    )
                    for type_part, v in zip(items, vs):
                        self.check_node(type_part, v, head)
                else:
                    v1: Term
                    v2: Term
                    [v1, v2] = self.bind_n(2, head.name)
                    # self.add_rule(term == adt(v1, [v2]), head, ann, watch=term, rule_type=RuleType.Type)
                    self.add_ambient_rule(term == adt(v1, [v2]), head)
                    self.check_node(t1, v1, head)
                    self.check_node(t2, v2, head)

            case {'tag': 'TyFun', 'contents': [ann, t1, t2]}:
                [var1, var2] = self.bind_n(2, head.name)
                self.check_node(t1, var1, head)
                self.check_node(t2, var2, head)
                # self.add_rule(term == fun_of(var1, var2), head, ann, watch=term, rule_type=RuleType.Type)
                self.add_ambient_rule(term == fun_of(var1, var2), head)

            case {'tag': 'TyTuple', 'contents': [ann, _, tys]}:
                args = self.bind_n(len(tys), head.name)
                self.add_rule(term == tuple_of(*args), head, ann, watch=term, rule_type=RuleType.Type)
                # self.add_ambient_rule(term == tuple_of(*args), head)
                for node_ast, node_term in zip(tys, args):
                    self.check_node(node_ast, node_term, head)

            case {'tag': 'TyList', 'contents': [ann, tnode]}:
                t_var = self.bind(head.name)
                self.add_ambient_rule(term == list_of(t_var), head,)
                # self.add_rule(term == list_of(t_var), head, ann, watch=term, rule_type=RuleType.Type)
                self.check_node(tnode, t_var, head)

            case {'tag': 'TyVar', 'contents': [ann, name]}:
                var_name = name['contents'][1]
                self.add_ambient_rule(
                    term == var('TypeVar_' + var_name), head
                )
                # self.add_rule(term == var('TypeVar_' + var_name), head, ann, watch=term, rule_type=RuleType.Type)

            case {'tag': 'TyParen', 'contents': [_, ty]}:
                self.check_node(ty, term, head)

            case {'tag': 'TyForall', 'contents': [_, _, _context, t]}:
                qualifications: list[tuple[str, str, dict]] = self.get_context(_context)
                self.check_node(t, term, head)
                for [class_name, type_var_name, ann] in qualifications:
                    class_name = class_name.split('_')[-1]
                    self.add_ambient_rule(
                        require_class(class_name, var('TypeVar_' + type_var_name), wildcard),
                        head,
                    )
                    # self.add_rule(
                    #     require_class(class_name, var('TypeVar_' + type_var_name), wildcard),
                    #     head,
                    #     ann,
                    #     watch=nothing,
                    #     rule_type=RuleType.TypeClass
                    # )

            # Patterns
            case {'tag': 'PVar', 'contents': [ann, name]}:
                p_name = combine_module_ident(self.current_module_name, self.get_name(name, toplevel))
                new_var = self.bind(head.name)
                self.add_ambient_rule(new_var == term, head)
                self.call_graph.add_closure(head.name, p_name, CallGraph.BOUND)
                self.call_graph.add_call(head.name, p_name, new_var.value)

            case {'tag': 'PTuple', 'contents': [ann, _, pats]}:
                elems = self.bind_n(len(pats), head.name)
                self.add_rule(term == tuple_of(*elems), head, ann, watch=term, rule_type=RuleType.Lit)
                for pat, elem in zip(pats, elems):
                    self.check_node(pat, elem, head)

            case {'tag': 'PList', 'contents': [ann, pats]}:
                elem = self.bind(head.name)
                self.add_rule(term == list_of(elem), head, ann, watch=term, rule_type=RuleType.Lit)
                for pat in pats:
                    self.check_node(pat, elem, head)

            case {'tag': 'PLit', 'contents': [ann, _, lit]}:
                self.check_node(lit, term, head)

            case {'tag': "PParen", "contents": [ann, p]}:
                self.check_node(p, term, head)

            case {'tag': 'PApp', 'contents': [ann, qname, p_args]}:
                fun_name = self.get_qname(qname, toplevel)
                fun_var = self.bind(head.name)
                arg_vars = self.bind_n(len(p_args), head.name)
                self.add_rule(fun_var == fun_of(*arg_vars, term), head, ann, watch=term,
                              rule_type=RuleType.App)
                self.call_graph.add_call(head.name, fun_name, fun_var.value)
                for arg_ast, arg_var in zip(p_args, arg_vars):
                    self.check_node(arg_ast, arg_var, head)

            case {'tag': 'PInfixApp', 'contents': [ann, p1, op, p2]}:
                self.check_node({
                    'tag': 'PApp',
                    'contents': [ann, op, [p1, p2]]
                }, term, head)
            case {'tag': 'PWildCard', 'contents': _}:
                pass
            case {'tag': 'QVarOp', 'contents': [ann, qname]}:
                v = self.bind(head.name)
                self.call_graph.add_call(head.name, self.get_qname(qname, toplevel), v.value)
                self.add_rule(v == term, head, ann, watch=term, rule_type=RuleType.Var)

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
                print(ujson.dumps(node))

                print("Unknown node type: ", node.get('type'))
                raise NotImplementedError


if __name__ == "__main__":
    to_check_file = "Test.hs"
    project_dir = Path(__file__).parent.parent
    base_dir = Path(__file__).parent.parent / "tmp" / "test"
    parser_bin = str(project_dir / "bin" / "haskell-parser.exe") if platform() == 'Windows' else str(
        project_dir / "bin" / "haskell-parser")
    result = run(f'{parser_bin} {base_dir}', shell=True, check=True, capture_output=True)
    parsed_data = ujson.loads(result.stdout)
    synonyms = list(chain(*[gather_type_synonym(r['ast']) for r in parsed_data['contents']]))
    call_graphs = {}
    free_vars = {}
    type_alias = {}

    for file_id, parse_result in enumerate(parsed_data['contents']):
        ast = parse_result['ast']
        file = parse_result['file']
        module_name = parse_result['moduleName']
        prolog_file = file[:-3] + '.pl'
        with Prolog(interface=PlInterface.File, file=base_dir / prolog_file) as prolog:
            system = System(
                base_dir=base_dir,
                synonyms=synonyms,
                ast=ast,
                file_id=file_id,
                hs_file=base_dir / file,
                module_name=module_name,
                prolog_instance=prolog
            )
            system.marshal()
            system.generate_only()
            call_graphs.update(system.call_graph.graph)
            free_vars.update(system.free_vars)

    for file_id, parse_result in enumerate(parsed_data['contents']):
        ast = parse_result['ast']
        file = parse_result['file']
        module_name = parse_result['moduleName']
        if file == to_check_file:
            prolog_file = file[:-3] + '.pl'
            with Prolog(interface=PlInterface.File, file=base_dir / prolog_file) as prolog:
                system = System(
                    base_dir=base_dir,
                    ast=ast,
                    file_id=file_id,
                    hs_file=base_dir / file,
                    module_name=module_name,
                    prolog_instance=prolog,
                    synonyms=synonyms
                )
                system.marshal()
                system.call_graph.graph.update(call_graphs)
                system.free_vars.update(free_vars)
                system.generate_only()
                for query in system.prolog.queries:
                    print(query.__str__() + ',')
                print('true.')
                r = system.type_check()
                print(r)
