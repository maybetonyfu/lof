from subprocess import run
from pathlib import Path
import json
from z3 import *
import pprint
from dataclasses import dataclass
from typing import TypeAlias, NewType
from enum import Enum

TypeVar: TypeAlias = DatatypeRef
Fid = NewType("FunctionMetaId", str)
pp = pprint.PrettyPrinter(indent=2)

Type = Datatype('Type')
TypeSort = DatatypeSort('Type')

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
    vid: int
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
    clause: BoolRef
    callstack: list[Fid]


class Context(Enum):
    Global = 1
    Local = 2


class System:
    def __init__(self):
        self.rootDir = dir
        self.project_dir = Path(__file__).parent.parent
        parser_bin = str(self.project_dir / "bin" / "haskell-tool-exe.exe")
        code_dir = str(self.project_dir / "example")
        result = run([parser_bin, code_dir], shell=True, check=True, capture_output=True)
        self.parsed_data = json.loads(result.stdout)
        self.asts = [c['ast'] for c in self.parsed_data['contents']]
        self.solver = Solver()
        self.variable_counter: int = 0
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
                vid=vid,
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
            line: int | None = None,
            col: int | None = None,
            module: str | None = None) -> TypeVar:
        if line is None or col is None:
            v = [v for v in self.variable_table if v.callstack == [] and v.name == name][0]
            return Const(v.internal_name, Type)
        else:
            v = [v for v in self.variable_table if v.line == line and v.col == col and v.name == name][0]
            return Const(v.internal_name, Type)

    def show_variables(self):
        print("Variables:")
        for v in self.variable_table:
            print(v)

    def show_functions(self):
        print("Function contexts:")
        for v in self.function_table:
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
                vid=vid,
                is_fresh=True
            ))
        var: TypeVar = Const(internal_name, Type)
        return var

    def fresh_n(self, n: int, callstack: list[Fid]) -> list[TypeVar]:
        fresh_vars = []
        for i in range(n):
            fresh_vars.append(self.fresh(callstack))
        return fresh_vars

    def add_rule(self, clause: BoolRef, callstack: list[Fid]):
        self.rules.append(Rule(callstack=callstack, clause=clause))

    def make_clause(self, proxies: list[TypeVar], arg_vars: list[TypeVar], function_var: TypeVar, return_var: TypeVar,
                    body: BoolRef) -> BoolRef:
        '''Proxies: Used in the apply relation, and universal var.
        Genereally the usiversal vars are [proxy, first var, rest vars]
        '''
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

    def solve(self):
        defs = []
        for c in self.function_table:
            function_proxies = self.fresh_n(len(c.arg_vars), [])
            universal_vars = c.arg_vars + [c.return_var]
            universal_var_names = [i.decl().name() for i in universal_vars]
            existential_vars = [Const(v.internal_name, Type) for v in self.variable_table if
                                c.fid in v.callstack and v.internal_name not in universal_var_names]
            body = Exists(
                existential_vars,
                And([r.clause for r in self.rules if c.fid in r.callstack])
            )
            defs.append(self.make_clause(
                proxies=function_proxies,
                arg_vars=c.arg_vars,
                function_var=c.function_var,
                return_var=c.return_var,
                body=body
            ))


        tlds = [r.clause for r in self.rules if r.callstack == []]
        print('defs:')
        print(defs)
        print('tlds:')
        print(tlds)
        self.solver.add(defs)
        self.solver.add(tlds)
        print('=====================')
        user_defined_vars = [v.internal_name for v in self.variable_table if not v.is_fresh]
        if self.solver.check().r == 1:
            m = self.solver.model()
            for val in m:
                if val.name() in user_defined_vars:
                    print(val, "=", m[val])
        else:
            print('The code is not well-typed')

    def type_check(self):
        for ast in self.asts:
            self.check_node(ast, Type.Unit, [])

    def get_name_var(self, node, callstack: list[Fid]) -> (TypeVar, str):
        # pp.pprint(node)
        match node:
            case {'tag': 'Ident', 'contents': [ann, ident]}:
                if ann['scope']['type'] == 'ValueBinder':
                    var = self.make_variable(
                        ident,
                        line=ann['loc']['from']['line'],
                        col=ann['loc']['from']['col'],
                        module='Test',
                        callstack=callstack
                    )
                    return var

                elif ann['scope']['type'] == 'GlobalSymbol':
                    var = self.lookup_variable(ident)
                    return var

                elif ann['scope']['type'] == 'LocalValue':
                    var = self.lookup_variable(
                        ident,
                        line=ann['scope']['loc']['line'],
                        col=ann['scope']['loc']['col'],
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
                self.add_rule(apply(var1, var2, term), callstack)

            # Lit nodes:
            case {'tag': 'Char', 'contents': [ann, _, _]}:
                self.add_rule(term == Type.Char, callstack)

            case {'tag': 'String', 'contents': [ann, _, _]}:
                self.add_rule(term == list_of(Type.Char), callstack)

            case {'tag': 'Int', 'contents': [ann, _, _]}:
                self.add_rule(term == Type.Int, callstack)

            case {'tag': 'Frac', 'contents': [ann, _, _]}:
                self.add_rule(term == Type.Float, callstack)

            # Patterns
            case {'tag': 'PVar', 'contents': [ann, name]}:
                var = self.get_name_var(name, callstack)
                self.add_rule(var == term, callstack)

            # Namings
            case {'tag': 'UnQual', 'contents': [_, name]}:
                var = self.get_name_var(name, callstack)
                self.add_rule(var == term, callstack)

            case _:
                print("Unknown node type: ", node.get('type'), node)
                raise NotImplementedError


if __name__ == "__main__":
    system = System()
    system.type_check()
    system.show_variables()
    system.show_functions()

    system.solve()
