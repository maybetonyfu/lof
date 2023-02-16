from subprocess import run
from pathlib import Path
import json
from z3 import *

project_dir = Path(__file__).parent.parent
node_runtime = "node"
program = str(project_dir / "out" / "main.js")
code = str(project_dir / "example" / "test.ts")

result = run([node_runtime, program, code], shell=True, check=True, capture_output=True)
parse_data = json.loads(result.stdout)

Type = Datatype('Type')
TypeSort = DatatypeSort('Type')

Type.declare('Any')
Type.declare('String')
Type.declare('Number')
Type.declare('Bool')
Type.declare('array', ('elem', Type))
Type.declare('union', ('part1', Type), ('part2', Type))


Type.declare('fun0', ('ret', Type))
Type.declare('fun1', ('arg0', Type), ('ret', Type))
Type.declare('fun2', ('arg0', Type), ('arg1', Type), ('ret', Type))
Type.declare('fun3', ('arg0', Type), ('arg1', Type), ('arg2', Type), ('ret', Type))
Type.declare('fun4', ('arg0', Type), ('arg1', Type), ('arg2', Type), ('arg3', Type), ('ret', Type))
Type.declare('fun5', ('arg0', Type), ('arg1', Type), ('arg2', Type), ('arg3', Type), ('arg4', Type), ('ret', Type))

Type = Type.create()
t_any = Type.Any
t_string = Type.String
t_number = Type.Number
t_bool = Type.Bool
fun0 = Type.fun0
fun1 = Type.fun1
fun2 = Type.fun2
fun3 = Type.fun3
fun4 = Type.fun4
fun5 = Type.fun5
union = Type.union

subtype = Function('subtype', Type, Type, BoolSort())


def union_sub_union(union1, union2):
    ands = []
    for u1 in union1.children():
        ors = []
        for u2 in union2.children():
            ors.append(subtype(u1, u2))
        ands.append(Or(*ors))
    print(ands)
    return And(*ands)


def term_sub_union(t, t_union):
    ors = []
    for u in t_union.children():
        ors.append(subtype(t, u))
    return Or(*ors)

agr0 = Type.arg0
arg1 = Type.arg1
arg2 = Type.arg2
arg3 = Type.arg3
arg4 = Type.arg4
ret = Type.ret


def union_of(*ts):
    print(ts)
    match len(ts):
        case 1:
            raise Exception("Arity out of range")
        case 0:
            raise Exception("Arity out of range")
        case 2:
            return union(ts[0], ts[1])
        case _:
            return union(ts[0], union_of(*ts[1:]))


def fun_of(*ts):
    match len(ts):
        case 1:
            return fun0(*ts)
        case 2:
            return fun1(*ts)
        case 3:
            return fun2(*ts)
        case 4:
            return fun3(*ts)
        case 5:
            return fun4(*ts)
        case 6:
            return fun5(*ts)
        case _:
            raise Exception("Arity out of range")


def is_atom_type(t):
    return Or(t == Type.Bool, t == Type.String, t == Type.Number)


def is_func(t):
    return Or(
        t.decl() == Type.fun0,
        t.decl() == Type.fun1,
        t.decl() == Type.fun2,
        t.decl() == Type.fun3,
        t.decl() == Type.fun4,
        t.decl() == Type.fun5,
    )


def is_array(t):
    return t.decl() == Type.array





s = Solver()
x, y, z, w, u, v = Consts('x y z w u v', Type)
x0, x1, x2, x3, x4, x5 = Consts('x0 x1 x2 x3 x4 x5', Type)
y0, y1, y2, y3, y4, y5 = Consts('y0 y1 y2 y3 y4 y5', Type)
s.add([
    ForAll([x, y, z], Implies(And(subtype(x, y), subtype(y, z)), subtype(x, z))),
    ForAll(x, subtype(x, t_any)),
    Not(subtype(t_bool, t_number)),
    Not(subtype(t_number, t_bool )),
    Not(subtype(t_string, t_bool)),
    Not(subtype(t_bool, t_string)),
    Not(subtype(t_string, t_number)),
    Not(subtype( t_number, t_string)),
    ForAll([x], Not(subtype(t_bool, Type.array(x)))),
    ForAll([x], Not(subtype(t_string, Type.array(x)))),
    ForAll([x], Not(subtype(t_number, Type.array(x)))),
    ForAll([x], Not(subtype(Type.array(x), t_bool))),
    ForAll([x], Not(subtype(Type.array(x), t_string))),
    ForAll([x], Not(subtype(Type.array(x), t_number))),
    subtype(union(t_bool, t_bool), t_bool),
    subtype(union(t_number, t_number), t_number),
    subtype(union(t_string, t_string), t_string),
    ForAll([x, y],Implies(
        Or(x == t_number, x == t_string, y == t_number, y == t_string),
        Not(subtype(union(x, y), t_bool))
    )),
    ForAll([x, y], Implies(
        Or(x == t_number, x == t_bool, y == t_number, y == t_bool),
        Not(subtype(union(x, y), t_string))
    )),
    ForAll([x, y], Implies(
        Or(x == t_string, x == t_bool, y == t_string, y == t_bool),
        Not(subtype(union(x, y), t_number))
    )),
    ForAll([y0, y1, y2], Implies(subtype(union(y0, union(y1, y2)), t_bool), And(y0 == t_bool, subtype(union(y1, y2), t_bool)))),
    ForAll([y0, y1, y2], Implies(subtype(union(y0, union(y1, y2)), t_string), And(y0 == t_string, subtype(union(y1, y2), t_string)))),
    ForAll([y0, y1, y2], Implies(subtype(union(y0, union(y1, y2)), t_number), And(y0 == t_number, subtype(union(y1, y2), t_number)))),
    ForAll([x, y0, y1],  Not(subtype(union(y0, y1), Type.array(x)))),
    ForAll([x0, y0, y1], Not(subtype(union(y0, y1), fun0(x0)))),
    ForAll([x0, x1, y0, y1], Not(subtype(union(y0, y1), fun1(x0, x1)))),
    ForAll([x0, x1, x2, y0, y1],  Not(subtype(union(y0, y1), fun2(x0, x1, x2)))),
    ForAll([x0, x1, x2, x3, y0, y1],  Not(subtype(union(y0, y1), fun3(x0, x1, x2, x3)))),
    ForAll([x0, x1, x2, x3, x4, y0, y1],  Not(subtype(union(y0, y1), fun4(x0, x1, x2, x3, x4)))),
    ForAll([x0, x1, x2, x3, x4, x5, y0, y1],  Not(subtype(union(y0, y1), fun5(x0, x1, x2, x3, x4, x5)))),
    ForAll([x, y], Implies(subtype(Type.array(x), Type.array(y)), subtype(x, y))),
    ForAll([x0, y0], Implies(subtype(fun0(x0), fun0(y0)), subtype(x0, y0))),
    ForAll([x0, x1,
            y0, y1], Implies(
        subtype(fun1(x0, x1), fun1(y0, y1)),
        And(
            subtype(y0, x0),
            subtype(x1, y1)
        )
    )),
    ForAll([x0, x1, x2,
            y0, y1, y2], Implies(
        subtype(fun2(x0, x1, x2), fun2(y0, y1, y2)),
        And(
            subtype(y0, x0),
            subtype(y1, x1),
            subtype(x2, y2)
        )
    )),
    ForAll([x0, x1, x2, x3,
            y0, y1, y2, y3], Implies(
        subtype(fun3(x0, x1, x2, x3), fun3(y0, y1, y2, y3)),
        And(
            subtype(y0, x0),
            subtype(y1, x1),
            subtype(y2, x2),
            subtype(x3, y3)
        )
    )),
    ForAll([x0, x1, x2, x3, x4,
            y0, y1, y2, y3, y4], Implies(
        subtype(fun4(x0, x1, x2, x3, x4), fun4(y0, y1, y2, y3, y4)),
        And(
            subtype(y0, x0),
            subtype(y1, x1),
            subtype(y2, x2),
            subtype(y3, x3),
            subtype(x4, y4)
        )
    )),
    ForAll([x0, x1, x2, x3, x4, x5,
            y0, y1, y2, y3, y4, y5], Implies(
        subtype(fun5(x0, x1, x2, x3, x4, x5), fun5(y0, y1, y2, y3, y4, y5)),
        And(
            subtype(y0, x0),
            subtype(y1, x1),
            subtype(y2, x2),
            subtype(y3, x3),
            subtype(y4, x4),
            subtype(x5, y5),
        )
    )),
    ForAll([x0, x1, y0, y1], Implies(
        subtype(union(x0, x1), union(y0, y1)),
        And(
            subtype(x0, union(y0, y1)),
            subtype(x1, union(y0, y1))),
    ), patterns=[subtype(union(x0, x1), union(y0, y1))]),
    ForAll([y0, y1], Implies(
        subtype(t_number, union(y0, y1)),
        Or(subtype(t_number, y0), subtype(t_number, y1)),
    ), patterns=[subtype(t_number, union(y0, y1))]),
    ForAll([y0, y1], Implies(
        subtype(t_bool, union(y0, y1)),
        Or(subtype(t_bool, y0), subtype(t_bool, y1)),
    ), patterns=[subtype(t_bool, union(y0, y1))]),
    ForAll([y0, y1], Implies(
        subtype(t_string, union(y0, y1)),
        Or(subtype(t_string, y0), subtype(t_string, y1)),
    ), patterns=[subtype(t_string, union(y0, y1))]),
    ForAll([x, y0, y1], Implies(
        subtype(Type.array(x), union(y0, y1)),
        Or(subtype(Type.array(x), y0), subtype(Type.array(x), y1)),
    ), patterns=[subtype(Type.array(x), union(y0, y1))]),
    # ForAll([x0, y0, y1], Implies(
    #     subtype(fun0(x0), union(y0, y1)),
    #     Or(subtype(fun0(x0), y0), subtype(fun0(x0), y1)),
    # )),
    # ForAll([x0, x1, y0, y1], Implies(
    #     subtype(fun1(x0, x1), union(y0, y1)),
    #     Or(subtype(fun1(x0, x1), y0), subtype(fun1(x0, x1), y1)),
    # )),
    # ForAll([x0, x1,x2, y0, y1], Implies(
    #     subtype(fun2(x0, x1, x2), union(y0, y1)),
    #     Or(subtype(fun2(x0, x1, x2), y0), subtype(fun2(x0, x1, x2), y1)),
    # )),
    # ForAll([x0, x1, x2,x3, y0, y1], Implies(
    #     subtype(fun3(x0, x1, x2, x3), union(y0, y1)),
    #     Or(subtype(fun3(x0, x1, x2, x3), y0), subtype(fun3(x0, x1, x2, x3), y1)),
    # )),
    # ForAll([x0, x1, x2, x3, x4, y0, y1], Implies(
    #     subtype(fun4(x0, x1, x2, x3, x4), union(y0, y1)),
    #     Or(subtype(fun4(x0, x1, x2, x3, x4), y0), subtype(fun4(x0, x1, x2, x3, x4), y1)),
    # )),
    # ForAll([x0, x1, x2, x3, x4, x5, y0, y1], Implies(
    #     subtype(fun5(x0, x1, x2, x3, x4, x5), union(y0, y1)),
    #     Or(subtype(fun5(x0, x1, x2, x3, x4, x5), y0), subtype(fun5(x0, x1, x2, x3, x4, x5), y1)),
    # )),
    # ForAll([x, y0, y1], Implies(
    #     And(subtype(x, union(y0, y1)),
    #         is_atom_type(x),
    #         ),
    #     Or(subtype(x, y0), subtype(x, y1)),
    # )),


    # subtype(
    #     t_bool,
    #     union_of(t_bool, t_number),
    # )
])


class System:
    def __init__(self, ast, scope):
        self.ast = ast
        self.scope = scope
        self.counter = 0
        self.constraints = []

    def var_from_def(self, name, span):
        vs = [var for var in self.scope if var['name'] == name and span in var['defs']]
        if len(vs) == 0:
            raise Exception(f'var {name} can not be found')
        else:
            vid = vs[0]['id']
            internal_name = f'{name}:{vid}'
            return Const(internal_name, Type)


    def var_from_use(self, name, span):
        vs = [var for var in self.scope if var['name'] == name and span in var['refs']]
        if len(vs) == 0:
            raise Exception(f'var {name} can not be found')
        else:
            vid = vs[0]['id']
            internal_name = f'{name}:{vid}'
            var = Const(internal_name, Type)
            return var

    def fresh(self):
        self.counter += 1
        internal_name = f'fresh{self.counter}'
        var = Const(internal_name, Type)
        return var

    def match(self, a, b):
        self.constraints.append(a == b)

    def add_raw_constraint(self, c):
        self.constraints.append(c)

    def is_subtype(self, a, b):
        self.constraints.append(subtype(a, b))

    def show_constraints(self):
        for c in self.constraints:
            print(c)

    def solve(self):
        s.add(self.constraints)
        print('=====================')
        user_defined_vars = [f'{v.get("name")}:{v.get("id")}' for v in self.scope if v['defs'] != []]
        # print(s)
        if s.check().r == 1:
            m = s.model()
            for val in m:
                if val.name() in user_defined_vars:
                    print(val, "=", m[val])
        else:
            print('The code is not well-typed')
            print(s.unsat_core())

    def run_node(self, node, term, bounds):
        match node:
            case {'type': "Program", 'body': decls, **kw}:
                for d in decls: self.run_node(d, term, bounds)

            # Statements
            case {'type': 'ExpressionStatement', 'expression': exp, **kw}:
                self.run_node(exp, term, bounds)

            case {'type': 'BlockStatement', 'body': decls, **kw}:
                for d in decls: self.run_node(d, term, bounds)

            case {'type': 'ReturnStatement', 'argument': arg, **kw}:
                return_type = bounds[0]['return_type']
                self.run_node(arg, return_type, bounds[1:])

            case {'type': 'BlockStatement', 'body': decls, **kw}:
                for d in decls: self.run_node(d, term, bounds)

            # Expressions
            case {'type': 'AssignmentExpression', 'left': left, 'right': right, **kw}:
                var_left = self.fresh()
                var_right = self.fresh()
                self.is_subtype(var_right, var_left)
                self.run_node(left, var_left, bounds)
                self.run_node(right, var_right, bounds)

            case {'type': 'ArrayExpression', 'elements': elems}:
                elements = []
                for elem in elems:
                    elem_type = self.fresh()
                    self.run_node(elem, elem_type, bounds)
                    elements.append(elem_type)
                unioned = union_of(*elements)
                self.is_subtype(Type.array(unioned), term)

            case {'type': 'CallExpression', 'callee': callee, 'arguments': args, **kw}:
                arg_types = []
                para_types = []
                ret_type = self.fresh()
                for p in args:
                    p_para = self.fresh()
                    p_arg = self.fresh()
                    self.run_node(p, p_arg, bounds)
                    para_types.append(p_para)
                    arg_types.append(p_arg)
                    self.is_subtype(p_arg, p_para)
                if callee['type'] == "Identifier":
                    f = self.var_from_use(callee['name'], callee['range'])
                    self.match(fun_of(*para_types, ret_type), f)
                    self.match(term, ret_type)
                else:
                    raise NotImplementedError

            # Declarations
            case {'type': "VariableDeclaration", 'declarations': decls, **kw}:
                for d in decls: self.run_node(d, t_any, bounds)

            case {'type': "VariableDeclarator", 'id': var_id, 'init': var_init, **kw}:
                var = self.var_from_def(var_id['name'], var_id['range'])
                rhs = self.fresh()
                if var_init is not None:
                    self.run_node(var_init, rhs, bounds)
                if var_id.get('typeAnnotation') is None:
                    self.match(rhs, var)
                else:
                    self.is_subtype(rhs, var)
                    self.run_node(var_id.get('typeAnnotation').get('typeAnnotation'), var, bounds)

            case {'type': 'FunctionDeclaration', 'id': f_id, 'params': params, 'body': body, **kw}:
                f_name = f_id['name']
                f_span = f_id['range']
                f_type = self.var_from_def(f_name, f_span)
                param_types = []
                for p in params:
                    p_var = self.var_from_def(p['name'], p['range'])
                    if p.get('typeAnnotation') is None:
                        self.match(p_var, Type.Any)
                    else:
                        self.run_node(p['typeAnnotation']['typeAnnotation'], p_var, bounds)
                    param_types.append(p_var)
                return_type = self.fresh()
                self.run_node(body, term, [
                    {'fun': f_name,
                     'vars': [*param_types, return_type],
                     'return_type': return_type,
                     }, *bounds])
                self.match(f_type, fun_of(*param_types, return_type))

            # Types
            case {'type': "TSNumberKeyword", **kw}:
                self.match(term, Type.Number)

            case {'type': "TSAnyKeyword", **kw}:
                self.match(term, Type.Any)

            case {'type': "TSStringKeyword", **kw}:
                self.match(term, Type.String)

            case {'type': "TSBooleanKeyword", **kw}:
                self.match(term, Type.Bool)

            case {'type': "TSArrayType", "elementType": elem, **kw}:
                elem_var = self.fresh()
                self.run_node(elem, elem_var, bounds)
                self.match(term, Type.array(elem_var))
            case {'type': 'TSUnionType', 'types': ts, **kw}:
                t_vars = []
                for t in ts:
                    t_var = self.fresh()
                    self.run_node(t, t_var, bounds)
                    t_vars.append(t_var)
                self.match(term, union_of(*t_vars))


            case {'type': "Identifier", "name": name, "range": span, **kw}:
                var = self.var_from_use(name, span)
                self.match(var, term)

            # Literal
            case {'type': "Literal", "value": value, **kw}:
                if type(value) is int or type(value) is float:
                    self.match(term, Type.Number)
                elif type(value) is str:
                    self.match(term, Type.String)
                elif type(value) is bool:
                    self.match(term, Type.Bool)
            case _:
                print("Unknown node type: ", node.get('type'), node)
                raise NotImplementedError


if __name__ == "__main__":
    system = System(parse_data.get('ast'), parse_data.get('scope'))
    system.run_node(system.ast, Const('program', Type), [])
    system.show_constraints()
    system.solve()
    print(system.ast)


    # ForAll([x, y], Implies(fun0(x) == fun0(y), x == y)),
    # ForAll([x, y, z], Implies(And(subtype(x, y), subtype(y, z)), subtype(x, z))),
    # ForAll([x, y], Implies(And(subtype(x, y), subtype(y, x)), x == y)),
    # ForAll([x, y, z], Implies(And(subtype(x, y), subtype(x, z)), Or(subtype(y, z), subtype(z, y)))),