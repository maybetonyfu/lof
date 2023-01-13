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
Type.declare('Any')
Type.declare('String')
Type.declare('Number')
Type.declare('Bool')
Type.declare('fun0', ('ret', Type))
Type.declare('fun1', ('arg0', Type), ('ret', Type))
Type.declare('fun2', ('arg0', Type), ('arg1', Type), ('ret', Type))
Type.declare('fun3', ('arg0', Type), ('arg1', Type), ('arg2', Type), ('ret', Type))
Type.declare('fun4', ('arg0', Type), ('arg1', Type), ('arg2', Type), ('arg3', Type), ('ret', Type))
Type.declare('fun5', ('arg0', Type), ('arg1', Type), ('arg2', Type), ('arg3', Type), ('arg4', Type), ('ret', Type))

Type = Type.create()
any = Type.Any
fun0 = Type.fun0
fun1 = Type.fun1
fun2 = Type.fun2
fun3 = Type.fun3
fun4 = Type.fun4
fun5 = Type.fun5

subtype = Function('subtype', Type, Type, BoolSort())


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


s = Solver()
x, y, z, w, u, v = Consts('x y z w u v', Type)
x0, x1, x2, x3, x4, x5 = Consts('x0 x1 x2 x3 x4 x5', Type)
y0, y1, y2, y3, y4, y5 = Consts('y0 y1 y2 y3 y4 y5', Type)
s.add([
    ForAll([x, y], Implies(And(is_atom_type(x), is_atom_type(y), x != y), Not(subtype(x, y)))),
    ForAll(x, subtype(x, any)),
    ForAll([x, y], Implies(fun0(x) == fun0(y), x == y)),
    ForAll([x, y, z], Implies(And(subtype(x, y), subtype(y, z)), subtype(x, z))),
    ForAll([x, y], Implies(And(subtype(x, y), subtype(y, x)), x == y)),
    ForAll([x, y, z], Implies(And(subtype(x, y), subtype(x, z)), Or(subtype(y, z), subtype(z, y)))),
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
            return Const(f'{name}:{vid}', Type)

    def var_from_use(self, name, span):
        vs = [var for var in self.scope if var['name'] == name and span in var['refs']]
        if len(vs) == 0:
            raise Exception(f'var {name} can not be found')
        else:
            id = vs[0]['id']
            return Const(f'{name}:{id}', Type)

    def fresh(self):
        self.counter += 1
        return Const(f'fresh{self.counter}', Type)

    def add_constraint(self, a, b):
        self.constraints.append(a == b)

    def is_subtype(self, a, b):
        self.constraints.append(subtype(a, b))

    def show_constraints(self):
        for c in self.constraints:
            print(c)

    def solve(self):
        s.add(self.constraints)
        print('=====================')
        user_defined_vars = [f'{v.get("name")}:{v.get("id")}' for v in self.scope if v['defs'] != []]
        if s.check().r == 1:
            m = s.model()
            for val in m:
                if val.name() in user_defined_vars:
                    print(val, "=", m[val])
        else:
            print('unsat')


    def run_node (self, node, term, bounds):
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

            case {'type': 'CallExpression', 'callee': callee, 'arguments': args,  **kw}:
                arg_types = []
                ret_type = self.fresh()
                for p in args:
                    p_var = self.fresh()
                    self.run_node(p, p_var, bounds)
                    arg_types.append(p_var)
                if callee['type'] == "Identifier":
                    f = self.var_from_use(callee['name'], callee['range'])
                    self.is_subtype(fun_of(*arg_types, ret_type), f)
                else:
                    raise NotImplementedError

            # Declarations
            case {'type': "VariableDeclaration", 'declarations': decls, **kw}:
                for d in decls: self.run_node(d, None, bounds)


            case {'type': "VariableDeclarator", 'id': var_id, 'init': var_init,  **kw}:
                var = self.var_from_def(var_id['name'], var_id['range'])
                if var_init is not None:
                    self.run_node(var_init, var, bounds)
                if var_id.get('typeAnnotation') is not None:
                    self.run_node(var_id.get('typeAnnotation').get('typeAnnotation'), var, bounds)


            case {'type': 'FunctionDeclaration', 'id': f_id, 'params': params, 'body': body,  **kw}:
                f_name = f_id['name']
                f_span = f_id['range']
                f_type = self.var_from_def(f_name, f_span)
                param_types = []
                for p in params:
                    p_var = self.var_from_def(p['name'], p['range'])
                    if p.get('typeAnnotation') is None:
                        self.add_constraint(p_var, Type.Any)
                    else:
                        self.run_node(p['typeAnnotation']['typeAnnotation'], p_var, bounds)
                    param_types.append(p_var)
                return_type = self.fresh()
                self.run_node(body, term, [
                    {'fun': f_name,
                     'vars': [*param_types, return_type],
                     'return_type': return_type,
                     }, *bounds])
                self.add_constraint(f_type, fun_of(*param_types, return_type))


            # Types
            case {'type': "TSNumberKeyword",  **kw}:
                self.add_constraint(term, Type.Number)

            case {'type': "TSStringKeyword",  **kw}:
                self.add_constraint(term, Type.String)

            case {'type': "TSBooleanKeyword",  **kw}:
                self.add_constraint(term, Type.Bool)

            case {'type': "Identifier", "name": name, "range": span, **kw}:
                var = self.var_from_use(name, span)
                self.add_constraint(var, term)

            # Literal
            case {'type': "Literal", "value": value,  **kw}:
                if type(value) is int or type(value) is float:
                    self.add_constraint(term, Type.Number)
                elif type(value) is str:
                    self.add_constraint(term, Type.String)
                elif type(value) is bool:
                    self.add_constraint(term, Type.Bool)
            case _:
                print("Unknown node type: ", node.get('type'),  node)
                raise NotImplementedError


if __name__ == "__main__":
    system = System(parse_data.get('ast'), parse_data.get('scope'))
    system.run_node(system.ast, None, [])
    system.show_constraints()
    system.solve()



