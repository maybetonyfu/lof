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
Type.declare('Function0', ('Arg0', Type))
Type.declare('Function1', ('Arg0', Type), ('Arg1', Type))
Type.declare('Function2', ('Arg0', Type), ('Arg1', Type), ('Arg2', Type))
Type.declare('Function3', ('Arg0', Type), ('Arg1', Type), ('Arg2', Type), ('Arg3', Type))
Type.declare('Function4', ('Arg0', Type), ('Arg1', Type), ('Arg2', Type), ('Arg3', Type), ('Arg4', Type))
Type.declare('Function5', ('Arg0', Type), ('Arg1', Type), ('Arg2', Type), ('Arg3', Type), ('Arg4', Type), ('Arg5', Type))
Type = Type.create()


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
            id = vs[0]['id']
            return Const(f'{name}:{id}', Type)

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

    def show_constraints(self):
        for c in self.constraints:
            print(c)

    def solve(self):
        solve(*self.constraints)

    def run_node (self, node, term, bounds):
        match node:
            case {'type': "Program", 'body': decls, **kw}:
                for d in decls: self.run_node(d, term, bounds)

            case {'type': 'ExpressionStatement', 'expression': exp, **kw}:
                self.run_node(exp, term, bounds)

            case {'type': 'BlockStatement', 'body': decls, **kw}:
                for d in decls: self.run_node(d, term, bounds)

            case {'type': 'ReturnStatement', 'argument': arg, **kw}:
                return_type = bounds[0]['return_type']
                self.run_node(arg, return_type, bounds[1:])

            case {'type': 'BlockStatement', 'body': decls, **kw}:
                for d in decls: self.run_node(d, term, bounds)

            case {'type': 'AssignmentExpression', 'left': left, 'right': right, **kw}:
                left_var = self.fresh()
                right_var = self.fresh()

                self.add_constraint(left_var, right_var)
                self.run_node(left, left_var, bounds)
                self.run_node(right, right_var, bounds)

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
                param_types = [self.var_from_def(p['name'], p['range']) for p in params]
                return_type = self.fresh()
                self.run_node(body, term, [
                    {'fun': f_name,
                     'vars': [*param_types, return_type],
                     'return_type': return_type,
                     }, *bounds])
                match len(param_types):
                    case 0:
                        self.add_constraint(f_type, Type.Function0(return_type))
                    case 1:
                        self.add_constraint(f_type, Type.Function1(*param_types, return_type))
                    case 2:
                        self.add_constraint(f_type, Type.Function2(*param_types, return_type))
                    case 3:
                        self.add_constraint(f_type, Type.Function3(*param_types, return_type))
                    case 4:
                        self.add_constraint(f_type, Type.Function4(*param_types, return_type))
                    case 5:
                        self.add_constraint(f_type, Type.Function5(*param_types, return_type))
                    case _:
                        raise Exception("Arity out of range")

            case {'type': "TSNumberKeyword",  **kw}:
                self.add_constraint(term, Type.Number)

            case {'type': "TSStringKeyword",  **kw}:
                self.add_constraint(term, Type.String)

            case {'type': "TSBooleanKeyword",  **kw}:
                self.add_constraint(term, Type.Bool)

            case {'type': "Identifier", "name": name, "range": span, **kw}:
                var = self.var_from_use(name, span)
                self.add_constraint(var, term)

            case {'type': "Literal", "value": value,  **kw}:
                if type(value) is int or type(value) is float:
                    self.add_constraint(term, Type.Number)
                elif type(value) is str:
                    self.add_constraint(term, Type.String)
                elif type(value) is bool:
                    self.add_constraint(term, Type.Bool)
            case _:
                print("Unknown node type: ", node.get('type'),  node)


if __name__ == "__main__":
    system = System(parse_data.get('ast'), parse_data.get('scope'))
    system.run_node(system.ast, None, [])
    system.show_constraints()
    system.solve()
    # S = DeclareSort('S')
    # f = Function('f', S, S)
    # g = Const('g', S)
    # x = Const('x', S)
    # solve(f == g)
