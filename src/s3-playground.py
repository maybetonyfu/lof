from z3 import *

Type = Datatype('Type')
Type.declare('Char')
Type.declare('Int')
Type.declare('Unit')
Type.declare('Float')
Type.declare('Fun', ('arg', Type), ('ret', Type))
Type = Type.create()

# GetLeft x y = x
# getLeft

apply = Function('apply', Type, Type, Type, BoolSort())
getleft, getright = Consts('getleft getright', Type)
f1, f2, f3, f4, f5 = Consts('f1 f2 f3 f4 f5', Type)
x, y, z, u, v, w = Consts('x y z u v w', Type)
id_func = Const('id', Type)
if __name__ == "__main__":
    s = Solver()
    s.add([
        ForAll([f1, x, y],
               Implies(
                   And(f1 == getleft, apply(f1, x, y)),
                   ForAll([f2, z, w],
                          Implies(
                              And(f2 == y, apply(f2, z, w)),
                              x == w
                          )
                          )
               )
        ),
        ForAll([f1, x, y],
               Implies(
                   And(f1 == getright, apply(f1, x, y)),
                   ForAll([f2, z, w],
                          Implies(
                              And(f2 == y, apply(f2, z, w)),
                              z == w
                          )
                          )
               )
               ),
        apply(getleft, Type.Int, x),
        apply(x, Type.Char, z),
        apply(getright, Type.Int, w),
        apply(w, Type.Char, u),

    ])
    print(s.check())
    print(s.model())
