from z3 import *

Type = Datatype('Type')
Type.declare('Char')
Type.declare('Int')
Type.declare('Unit')
Type.declare('Float')
Type.declare('Function', ('head', Type), ('tail', Type))
Type = Type.create()

t_char = Type.Char
t_int = Type.Int
t_unit = Type.Unit
t_float = Type.Float
fun = Type.Function

getleft, getleft_1 = Consts('getleft getleft_1', Type)
f1, f2, f3, f4, f5 = Consts('f1 f2 f3 f4 f5', Type)
x, y, z, u, v, w = Consts('x y z u v w', Type)
a, b, c, d, e, f = Consts('a b c d e f', Type)

if __name__ == "__main__":
    s = Solver()
    is_func = Function('is_func', Type, Type, BoolSort())
    id_func = Const('id_func', Type)
    id_def = ForAll(
        [a],
        Implies(
            is_func(a, id_func),
            ForAll([b, c], Implies(a == fun(b, c), b == c))
        ))

    getleft_def = ForAll(
        [a],
        Implies(
            is_func(a, getleft),
            ForAll([b, c], Implies(a == fun(b, c),
                                   And(
                                       c == fun(Const("fa", Type), Const("fb", Type)),
                                       ForAll(d,
                                              Implies(is_func(d, c),
                                                      ForAll([e, f], Implies(d == fun(e, f), f == b))
                                                      )
                                              )

                                   )
                    ))
        )
    )
    #
    # s.add(id_def)
    s.add([
        id_def,
        #
        # ForAll(f, Implies(
        #     is_func(f, id_func),
        #     Or(f == x, f == y)
        #
        # )),
        # ForAll(f, Implies(
        #     is_func(f, getleft),
        #     Or(f == z)
        # )),
        ForAll(f, Not(Or(
            is_func(t_int, f),
            is_func(t_float, f),
            is_func(t_char, f),
            is_func(t_unit, f),

        ))),
        is_func(x, id_func),
        is_func(y, id_func),
        #
        x == fun(t_int, t_int),
        y == fun(t_char, t_char),
        getleft_def,
        is_func(z, getleft),

        # d == z,
        z == fun(t_char, a),
        is_func(d, a),
        d == fun(t_int, c),
        is_func(e, a),
        e == fun(t_float, f),
        getleft == fun(u, fun(v, w))
    ])

    # s.add([
    #     getleft_def,
    #
    #     # z == fun(t_float, fun(u, t_int))
    # ])

    print(s.check())

    if s.check().r == 1:
        m = s.model()
        # print(m)
        # print(m.eval(is_func(y, id_func)))
        for d in m:
            if d.name() not in ['is_func', 'head', 'tail']:
                print(d, '::', m[d])
