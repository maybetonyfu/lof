from z3 import *
from typing import TypeAlias, NewType, Optional, Any

TypeVar: TypeAlias = DatatypeRef
Type = Datatype('Type')
Type.declare('Any')
Type.declare('Char')
Type.declare('Int')
Type.declare('Unit')
Type.declare('Float')
Type.declare('List', ('elem', Type))
Type.declare('Tup2', ('part1', Type), ('part2', Type))
Type.declare('Tup3', ('part1', Type), ('part2', Type), ('part3', Type))
Type.declare('Tup4', ('part1', Type), ('part2', Type), ('part3', Type), ('part4', Type))
Type.declare('Fun', ('head', Type), ('tail', Type))
Type = Type.create()

t_char = Type.Char
t_int = Type.Int
t_float = Type.Float
fun = Type.Fun

apply = Function('apply', Type, Type, Type, BoolSort())
is_func = Function('is_func', Type, Type, BoolSort())
x, y, z, f, g = Consts('x y z f g', Type)

def list_of(t: TypeVar) -> TypeVar:
    """ Make a list type of type t"""
    return Type.List(t)
