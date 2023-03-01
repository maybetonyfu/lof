from typing import TypeVar, TypeAlias, Generic
from pydantic import BaseModel

T = TypeVar('T')


class Maybe(Generic[T], BaseModel):
    is_nothing: bool
    is_just: bool
    value: T | None

    @classmethod
    def nothing(cls):
        return cls(is_nothing=True, is_just=False, value=None)

    @classmethod
    def just(cls, value: T):
        return cls(is_nothing=False, is_just=True, value=value)


nothing = Maybe.nothing()
just = Maybe.just
