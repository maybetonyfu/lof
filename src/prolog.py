from swiplserver import *
from typing import Optional
from contextlib import ContextDecorator
from pathlib import Path
from enum import Enum
from pydantic import BaseModel


# from devtools import debug

class Kind(Enum):
    Atom = "Atom"
    Var = "Var"
    Struct = "Struct"
    Array = "Array"


class Term(BaseModel):
    value: str | dict | list
    kind: Kind

    @classmethod
    def atom(cls, name: str):
        assert is_prolog_atom(name)
        return cls(value=name, kind=Kind.Atom)

    @classmethod
    def var(cls, name):
        assert is_prolog_variable(name)
        return cls(value=name, kind=Kind.Var)

    @classmethod
    def struct(cls, functor: str, *args: 'Term'):
        value = {'functor': functor, 'args': [arg.value for arg in args]}
        assert is_prolog_functor(value)
        return cls(value=value, kind=Kind.Struct)

    @classmethod
    def array(cls, *args: 'Term'):
        value = [arg.value for arg in args]
        assert is_prolog_list(value)
        return cls(value=value, kind=Kind.Array)

    def __str__(self):
        return json_to_prolog(self.value)

    def __repr__(self):
        return self.__str__()

    def __eq__(self, other: 'Term') -> 'Term':
        return unify(self, other)


var = Term.var
atom = Term.atom
struct = Term.struct
array = Term.array
succeed = atom('true')
fail = atom('false')


def cons(x: Term, xs: Term):
    return struct('[|]', x, xs)


def unify(a: Term, b: Term):
    return struct('=', a, b)


class Clause(BaseModel):
    head: Term
    body: list[Term]

    def __str__(self):
        head = self.head.__str__()
        body = ','.join([b.__str__() for b in self.body])
        if len(body) == 0:
            return head
        else:
            return f'{head} :- {body}'

    def __repr__(self):
        return self.__str__()


class PlInterface(Enum):
    File = 0
    Console = 1


class Prolog(ContextDecorator):
    def __init__(self, interface: PlInterface, file: Optional[str] = None):
        self.file: str | None = file
        self.prelude: str = (Path(__file__).parent.parent / 'prolog' / 'prelude.pl').as_posix()
        self.clauses: list[Clause] = []
        self.queries: list[Term] = []
        self.predicates: list[tuple[str, int]] = []
        self.interface: PlInterface = interface

    def __enter__(self):
        self.mqi = PrologMQI()
        self.prolog_thread: PrologThread = self.mqi.create_thread()
        return self

    def __exit__(self, *exc):
        self.prolog_thread.stop()
        self.mqi.stop()

    def generate_script(self) -> str:
        return '\n'.join([c.__str__() + '.' for c in self.clauses])

    def set_clauses(self, clauses: list[Clause]):
        self.clauses = clauses

    def add_clause(self, clause: Clause):
        self.clauses.append(clause)

    def add_clauses(self, clauses: list[Clause]):
        self.clauses += clauses

    def set_queries(self, qs: list[Term]):
        self.queries = qs

    def add_query(self, q: Term):
        self.queries.append(q)

    def add_queries(self, qs: list[Term]):
        self.queries += qs

    def run(self):
        if self.interface == PlInterface.File:
            return self.run_file()
        elif self.interface == PlInterface.Console:
            return self.run_console()

    def generate_asserts(self) -> list[str]:
        return [f'assert(({c}))' for c in self.clauses]

    def generate_abolishes(self) -> list[str]:
        abolishes = [f'abolish({predicate}/{arity})' for predicate, arity in self.predicates]
        self.predicates = []
        for c in self.clauses:
            value: dict = c.head.value
            predicate = value['functor']
            self.predicates.append((predicate, 2))
        return abolishes

    def run_file(self):
        with open(self.file, mode="w") as f:
            f.write(self.generate_script())
        # prelude = [f'assert(({prelude}))' for prelude in self.prelude]
        abolishes = self.generate_abolishes()
        consult_query = ','.join(['style_check(-singleton)'] +
                                 abolishes +
                                 [f"consult('{self.prelude}')"] +
                                 [f"consult('{self.file}')"] +
                                 [q.__str__() for q in self.queries])
        return self.prolog_thread.query(consult_query)

    def run_console(self):
        asserts = self.generate_asserts()
        abolishes = self.generate_abolishes()
        consult_query = ','.join(['style_check(-singleton)'] +
                                 abolishes +
                                 [f"consult('{self.prelude}')"]  +
                                 asserts +
                                 [q.__str__() for q in self.queries])

        return self.prolog_thread.query(consult_query)

    def run_raw_query(self, raw: str):
        return self.prolog_thread.query(raw)


if __name__ == "__main__":
    with Prolog(interface=PlInterface.Console) as prolog:
        path = (Path(__file__).parent.parent / 'prolog' / 'prelude.pl').as_posix()
        print(str(path))
        r = prolog.run_raw_query(f'''style_check(-discontiguous),consult('{path}'),member(X, [[a,b,c], [b | Xs]])''')
        # r = prolog.run()
        print(r)
