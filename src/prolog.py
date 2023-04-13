from swiplserver import *
from contextlib import ContextDecorator
from pathlib import Path
from enum import Enum
from pydantic import BaseModel


class Kind(Enum):
    Atom = "Atom"
    Var = "Var"
    Struct = "Struct"
    Array = "Array"
    String = "String"
    StructExtern = "StructExtern"


class Term(BaseModel):
    value: str | dict | list
    kind: Kind

    @classmethod
    def string(cls, content: str):
        value = f"'{content}"
        return cls(value=value, kind=Kind.String)

    @classmethod
    def atom(cls, name: str):
        assert is_prolog_atom(name)
        return cls(value=name, kind=Kind.Atom)

    @classmethod
    def var(cls, name):
        assert is_prolog_variable(name)
        return cls(value=name, kind=Kind.Var)

    @classmethod
    def struct_extern(cls, module: str, functor: str, *args: 'Term'):
        # used for multifile prolog predicates with use_module/1, for example mod:pred(X, Y) :- ...
        value = {'module': module, 'functor': functor, 'args': [arg.value for arg in args]}
        assert is_prolog_functor(value)
        return cls(value=value, kind=Kind.StructExtern)

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
        if self.kind == Kind.String:
            return f"'{self.value}'"
        elif self.kind == Kind.StructExtern:
            return f"{self.value['module']}:{json_to_prolog(self.value)}"
        else:
            return json_to_prolog(self.value)

    def __repr__(self):
        return self.__str__()

    def __eq__(self, other: 'Term') -> 'Term':
        return unify(self, other)


var = Term.var
atom = Term.atom
struct = Term.struct
struct_extern = Term.struct_extern
array = Term.array
succeed = atom('true')
fail = atom('false')
nil = atom('nil')
wildcard = var('_')
cut = atom('!')


def cons(x: Term, xs: Term) -> Term:
    return struct('[|]', x, xs)


def unify(a: Term, b: Term):
    return struct('=', a, b)


def prolog_list_to_list(pl_list: dict | str) -> list[dict | str]:
    is_list = isinstance(pl_list, dict) and pl_list.get('functor') == '[|]'
    if is_list:
        head = pl_list['args'][0]
        tail = pl_list['args'][1]
        return [head] + prolog_list_to_list(tail)
    else:
        return [pl_list]


class Clause(BaseModel):
    head: Term
    body: list[Term]

    def __str__(self):
        head = self.head.__str__()
        body = ',\n  '.join([b.__str__() for b in self.body])
        if len(body) == 0:
            return head
        else:
            return f'{head} :-\n  {body}'

    def __repr__(self):
        return self.__str__()


class PlInterface(Enum):
    File = 0
    Console = 1


class Prolog(ContextDecorator):
    def __init__(self, interface: PlInterface, file: Path):
        self.file: Path = file
        self.builtin: Path = Path(__file__).parent.parent / 'prolog' / 'builtin.pl'
        self.clauses: list[Clause] = []
        self.queries: list[Term] = []
        self.predicates: list[tuple[str, int]] = []
        self.modules: list[str] = []
        self.multifiles: list[str] = []
        self.use_module: set[str] = set()
        self.interface: PlInterface = interface

    def reset(self):
        self.clauses = []
        self.queries = []
        self.predicates = []
        self.modules = []
        self.multifiles = []
        self.use_module = set()

    def __enter__(self):
        self.mqi = PrologMQI()
        self.prolog_thread: PrologThread = self.mqi.create_thread()
        return self

    def __exit__(self, *exc):
        self.prolog_thread.stop()
        self.mqi.stop()

    def generate_script(self) -> str:
        module_name = "user_" + self.file.stem
        pubs = []
        for c in self.clauses:
            value: dict = c.head.value
            predicate = value['functor']
            pubs.append(f'{predicate}/2')
        pub_string = ','.join(pubs)
        header = f""":- module({module_name}, [{pub_string}]).
:- reexport('{self.builtin.as_posix()}')."""
        imports = '\n'.join([f":- reexport('{m}')." for m in self.modules])
        multifile = ":- multifile "  + ",".join([f'{mf}' for mf in self.multifiles]) + "." if self.multifiles else ''
        use_module = '\n'.join([f":- use_module('{m}')." for m in self.use_module])
        clauses = '\n'.join([c.__str__() + '.' for c in self.clauses])
        return '\n'.join([header, imports, use_module, multifile, clauses])


    def add_clause(self, clause: Clause):
        self.clauses.append(clause)

    def add_use_module(self, module: Path):
        self.use_module.add(module.as_posix())

    def add_query(self, q: Term):
        self.queries.append(q)

    def add_multifile(self, multifile: str):
        self.multifiles.append(multifile)


    def set_modules(self, modules: list[str]):
        self.modules = modules

    def generate_file(self):
        with open(self.file, mode="w") as f:
            f.write(self.generate_script())

    def run_file(self) -> bool | list[bool | dict]:
        consult_query = ','.join(['style_check(-singleton)'] +
                                 [f"consult('{self.file.as_posix()}')"] +
                                 ['once((' + ','.join([q.__str__() for q in self.queries]) + '))']
                                 )
        return self.prolog_thread.query(consult_query)


    def run_raw_query(self, raw: str):
        self.prolog_thread.query_async(raw, find_all=False)
        result = self.prolog_thread.query_async_result()
        self.prolog_thread.cancel_query_async()
        return result


if __name__ == "__main__":
    with Prolog(interface=PlInterface.File, file=Path('./test.pl')) as prolog:
        r = prolog.run_raw_query(f'''member(a, X),member(b, X), X=[a]''')
        print(r)
