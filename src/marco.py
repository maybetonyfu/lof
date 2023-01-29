from z3 import *
from typing import TypeAlias, NewType, Optional, Any, Callable
from haskell import Rule, Rid, System


# def ff(s, p):
#     return is_false(s.model().eval(p))
#
# def marco(s, ps):
#     map = Solver()
#     set_core_minimize(s)
#     while map.check() == sat:
#         seed = {p for p in ps if not ff(map, p)}
#         if s.check(seed) == sat:
#            mss = get_mss(s, seed, ps)
#            map.add(Or(ps - mss))
#            yield "MSS", mss
#         else:
#            mus = s.unsat_core()
#            map.add(Not(And(mus)))
#            yield "MUS", mus

class Marco:
    def __init__(self, rules: set[Rid], sat_fun: Callable[[set[Rid]], bool]):
        self.sat_solver = Solver()
        self.rules = rules
        self.map = [Bool(rid) for rid in self.rules]
        self.mus_list = []
        self.mss_list = []
        self.loop_counter = 0
        self.max_loops = 3
        self.model: Optional[ModelRef] = None
        self.sat_fun = sat_fun

    def grow(self, seed: set[Rid]) -> set[Rid]:
        for c in self.rules - seed:
            if self.sat(seed | {c}):
                seed = seed | {c}

        return seed

    def shrink(self, seed: set[Rid]) -> set[Rid]:
        for c in seed:
            if self.sat(seed - {c}):
                seed = seed - {c}

        return seed

    def get_unexplored(self, model: ModelRef) -> set[Rid]:
        seeds = []
        print(model)
        for rid in self.rules:
            assignment: BoolRef = model.eval(Bool(rid))
            seeds.append(assignment)
        return {ruleId for keep, ruleId in zip(seeds, self.rules) if keep}

    def is_satisfiable(self) -> (bool, Optional[ModelRef]):
        self.sat_solver.add(self.map)
        if self.sat_solver.check().r == 1:
            return True, self.sat_solver.model()
        else:
            return False, None

    def sat(self, rules: set[Rid]) -> bool:
        return self.sat_fun(rules)

    def run(self):
        successful, model = self.is_satisfiable()
        while successful:
            if self.loop_counter >= self.max_loops:
                raise Exception("Too many loops")
            self.loop_counter += 1

            seed = self.get_unexplored(model)
            if self.sat(seed):
                mss = self.grow(seed)
                self.mss_list.append(mss)
                self.map = self.map + [Or([Bool(r) for r in self.rules if r not in mss])]
            else:
                mus = self.shrink(seed)
                self.mus_list.append(mus)
                self.map = self.map + [Not(And([Bool(r) for r in mus]))]

            successful, model = self.is_satisfiable()


if __name__ == "__main__":
    system = System()
    system.type_check()
    system.show_rules()

    marco = Marco(
        rules={r.rid for r in system.rules},
        sat_fun=system.solve
    )
    marco.run()
