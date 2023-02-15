from z3 import *
from typing import Optional, Callable
import networkx
from pydantic import BaseModel
from devtools import debug


class RuleSet(BaseModel):
    rules: set[int]
    setId: int


class TCError(BaseModel):
    error_id: int
    mus_list: list[RuleSet]
    mcs_list: list[RuleSet]
    mss_list: list[RuleSet]


class Marco:
    def __init__(self, rules: set[int], sat_fun: Callable[[set[int]], bool]):
        self.rules = rules
        self.graph = networkx.Graph()
        self.mus_list: list[RuleSet] = []
        self.mss_list: list[RuleSet] = []
        self.mcs_list: list[RuleSet] = []
        self.tc_errors: list[TCError] = []
        self.solver = Solver()
        self.loop_counter = 0
        self.max_loops = 999
        self.sat_fun = sat_fun

    def grow(self, seed: set[int]) -> set[int]:
        for c in self.rules - seed:
            if self.sat(seed | {c}):
                seed = seed | {c}

        return seed

    def shrink(self, seed: set[int]) -> set[int]:
        for c in seed:
            if not self.sat(seed - {c}):
                seed = seed - {c}

        return seed

    def get_unexplored(self, model: ModelRef) -> set[int]:
        seeds = []
        for rid in self.rules:
            assignment: bool = not is_false(model.eval(Bool(rid)))
            seeds.append(assignment)
        # print(seeds)
        return {ruleId for keep, ruleId in zip(seeds, self.rules) if keep}

    def is_satisfiable(self) -> (bool, Optional[ModelRef]):

        if self.solver.check().r == 1:
            return True, self.solver.model()
        else:
            return False, None

    def sat(self, rules: set[int]) -> bool:
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
                self.mss_list.append(RuleSet(rules=mss, setId=len(self.mss_list)))
                self.solver.add(Or([Bool(r) for r in self.rules if r not in mss]))
            else:
                mus = self.shrink(seed)
                self.mus_list.append(RuleSet(rules=mus, setId=len(self.mus_list)))
                self.solver.add(Not(And([Bool(r) for r in mus])))

            successful, model = self.is_satisfiable()

    def analyse(self):
        mcs_counter = 0
        # Populate mcs list
        for mss in self.mss_list:
            self.mcs_list.append(RuleSet(rules=self.rules - mss.rules, setId=mss.setId))

        self.graph.add_nodes_from([mus.setId for mus in self.mus_list])

        for mus1 in self.mus_list:
            for mus2 in self.mus_list:
                if mus1.setId == mus2.setId:
                    continue
                elif mus1.rules & mus2.rules == set():
                    continue
                else:
                    self.graph.add_edge(mus1.setId, mus2.setId)

        for i, component in enumerate(networkx.connected_components(self.graph)):
            mus_list = [mus for mus in self.mus_list if mus.setId in component]
            mcs_list = []
            mss_list = []
            all_mus_rules: set[int] = set().union(*[mus.rules for mus in mus_list])
            reduced_mcses = [RuleSet(setId=mcs.setId, rules=mcs.rules & all_mus_rules) for mcs in self.mcs_list]
            non_empty_mcses = [mcs for mcs in reduced_mcses if len(mcs.rules) != 0]
            seen = []
            for mcs in non_empty_mcses:
                if mcs.rules in seen:
                    continue
                else:
                    seen.append(mcs.rules)
                    mcs_list.append(mcs)
                    mss_list.append([mss for mss in self.mss_list if mss.setId == mcs.setId][0])

            self.tc_errors.append(TCError(error_id=i,mus_list=mus_list, mcs_list=mcs_list, mss_list=mss_list))

    def show(self):
        print(f"Process finished after {self.loop_counter} iterations")
        print(f"{len(self.tc_errors)} islands found in the code")
        for island in self.tc_errors:
            print(f"island:")
            print(f"\nMUSs:")
            for mus in island.mus_list:
                print(mus)

            print(f"\nMCSs:")
            for mcs in island.mcs_list:
                print(mcs)
            print("\n\n")


if __name__ == "__main__":
    from haskell import System
    from pathlib import Path

    system = System(code_dir=str(Path(__file__).parent.parent / "example"))
    system.type_check()

    marco = Marco(
        rules={r.rid for r in system.rules},
        sat_fun=system.solve
    )
    marco.run()
    marco.analyse()
    marco.show()
