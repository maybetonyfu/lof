from z3 import *
from typing import Optional, Callable
import networkx
from pydantic import BaseModel
from itertools import product, combinations


class RuleSet(BaseModel):
    rules: set[int]
    setId: int


class Error(BaseModel):
    error_id: int
    mus_list: list[RuleSet]
    mcs_list: list[RuleSet]
    mss_list: list[RuleSet]


class Marco:
    def __init__(self, rules: set[int], sat_fun: Callable[[set[int]], bool], parent_relations: list[tuple[int, int]]):
        self.rules = frozenset(rules)
        self.graph = networkx.Graph()
        self.mus_list: set[frozenset[int]] = set()
        self.mss_list: set[frozenset[int]] = set()
        self.mcs_list: set[frozenset[int]] = set()
        self.tc_errors: list[Error] = []
        self.parent_relations: list[tuple[int, int]] = parent_relations
        self.solver = Solver()
        self.loop_counter = 0
        self.sat_counter = 0
        self.max_loops = 999
        self.sat_fun = sat_fun

    def grow(self, seed: frozenset[int]) -> frozenset[int]:
        for c in self.rules - seed:
            if self.sat(seed | {c}):
                seed = seed | {c}

        return seed

    def shrink(self, seed: frozenset[int]) -> frozenset[int]:
        for c in seed:
            if not self.sat(seed - {c}):
                seed = seed - {c}

        return seed

    def get_other_msses(self, mss: frozenset[int]) -> set[frozenset[int]]:
        # if relation holds: parent_child(p1, p2), and P - ({p2} + C) is MSS, and pi
        # then P - ({p1) + C) + {pi . parent_child(p1, pi}
        alternatives = []
        mcs = self.rules - mss
        for rule in mcs:
            replacers = [rule]
            for parent, child in self.parent_relations:
                if child == rule:
                    replacers.append(parent)
            alternatives.append(replacers)

        possible_mixes = product(*alternatives)
        combination_sets = {frozenset(combination) for combination in possible_mixes}
        remove_parent_child = set()
        for possible_mcs in combination_sets:
            removed = set(possible_mcs)
            for comb in combinations(possible_mcs, 2):
                if (comb[0], comb[1]) in self.parent_relations:
                    removed.remove(comb[1])
                elif (comb[1], comb[0]) in self.parent_relations:
                    removed.remove(comb[0])
            remove_parent_child.add(frozenset(removed))
        # print('for set ', mcs, ' \nthe muses are generated: ', remove_parent_child - {mcs} )

        # return [set(s) for s in combination_sets if s != frozenset(mus)]
        return {self.rules - new_mcs for new_mcs in remove_parent_child}
    def get_unexplored(self, model: ModelRef) -> frozenset[int]:
        seeds = []
        for rid in self.rules:
            assignment: bool = not is_false(model.eval(Bool(rid)))
            seeds.append(assignment)
        # print(seeds)
        return frozenset({ruleId for keep, ruleId in zip(seeds, self.rules) if keep})

    def is_satisfiable(self) -> (bool, Optional[ModelRef]):
        if self.solver.check().r == 1:
            return True, self.solver.model()
        else:
            return False, None

    def sat(self, rules: frozenset[int]) -> bool:
        self.sat_counter += 1
        return self.sat_fun(set(rules))

    def run(self):
        successful, model = self.is_satisfiable()
        while successful:
            if self.loop_counter >= self.max_loops:
                raise Exception("Too many loops")
            self.loop_counter += 1

            seed = self.get_unexplored(model)
            if self.sat(seed):
                mss = self.grow(seed)
                other_msses = self.get_other_msses(mss)

                self.mss_list.add(mss)
                self.solver.add(Or([Bool(r) for r in self.rules if r not in mss]))

                for other_mss in other_msses:
                    self.mss_list.add(other_mss)
                    self.solver.add(Or([Bool(r) for r in self.rules if r not in other_mss]))


            else:
                mus = self.shrink(seed)
                self.mus_list.add(frozenset(mus))
                self.solver.add(Not(And([Bool(r) for r in mus])))

            successful, model = self.is_satisfiable()
        print('finished after ', self.sat_counter, ' runs')

    def analyse(self):
        mcs_counter = 0
        # Populate mcs list
        for mss in self.mss_list:
            self.mcs_list.add(self.rules - mss)

        mus_index_list = list(enumerate(self.mus_list))
        self.graph.add_nodes_from([i for i, mus in mus_index_list])

        for combination in combinations(mus_index_list, 2):
            index1, mus1 = combination[0]
            index2, mus2 = combination[1]
            if mus1 & mus2 != set():
                self.graph.add_edge(index1, index2)

        for i, component in enumerate(networkx.connected_components(self.graph)):
            mus_list = [mus_index_list[musId][1] for musId in component]
            mcs_list = []
            mss_list = []
            all_mus_rules: set[int] = set().union(*[mus for mus in mus_list])
            reduced_mcses = [RuleSet(setId=mcsId, rules=mcs & all_mus_rules) for mcsId, mcs in enumerate(self.mcs_list)]
            non_empty_mcses = [mcs for mcs in reduced_mcses if len(mcs.rules) != 0]
            seen = []
            for mcs in non_empty_mcses:
                if mcs.rules in seen:
                    continue
                else:
                    seen.append(mcs.rules)
                    mcs_list.append(mcs)
                    mss_list.append([RuleSet(setId=mssId, rules=mss) for mssId, mss in enumerate(self.mss_list) if mssId == mcs.setId][0])

            mus_ruleset = [RuleSet(setId=musId, rules=set(mus)) for musId, mus in enumerate(mus_list)]
            self.tc_errors.append(Error(error_id=i, mus_list=mus_ruleset, mcs_list=mcs_list, mss_list=mss_list))

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



