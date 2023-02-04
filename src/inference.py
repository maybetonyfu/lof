from z3 import *
from src.haskell_types import *

class Inference:
    def __init__(self, s: Solver, model: ModelRef, targets):
        for t in targets:
            is_fun = not is_false(
                model.eval(
                    apply(
                        t,
                        Const('y', Type),
                        Const('y', Type)
                    )
                )
            )
        pass