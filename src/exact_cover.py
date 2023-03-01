from collections import defaultdict, namedtuple
from devtools import debug
piece_to_constraints = defaultdict(set)

for i in range(9):
    for j in range(9):
        for digit in range(1, 10):
            piece_to_constraints[digit, i, j].add(("square", i, j))
            piece_to_constraints[digit, i, j].add(("line", i, digit))
            piece_to_constraints[digit, i, j].add(("column", j, digit))
            piece_to_constraints[digit, i, j].add(("area", i//3, j//3, digit))

Piece = namedtuple("Piece", "name constraints")

def convert(piece_to_constraints):
    """Take a dictionnary with iterable keys
    Return a dictionnary constraint_to_pieces containing sets of Piece
    """
    constraint_to_pieces = defaultdict(set)
    for piece_name, constraints in piece_to_constraints.items():
        piece = Piece(piece_name, tuple(constraints))
        for constraint in constraints:
            constraint_to_pieces[constraint].add(piece)
    return constraint_to_pieces

def select(constraint_to_pieces, constraints):
    """Suppose a list of constraints is fulfilled (for example because we added a piece).
    This implies that all pieces that have these constraints cannnot be used anymore so we remove them.
    Finally we remove the constraints from the dictionnary as well and return the pieces that were removed to be able to backtrack.
    """
    other_pieces = []
    for constraint in constraints:
        # this constraint is now fulfilled:
        # all pieces that have this constraint can be removed from the other constraints
        for piece in constraint_to_pieces[constraint]:
            for other_constraint in piece.constraints:
                if other_constraint != constraint:
                    constraint_to_pieces[other_constraint].remove(piece)
        # remove the constraint and store it for backtracking
        other_pieces.append(constraint_to_pieces.pop(constraint))
    return other_pieces

def deselect(constraint_to_pieces, constraints, other_pieces):
    for constraint in reversed(constraints):
        constraint_to_pieces[constraint] = other_pieces.pop()
        for other_piece in constraint_to_pieces[constraint]:
            for other_constraint in other_piece.constraints:
                if other_constraint != constraint:
                    constraint_to_pieces[other_constraint].add(other_piece)

def solve(constraint_to_pieces, solution=None):
    if solution is None:
        solution = []
    if not constraint_to_pieces:
        # make the solution a tuple so that it cannot be modified anymore
        yield tuple(solution)
        return

    # heuristic to minimize the branching factor
    constraint = min(constraint_to_pieces, key=lambda c: len(constraint_to_pieces[c]))
    for piece in list(constraint_to_pieces[constraint]):
        solution.append(piece.name)
        other_pieces = select(constraint_to_pieces, piece.constraints)
        for s in solve(constraint_to_pieces, solution):
            yield s
        deselect(constraint_to_pieces, piece.constraints, other_pieces)
        solution.pop()

if __name__ == "__main__":
    piece_to_constraints = {"A": {1}, "B": {2, 4}, "C": {2, 3, 5}, "D": {3, 5}}
    sol = list(solve(convert(piece_to_constraints)))
    debug(sol)