"""
metaopt implements a unified interface for running different optimizers.
"""
import numpy as np
import modcma

default_targets = np.power(10, np.linspace(2, -8, 51))

def run_modcma(problem, fopt, targets, *args, **kwargs):
    """
    Runs the modcma algorithm with the specified args,
    returning hitting times (if any) for given targets.
    """
    fevals = 0

    next_target = max(targets)

    hits = np.repeat(np.inf, len(targets))
    unsolved_targets = sum(hits == np.inf)

    cma = modcma.AskTellCMAES(*args, **kwargs)

    while (not any(cma.break_conditions) and
           not any(cma.parameters.termination_criteria.values()) and
           unsolved_targets > 0):
        fevals += 1

        # Retrieve a single new candidate solution
        xi = cma.ask()

        # Evaluate the objective function
        fi = problem(np.transpose(xi)[0])
        # Update the algorithm with the objective function value
        cma.tell(xi, fi)

        gap = fi - fopt

        if gap < next_target:
            ts = np.logical_and(gap <= targets, hits < np.inf)
            hits[ts] = fevals

            unsolved_targets = sum(hits == np.inf)
            if unsolved_targets > 0:
                next_target = max(hits == np.inf)
                print(next_target)

    return hits
