from HzzProlog.PrologCallable import define_parameterized_predicate
from definitions.operators import time_point


class time_range(define_parameterized_predicate("time_range")):
    def __init__(self, start: time_point, end: time_point):
        super().__init__(start, end)
