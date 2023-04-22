from HzzProlog.PrologCallable import define_parameterized_functor
from definitions.operators import time_point


class time_range(define_parameterized_functor("time_range")):
    def __init__(self, start: time_point, end: time_point):
        super().__init__(start, end)


# nuel tara
class booked_slot(define_parameterized_functor("booked_slot")):
    def __init__(self, npm: int, is_preferred: 1, _range: time_range):
        super().__init__(npm, is_preferred, _range)

