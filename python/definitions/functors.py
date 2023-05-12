from typing import Union

from HzzProlog.PrologCallable import define_parameterized_functor, Variable
from definitions.operators import time_point


class time_range(define_parameterized_functor("time_range")):
    def __init__(self, start: time_point, end: time_point):
        super().__init__(start, end)


# nuel tara
class booked_slot(define_parameterized_functor("booked_slot")):
    def __init__(self, npm: Union[int, Variable[int]], is_preferred: 1, _range: time_range):
        super().__init__(npm, is_preferred, _range)


class penalty_and_slots(define_parameterized_functor("penalty_and_slots")):
    def __init__(self, penalty: Union[int, Variable[int]], slot: booked_slot):
        super().__init__(penalty, slot)
