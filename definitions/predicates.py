from HzzProlog.PrologCallable import define_parameterized_predicate, PrologList
from definitions.functors import booked_slot
from definitions.operators import datetime, time_point

time = define_parameterized_predicate("time")
hari = define_parameterized_predicate("hari")
add_time = define_parameterized_predicate("add_time")
subtract_time = define_parameterized_predicate("subtract_time")
convert_from_minutes = define_parameterized_predicate("convert_from_minutes")


class have_time(define_parameterized_predicate('have_time')):
    def __init__(self, npm: int, is_preferred: int, start: datetime, end: datetime):
        super().__init__(npm, is_preferred, start, end)


class all_npm(define_parameterized_predicate("all_npm")):
    def __init__(self, npm: int):
        super().__init__(npm)


available = define_parameterized_predicate("available")
time_in_range = define_parameterized_predicate("time_in_range")
duration = define_parameterized_predicate("duration")


class find_jadwal(define_parameterized_predicate("find_jadwal")):
    def __init__(self,
                 time_points: list[time_point],
                 booked_slots: list[booked_slot],
                 npm_not_yet_book: list[int]):
        super().__init__(time_points, booked_slots, npm_not_yet_book)
