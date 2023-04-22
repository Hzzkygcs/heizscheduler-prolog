from HzzProlog.PrologCallable import define_parameterized_predicate, Variable
from definitions.functors import booked_slot, time_range
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


class available(define_parameterized_predicate("available")):
    def __init__(self, start: time_point, end: time_point):
        super().__init__(start, end)


time_in_range = define_parameterized_predicate("time_in_range")
duration = define_parameterized_predicate("duration")


class find_jadwal(define_parameterized_predicate("find_jadwal")):
    def __init__(self, duration: int, booking_result: list[booked_slot]):
        super().__init__(duration, booking_result)


class less_than(define_parameterized_predicate("less_than")):
    def __init__(self, point1: time_point, point2: time_point):
        super().__init__(point1, point2)


class inside_another_timerange(define_parameterized_predicate("inside_another_timerange")):
    def __init__(self, smaller_timerange: time_range, bigger_timerange: time_range):
        super().__init__(smaller_timerange, bigger_timerange)


class time_conflict(define_parameterized_predicate("time_conflict")):
    def __init__(self, range1: time_range, range2: time_range):
        super().__init__(range1, range2)


class time_conflict_list(define_parameterized_predicate("time_conflict_list")):
    def __init__(self, range1: time_range, range2: list[time_range]):
        super().__init__(range1, range2)


class bruteforce_timeranges(define_parameterized_predicate("bruteforce_timeranges")):
    def __init__(self, duration: int, result: Variable):
        super().__init__(duration, result)


class list_of_timeranges_inside_booked_slot(define_parameterized_predicate("list_of_timeranges_inside_booked_slot")):
    def __init__(self, list_of_booked_slots: list[booked_slot], list_of_resulting_timeranges: list[time_range]):
        super().__init__(list_of_booked_slots, list_of_resulting_timeranges)
