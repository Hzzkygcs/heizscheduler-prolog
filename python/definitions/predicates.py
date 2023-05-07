from HzzProlog.PrologCallable import define_parameterized_predicate, Variable
from definitions.functors import booked_slot, time_range
from definitions.operators import time_point

time = define_parameterized_predicate("time")
hari = define_parameterized_predicate("hari")
add_time = define_parameterized_predicate("add_time")
subtract_time = define_parameterized_predicate("subtract_time")
convert_from_minutes = define_parameterized_predicate("convert_from_minutes")


class have_time(define_parameterized_predicate('have_time')):
    def __init__(self, npm: int, is_preferred: int, timerange: time_range):
        super().__init__(npm, is_preferred, timerange)


class all_npm(define_parameterized_predicate("all_npm")):
    def __init__(self, npm: int):
        super().__init__(npm)


class available(define_parameterized_predicate("available")):
    def __init__(self, range_: time_range):
        super().__init__(range_)


time_in_range = define_parameterized_predicate("time_in_range")


class duration(define_parameterized_predicate("duration")):
    def __init__(self, input_time_range: time_range, resulting_duration: int):
        super().__init__(input_time_range, resulting_duration)


class find_jadwal(define_parameterized_predicate("find_jadwal")):
    def __init__(self, duration: int, booking_result: list[booked_slot]):
        super().__init__(duration, booking_result)


class less_than(define_parameterized_predicate("less_than")):
    def __init__(self, point1: time_point, point2: time_point):
        super().__init__(point1, point2)


class inside_another_timerange(define_parameterized_predicate("inside_another_timerange")):
    def __init__(self, smaller_timerange: time_range, bigger_timerange: time_range):
        super().__init__(smaller_timerange, bigger_timerange)


class check_if_they_have_time(define_parameterized_predicate("check_if_they_have_time")):
    def __init__(self, npm: int, time_range: time_range, is_preferred):
        super().__init__(npm, time_range, is_preferred)


class time_conflict(define_parameterized_predicate("time_conflict")):
    def __init__(self, range1: time_range, range2: time_range):
        super().__init__(range1, range2)


class time_conflict_list(define_parameterized_predicate("time_conflict_list")):
    def __init__(self, range1: time_range, range2: list[time_range]):
        super().__init__(range1, range2)


class bruteforce_timerange(define_parameterized_predicate("bruteforce_timerange")):
    def __init__(self, duration_: int, base: time_point, result: Variable):
        super().__init__(duration_,  base, result)


class list_of_timeranges_inside_booked_slot(define_parameterized_predicate("list_of_timeranges_inside_booked_slot")):
    def __init__(self, list_of_booked_slots: list[booked_slot], list_of_resulting_timeranges: list[time_range]):
        super().__init__(list_of_booked_slots, list_of_resulting_timeranges)


class list_of_timeranges_to_list_of_timepoints(define_parameterized_predicate("list_of_timeranges_to_list_of_timepoints")):
    def __init__(self, list_of_timeranges: list[time_range], list_of_timepoints: Variable):
        super().__init__(list_of_timeranges, list_of_timepoints)


class unique_call(define_parameterized_predicate("unique_call")):
    def __init__(self, template, goal, bag):
        super().__init__(template, goal, bag)


class count_preferred(define_parameterized_predicate("count_preferred")):
    def __init__(self, list_of_booked_slots, number_of_preferred):
        super().__init__(list_of_booked_slots, number_of_preferred)


class sort_booked_slots_by_starting_time(define_parameterized_predicate("sort_booked_slots_by_starting_time")):
    def __init__(self, list_of_booked_slots: list[booked_slot],
                 output_sorted_of_booked_slot: Variable[list[booked_slot]]):
        super().__init__(list_of_booked_slots, output_sorted_of_booked_slot)

class get_booking_slot_distance(define_parameterized_predicate("get_booking_slot_distance")):
    def __init__(self, booked_slot_1: booked_slot,
                 booked_slot_2: booked_slot,
                 result: Variable[int]):
        super().__init__(booked_slot_1, booked_slot_2, result)


class get_multiple_booking_slot_distance(define_parameterized_predicate("get_multiple_booking_slot_distance")):
    def __init__(self, booked_slots: list[booked_slot], result: Variable[int]):
        super().__init__(booked_slots, result)


class get_booking_slot_distance_penalties(define_parameterized_predicate("get_booking_slot_distance_penalties")):
    def __init__(self, booked_slots: list[booked_slot], result: Variable[list[int]]):
        super().__init__(booked_slots, result)
