from unittest import TestCase

from HzzProlog.HzzProlog import HzzProlog
from HzzProlog.test_util import remove_trailing_false_or_true
from definitions.functors import time_range, booked_slot
from definitions.misc import define_tokenizer_regex
from definitions.operators import time_point
from definitions.paths import FIND_JADWAL_PATH
from definitions.predicates import list_of_timeranges_inside_booked_slot
from definitions.variables import dont_care, Result


class TestListOfTimeRangesInsideBookedSlot(TestCase):
    def setUp(self) -> None:
        self.prolog = HzzProlog(FIND_JADWAL_PATH)
        define_tokenizer_regex(self.prolog)

    def test__should_return_empty_timerange_list_if_no_booked_slots(self):
        self.__test_if_it_returns_correctly_on_a_list_of_booked_slots([])

    def test__should_return_correct_timerange_list_if_there_is_one_booked_slots(self):
        self.__test_if_it_returns_correctly_on_a_list_of_booked_slots([
            time_range(time_point(1, 2, 3), time_point(4, 5, 6)),
        ])

    def test__should_return_correct_timerange_list_if_there_is_two_booked_slots(self):
        # we don't talk about collision/conflicts here
        self.__test_if_it_returns_correctly_on_a_list_of_booked_slots([
            time_range(time_point(1, 2, 3), time_point(4, 5, 6)),
            time_range(time_point(3, 3, 3), time_point(4, 4, 4)),
        ])

    def test__should_return_correct_timerange_list_if_there_is_three_booked_slots(self):
        self.__test_if_it_returns_correctly_on_a_list_of_booked_slots([
            time_range(time_point(1, 2, 3), time_point(4, 5, 6)),
            time_range(time_point(3, 3, 3), time_point(4, 4, 4)),
            time_range(time_point(5, 5, 5), time_point(5, 5, 5)),
        ])

    def test__should_return_correct_timerange_list_if_there_is_any_booked_slots(self):
        self.__test_if_it_returns_correctly_on_a_list_of_booked_slots([
            time_range(time_point(1, 1, 1), time_point(2, 2, 2)),
            time_range(time_point(3, 3, 3), time_point(4, 4, 4)),
            time_range(time_point(5, 5, 5), time_point(6, 6, 6)),
            time_range(time_point(7, 7, 7), time_point(8, 8, 8)),
        ])

    def __test_if_it_returns_correctly_on_a_list_of_booked_slots(self, time_ranges):
        list_of_booked_slots = [
            booked_slot(dont_care, dont_care, time_range_) for time_range_ in time_ranges
        ]
        result = self.prolog.query(list_of_timeranges_inside_booked_slot(
            list_of_booked_slots, Result
        ), print_query=True)
        result = remove_trailing_false_or_true(result)
        result = result[0]['Result']
        self.assertCountEqual(time_ranges, result)

