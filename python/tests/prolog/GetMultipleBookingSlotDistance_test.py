from itertools import permutations
from unittest import TestCase

from HzzProlog.HzzProlog import HzzProlog
from HzzProlog.test_util import remove_trailing_false_or_true, assert_prolog_output_the_same, \
    compare_list_just_like_a_set
from definitions.functors import time_range, booked_slot
from definitions.misc import define_tokenizer_regex
from definitions.operators import time_point
from definitions.paths import MAIN_PROLOG_PATH
from definitions.predicates import available, have_time, find_jadwal, get_booking_slot_distance, \
    get_multiple_booking_slot_distance
from definitions.variables import Result, dont_care
from tests.testing_utils.get_list_from_list_of_dicts import get_list_from_list_of_dicts

booked_slot_1 = booked_slot(
    dont_care, dont_care, time_range(
        time_point(1, 0, 0), time_point(2, 3, 4)
    ))
booked_slot_2 = booked_slot(
    dont_care, dont_care, time_range(
        time_point(3, 7, 1), time_point(3, 8, 5),
    ))
booked_slot_3 = booked_slot(
    dont_care, dont_care, time_range(
        time_point(5, 5, 8), dont_care
    ))

range_1 = (3-2)*24*60 + (7-3)*60 + (1 - 4)
range_2 = (5-3)*24*60 + (5-8)*60 + (8 - 5)



class TestGetMultipleBookingSlotDistance(TestCase):
    def setUp(self) -> None:
        self.prolog = HzzProlog(MAIN_PROLOG_PATH)
        define_tokenizer_regex(self.prolog)

    def test__should_return_time_differences_correctly(self):

        result = self.prolog.query(get_multiple_booking_slot_distance(
            [booked_slot_1, booked_slot_2, booked_slot_3], Result
        ))
        assert len(result) == 1
        self.assertListEqual([range_1, range_2], result[0][Result])

    def test__shouldnt_be_affected_by_the_given_order__1(self):

        result = self.prolog.query(get_multiple_booking_slot_distance(
            [booked_slot_3, booked_slot_1, booked_slot_2,], Result
        ))
        assert len(result) == 1
        self.assertListEqual([range_1, range_2], result[0][Result])

    def test__shouldnt_be_affected_by_the_given_order__2(self):

        result = self.prolog.query(get_multiple_booking_slot_distance(
            [booked_slot_2, booked_slot_3, booked_slot_1,], Result
        ), print_query=True)
        assert len(result) == 1
        self.assertListEqual([range_1, range_2], result[0][Result])

