from itertools import permutations
from unittest import TestCase

from HzzProlog.HzzProlog import HzzProlog
from HzzProlog.test_util import remove_trailing_false_or_true, assert_prolog_output_the_same, \
    compare_list_just_like_a_set
from definitions.functors import time_range, booked_slot
from definitions.misc import define_tokenizer_regex
from definitions.operators import time_point
from definitions.paths import MAIN_PROLOG_PATH, get_main_prolog
from definitions.predicates import available, have_time, find_jadwal, get_booking_slot_distance
from definitions.variables import Result, dont_care
from tests.testing_utils.get_list_from_list_of_dicts import get_list_from_list_of_dicts


class TestGetBookingSlotDistance(TestCase):
    def setUp(self) -> None:
        self.prolog = get_main_prolog()

    def test__should_return_time_difference_correctly(self):
        booked_slot_first = booked_slot(
            dont_care, dont_care, time_range(
                dont_care, time_point(2, 3, 4)
            ))
        booked_slot_second = booked_slot(
            dont_care, dont_care, time_range(
                time_point(3, 7, 1), dont_care
            ))

        result = self.prolog.query(get_booking_slot_distance(
            booked_slot_first, booked_slot_second, Result
        ))
        duration = (3-2)*24*60 + (7-3)*60 + (1 - 4)
        assert len(result) == 1
        self.assertEqual(duration, result[0][Result])

