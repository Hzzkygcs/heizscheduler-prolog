from token import LESS
from unittest import TestCase

from HzzProlog.HzzProlog import HzzProlog
from HzzProlog.test_util import remove_trailing_false_or_true
from definitions.functors import time_range, booked_slot
from definitions.misc import define_tokenizer_regex
from definitions.operators import time_point
from definitions.paths import MAIN_PROLOG_PATH
from definitions.predicates import list_of_timeranges_inside_booked_slot, get_booking_slot_distance_penalties
from definitions.variables import dont_care, Result
from tests.testing_utils.incremental_get_booked_slot import BookedSlotGenerator

MINUTE_DIFFERENCE_TO_CAUSE_FOUR_PENALTY = 23
MINUTE_DIFFERENCE_TO_CAUSE_THREE_PENALTY = 44
MINUTE_DIFFERENCE_TO_CAUSE_TWO_PENALTY = 59
MINUTE_DIFFERENCE_TO_CAUSE_ONE_PENALTY = 119
MINUTE_DIFFERENCE_NO_PENALTY_MORE_THAN_120 = 121
MINUTE_DIFFERENCE_NO_PENALTY_LESS_THAN_10 = 9


class TestGetBookingSlotDistancePenalties(TestCase):
    def setUp(self) -> None:
        self.prolog = HzzProlog(MAIN_PROLOG_PATH)
        define_tokenizer_regex(self.prolog)
        self.booked_slot_generator = BookedSlotGenerator()

    def test__should_return_empty_list_for_empty_list(self):
        result = self.prolog.query(get_booking_slot_distance_penalties(
            [], Result
        ))
        result = get(self, result, Result)
        self.assertEqual([], result)


    def test__should_return_empty_list_for_one_item(self):
        result = self.prolog.query(get_booking_slot_distance_penalties(
            [self.booked_slot_generator.get_booked_slot(10, 5)], Result
        ), print_query=True)
        result = get(self, result, Result)
        self.assertEqual([], result)

    def test__two_item__zero_penalty(self):
        result = self.prolog.query(get_booking_slot_distance_penalties(
            [self.booked_slot_generator.get_booked_slot(10, 5),
             self.booked_slot_generator.get_booked_slot(MINUTE_DIFFERENCE_NO_PENALTY_LESS_THAN_10, 5)], Result
        ))
        result = get(self, result, Result)
        self.assertEqual([0], result)

    def test_two_item__4_penalty(self):
        result = self.prolog.query(get_booking_slot_distance_penalties(
            [self.booked_slot_generator.get_booked_slot(10, 5),
             self.booked_slot_generator.get_booked_slot(MINUTE_DIFFERENCE_TO_CAUSE_FOUR_PENALTY, 5)], Result
        ))
        result = get(self, result, Result)
        self.assertEqual([4], result)

    def test__two_item__3_penalty(self):
        result = self.prolog.query(get_booking_slot_distance_penalties(
            [self.booked_slot_generator.get_booked_slot(10, 5),
             self.booked_slot_generator.get_booked_slot(MINUTE_DIFFERENCE_TO_CAUSE_THREE_PENALTY, 5)], Result
        ))
        result = get(self, result, Result)
        self.assertEqual([3], result)

    def test__two_item__2_penalty(self):
        result = self.prolog.query(get_booking_slot_distance_penalties(
            [self.booked_slot_generator.get_booked_slot(10, 5),
             self.booked_slot_generator.get_booked_slot(MINUTE_DIFFERENCE_TO_CAUSE_TWO_PENALTY, 5)], Result
        ))
        result = get(self, result, Result)
        self.assertEqual([2], result)

    def test__two_item__1_penalty(self):
        result = self.prolog.query(get_booking_slot_distance_penalties(
            [self.booked_slot_generator.get_booked_slot(10, 5),
             self.booked_slot_generator.get_booked_slot(MINUTE_DIFFERENCE_TO_CAUSE_ONE_PENALTY, 5)], Result
        ))
        result = get(self, result, Result)
        self.assertEqual([1], result)

    def test__two_item__zero_penalty__more_than_two_hour(self):
        result = self.prolog.query(get_booking_slot_distance_penalties(
            [self.booked_slot_generator.get_booked_slot(10, 5),
             self.booked_slot_generator.get_booked_slot(MINUTE_DIFFERENCE_NO_PENALTY_MORE_THAN_120, 5)], Result
        ))
        result = get(self, result, Result)
        self.assertEqual([0], result)

    def test__multiple_item(self):
        result = self.prolog.query(get_booking_slot_distance_penalties(
            [self.booked_slot_generator.get_booked_slot(10, 5),
             self.booked_slot_generator.get_booked_slot(MINUTE_DIFFERENCE_TO_CAUSE_FOUR_PENALTY, 10),
             self.booked_slot_generator.get_booked_slot(MINUTE_DIFFERENCE_NO_PENALTY_MORE_THAN_120, 10),
             self.booked_slot_generator.get_booked_slot(MINUTE_DIFFERENCE_TO_CAUSE_ONE_PENALTY, 10),
             self.booked_slot_generator.get_booked_slot(MINUTE_DIFFERENCE_TO_CAUSE_THREE_PENALTY, 10),
             self.booked_slot_generator.get_booked_slot(MINUTE_DIFFERENCE_NO_PENALTY_LESS_THAN_10, 10),
             self.booked_slot_generator.get_booked_slot(MINUTE_DIFFERENCE_TO_CAUSE_TWO_PENALTY, 10),
             ], Result
        ))
        result = get(self, result, Result)
        self.assertListEqual([4, 0, 1, 3, 0, 2], result)



def get(self, lst, key):
    self.assertEqual(1, len(lst), "result is not exactly one")
    self.assertNotEqual("false", len(lst), "result is boolea false")
    return lst[0][key]

