from unittest import TestCase

from HzzProlog.HzzProlog import HzzProlog
from HzzProlog.PrologCallable import define_parameterized_predicate
from definitions.functors import booked_slot, time_range
from definitions.misc import define_tokenizer_regex
from definitions.operators import time_point
from definitions.paths import MAIN_PROLOG_PATH, get_main_prolog
from definitions.predicates import sort_booked_slots_by_starting_time
from definitions.variables import Result, dont_care
from tests.testing_utils.get_list_from_list_of_dicts import get_list_from_list_of_dicts

fact = define_parameterized_predicate("fact")

booked_slot_1 = booked_slot(
    dont_care, dont_care, time_range(
        time_point(1, 2, 3), dont_care
    ))
booked_slot_2 = booked_slot(
    dont_care, dont_care, time_range(
        time_point(1, 2, 4), dont_care
    ))
booked_slot_3 = booked_slot(
    dont_care, dont_care, time_range(
        time_point(1, 3, 4), dont_care
    ))
booked_slot_4 = booked_slot(
    dont_care, dont_care, time_range(
        time_point(2, 3, 4), dont_care
    ))


class TestSortBookedSlotsByStartingTime(TestCase):
    def setUp(self) -> None:
        self.prolog = get_main_prolog()

    def test__should_be_able_to_handle_empty_list(self):
        result = self.prolog.query(sort_booked_slots_by_starting_time(
            [], Result
        ))
        result = get_list_from_list_of_dicts(result, Result)
        assert len(result) == 1
        result = result[0]
        self.assertCountEqual([], result)

    def test__should_be_able_to_handle_single_list(self):
        result = self.prolog.query(sort_booked_slots_by_starting_time(
            [booked_slot_1], Result
        ), print_query=True)
        result = get_list_from_list_of_dicts(result, Result)
        assert len(result) == 1
        result = result[0]
        self.assertCountEqual([booked_slot_1], result)

    def test__should_be_able_to_handle_double_list__1(self):
        result = self.prolog.query(sort_booked_slots_by_starting_time(
            [booked_slot_1, booked_slot_2], Result
        ))
        result = get_list_from_list_of_dicts(result, Result)
        assert len(result) == 1
        result = result[0]
        self.assertCountEqual([booked_slot_1, booked_slot_2], result)

    def test__should_be_able_to_handle_double_list__2(self):
        result = self.prolog.query(sort_booked_slots_by_starting_time(
            [booked_slot_2, booked_slot_1], Result
        ))
        result = get_list_from_list_of_dicts(result, Result)
        assert len(result) == 1
        result = result[0]
        self.assertCountEqual([booked_slot_1, booked_slot_2], result)

    def test__should_be_able_to_handle_multiple_item_list(self):
        result = self.prolog.query(sort_booked_slots_by_starting_time(
            [booked_slot_2, booked_slot_1, booked_slot_4, booked_slot_3], Result
        ))
        result = get_list_from_list_of_dicts(result, Result)
        assert len(result) == 1
        result = result[0]
        self.assertCountEqual([booked_slot_1, booked_slot_2,
                               booked_slot_3, booked_slot_4], result)

