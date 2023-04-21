from unittest import TestCase

from HzzProlog.HzzProlog import HzzProlog
from HzzProlog.test_util import remove_trailing_false_or_true, assert_prolog_output_the_same
from definitions.functors import time_range
from definitions.misc import define_tokenizer_regex
from definitions.operators import time_point
from definitions.paths import MAIN_PROLOG_FILE_IO
from definitions.predicates import bruteforce_timeranges, available, have_time, find_jadwal
from definitions.variables import Result, dont_care
from tests.list_utils import get_list_from_list_of_dicts
from tests.list_utils.get_list_from_list_of_dicts import get_list_from_list_of_dicts

HOUR = 60
DAY = 24*HOUR

class TestFindJadwal(TestCase):    # TODO
    def setUp(self) -> None:
        self.prolog = HzzProlog(MAIN_PROLOG_FILE_IO)
        define_tokenizer_regex(self.prolog)

    def test__should_return_empty_when_no_facts_provided(self):
        result = self.prolog.query(find_jadwal(10, Result))
        result = remove_trailing_false_or_true(result)
        self.assertEqual([], result)

    def test__should_return_empty_when_the_only_fact_provided_is_available(self):
        self.prolog.add_facts('testing_definitions', [
            available(time_point(dont_care, dont_care, dont_care),
                      time_point(dont_care, dont_care, dont_care)),
        ])
        result = self.prolog.query(find_jadwal(10, Result))
        result = remove_trailing_false_or_true(result)
        self.assertEqual([], result)


    def test__should_return_empty_when_the_only_fact_provided_is_have_time(self):
        self.prolog.add_facts('testing_definitions', [
            have_time(dont_care, dont_care,
                      time_point(dont_care, dont_care, dont_care),
                      time_point(dont_care, dont_care, dont_care)),
        ])
        result = self.prolog.query(find_jadwal(10, Result))
        result = remove_trailing_false_or_true(result)
        self.assertEqual([], result)


    def test__should_return_empty_when_available_and_haveTime_do_not_intersect(self):
        self.prolog.add_facts('testing_definitions', [
            available(time_point(1, 0, 0),
                      time_point(1, 1, 0)),
            have_time(dont_care, dont_care,
                      time_point(2, 0, 0),
                      time_point(2, 1, 0)),
        ])
        result = self.prolog.query(find_jadwal(10, Result))
        result = remove_trailing_false_or_true(result)
        self.assertEqual([], result)

    def test__should_return_empty_when_available_and_haveTime_intersects_but_the_duration_provided_cannot_fit_into_it(self):
        duration = 10
        intersect_width = duration - 1
        assert intersect_width < duration   # for the purpose of this test case
        self.prolog.add_facts('testing_definitions', [
            available(time_point(1, 0, 0),
                      time_point(1, 0, 59)),
            have_time(dont_care, dont_care,
                      time_point(1, 0, 59 - intersect_width),
                      time_point(2, 0, 0)),
        ])
        result = self.prolog.query(find_jadwal(duration, Result))
        result = remove_trailing_false_or_true(result)
        self.assertEqual([], result)



