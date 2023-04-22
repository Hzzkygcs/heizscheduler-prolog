from unittest import TestCase

from HzzProlog.HzzProlog import HzzProlog
from HzzProlog.test_util import remove_trailing_false_or_true, assert_prolog_output_the_same
from definitions.functors import time_range
from definitions.misc import define_tokenizer_regex
from definitions.operators import time_point
from definitions.paths import MAIN_PROLOG_FILE_IO
from definitions.predicates import bruteforce_timerange
from definitions.variables import Result
from tests.list_utils import get_list_from_list_of_dicts
from tests.list_utils.get_list_from_list_of_dicts import get_list_from_list_of_dicts

HOUR = 60
DAY = 24*HOUR

class TestBruteForceTimeranges(TestCase):
    def setUp(self) -> None:
        self.prolog = HzzProlog(MAIN_PROLOG_FILE_IO)
        define_tokenizer_regex(self.prolog)

    def test_bruteforce_timeranges__should_try_bruteforce_forward_and_backward(self):
        base = time_point(5, 5, 5)
        result = self.prolog.query(bruteforce_timerange(
            1, base, Result
        ))
        result = remove_trailing_false_or_true(result)
        result = get_list_from_list_of_dicts(result, Result)
        assert_prolog_output_the_same(self, [
            time_range(time_point(5, 5, 4), time_point(5, 5, 5)),
            time_range(time_point(5, 5, 5), time_point(5, 5, 6)),
        ], result)

    def test_bruteforce_timeranges__should_try_bruteforce_forward_and_backward__other_duration(self):
        base = time_point(5, 5, 5)
        result = self.prolog.query(bruteforce_timerange(
            10, base, Result
        ))
        result = remove_trailing_false_or_true(result)
        result = get_list_from_list_of_dicts(result, Result)
        assert_prolog_output_the_same(self, [
            time_range(time_point(5, 4, 55), time_point(5, 5, 5)),
            time_range(time_point(5, 5, 5), time_point(5, 5, 15)),
        ], result)


    # def test_bruteforce_timeranges__should_return_correctly(self):
    #     self.prolog.add_facts("testing_definitions", [
    #         available(time_range(time_point(3, 2, 1), time_point(6, 5, 4)))
    #     ])
    #     result = self.prolog.query(bruteforce_timeranges(
    #         1, Result
    #     ))
    #     result = remove_trailing_false_or_true(result)
    #     result = get_list_from_list_of_dicts(result, Result)
    #     assert_prolog_output_the_same(self, [
    #         time_range(time_point(3, 2, 0), time_point(3, 2, 1)),
    #         time_range(time_point(3, 2, 1), time_point(3, 2, 2)),
    #
    #         time_range(time_point(6, 5, 3), time_point(6, 5, 4)),
    #         time_range(time_point(6, 5, 4), time_point(6, 5, 5)),
    #     ], result)
    #
    # def test_bruteforce_timeranges__should_return_correctly__duration_other_than_1(self):
    #     self.prolog.add_facts("testing_definitions", [
    #         available(time_range(time_point(3, 2, 1), time_point(6, 5, 4)))
    #     ])
    #     result = self.prolog.query(bruteforce_timeranges(
    #         3*DAY + 2*HOUR + 1, Result
    #     ))
    #     result = remove_trailing_false_or_true(result)
    #     result = get_list_from_list_of_dicts(result, Result)
    #     assert_prolog_output_the_same(self, [
    #         time_range(time_point(0, 0, 0), time_point(3, 2, 1)),
    #         time_range(time_point(3, 2, 1), time_point(6, 4, 2)),
    #
    #         time_range(time_point(3, 3, 3), time_point(6, 5, 4)),
    #         time_range(time_point(6, 5, 4), time_point(9, 7, 5)),
    #     ], result)
    #
    #
    # def test_bruteforce_timeranges__should_return_correctly__even_if_there_are_multiple_source(self):
    #     self.prolog.add_facts("testing_definitions", [
    #         available(time_range(time_point(1, 1, 30), time_point(2, 2, 30))),
    #         have_time(dont_care, dont_care, time_range(time_point(3, 3, 30), time_point(4, 4, 30))),
    #     ])
    #     result = self.prolog.query(bruteforce_timeranges(
    #         30, Result
    #     ))
    #     result = remove_trailing_false_or_true(result)
    #     result = get_list_from_list_of_dicts(result, Result)
    #     assert_prolog_output_the_same(self, [
    #         time_range(time_point(1, 1, 0), time_point(1, 1, 30)),
    #         time_range(time_point(1, 1, 30), time_point(1, 2, 0)),
    #
    #         time_range(time_point(2, 2, 0), time_point(2, 2, 30)),
    #         time_range(time_point(2, 2, 30), time_point(2, 3, 0)),
    #
    #         time_range(time_point(3, 3, 0), time_point(3, 3, 30)),
    #         time_range(time_point(3, 3, 30), time_point(3, 4, 0)),
    #
    #         time_range(time_point(4, 4, 0), time_point(4, 4, 30)),
    #         time_range(time_point(4, 4, 30), time_point(4, 5, 0)),
    #     ], result)
    #
    #



