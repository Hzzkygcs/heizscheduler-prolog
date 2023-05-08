from unittest import TestCase

from HzzProlog.HzzProlog import HzzProlog
from HzzProlog.test_util import remove_trailing_false_or_true, assert_prolog_output_the_same
from definitions.functors import time_range
from definitions.misc import define_tokenizer_regex
from definitions.operators import time_point
from definitions.paths import MAIN_PROLOG_PATH
from definitions.predicates import bruteforce_timerange
from definitions.variables import Result
from tests.testing_utils import get_list_from_list_of_dicts
from tests.testing_utils.get_list_from_list_of_dicts import get_list_from_list_of_dicts

HOUR = 60
DAY = 24*HOUR

class TestBruteForceTimeranges(TestCase):
    def setUp(self) -> None:
        self.prolog = HzzProlog(MAIN_PROLOG_PATH)
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

