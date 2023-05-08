from unittest import TestCase

from HzzProlog.HzzProlog import HzzProlog
from definitions.functors import time_range
from definitions.misc import define_tokenizer_regex
from definitions.operators import time_point
from definitions.paths import HZZ_TIME_PL_PATH
from definitions.predicates import inside_another_timerange


def extract_bool(result):
    if len(result) > 1:
        return True
    if len(result) == 0:
        return False
    return result[0] == 'true'


class TestInsideAnotherTimerange(TestCase):
    def setUp(self) -> None:
        self.prolog = HzzProlog(HZZ_TIME_PL_PATH)
        define_tokenizer_regex(self.prolog)

    def test__should_be_true_if_both_are_exactly_the_same(self):
        result = self.prolog.query(inside_another_timerange(
            time_range(time_point(0, 0, 0), time_point(2, 0, 0)),
            time_range(time_point(0, 0, 0), time_point(2, 0, 0)),
        ))
        self.assertTrue(extract_bool(result))

    def test__should_be_false_if_they_only_intersects_despite_of_having_the_same_width(self):
        result = self.prolog.query(inside_another_timerange(
            time_range(time_point(0, 0, 0), time_point(2, 0, 0)),
            time_range(time_point(0, 0, 1), time_point(2, 0, 1)),
        ))
        self.assertFalse(extract_bool(result))

    def test__should_be_false_if_they_dont_intersect_at_all(self):
        result = self.prolog.query(inside_another_timerange(
            time_range(time_point(0, 0, 0), time_point(2, 0, 0)),
            time_range(time_point(4, 0, 0), time_point(6, 0, 0)),
        ), print_query=True)
        self.assertFalse(extract_bool(result))

    def test__should_be_true_if_the_first_is_inside_the_second__day(self):
        result = self.prolog.query(inside_another_timerange(
            time_range(time_point(1, 0, 0), time_point(2, 0, 0)),
            time_range(time_point(0, 0, 0), time_point(3, 0, 0)),
        ))
        self.assertTrue(extract_bool(result))

    def test__should_be_true_if_the_first_is_inside_the_second__hour(self):
        result = self.prolog.query(inside_another_timerange(
            time_range(time_point(0, 1, 0), time_point(0, 2, 0)),
            time_range(time_point(0, 0, 0), time_point(0, 3, 0)),
        ))
        self.assertTrue(extract_bool(result))

    def test__should_be_true_if_the_first_is_inside_the_second__minute(self):
        result = self.prolog.query(inside_another_timerange(
            time_range(time_point(0, 0, 1), time_point(0, 0, 2)),
            time_range(time_point(0, 0, 0), time_point(0, 0, 3)),
        ))
        self.assertTrue(extract_bool(result))

    def test__should_be_false_if_the_first_contains_the_second__day(self):
        result = self.prolog.query(inside_another_timerange(
            time_range(time_point(0, 0, 0), time_point(3, 0, 0)),
            time_range(time_point(1, 0, 0), time_point(2, 0, 0)),
        ), print_query=True)
        self.assertFalse(extract_bool(result))

    def test__should_be_false_if_the_first_contains_the_second__hour(self):
        result = self.prolog.query(inside_another_timerange(
            time_range(time_point(0, 0, 0), time_point(0, 3, 0)),
            time_range(time_point(0, 1, 0), time_point(0, 2, 0)),
        ))
        self.assertFalse(extract_bool(result))

    def test__should_be_false_if_the_first_contains_the_second__minute(self):
        result = self.prolog.query(inside_another_timerange(
            time_range(time_point(0, 0, 0), time_point(0, 0, 3)),
            time_range(time_point(0, 0, 1), time_point(0, 0, 2)),
        ))
        self.assertFalse(extract_bool(result))


