from unittest import TestCase

from HzzProlog.HzzProlog import HzzProlog
from definitions.operators import time_point
from definitions.paths import TIME_PROCESSING_UTILITIES_PL_PATH
from definitions.predicates import less_than


def is_less_than(result):
    if len(result) > 1:
        return True
    if len(result) == 0:
        return False
    return result[0] == 'true'


class LessThanTrue(TestCase):
    def setUp(self) -> None:
        self.prolog = HzzProlog(TIME_PROCESSING_UTILITIES_PL_PATH)

    def test__should_be_true_if_RHS_is_more_than_a_day(self):
        result = self.prolog.query(less_than(
            time_point(0, 0, 0), time_point(2, 0, 0)
        ))
        self.assertTrue(is_less_than(result))

    def test__should_be_false_if_LHS_is_more_than_a_day(self):
        result = self.prolog.query(less_than(
            time_point(2, 0, 0), time_point(0, 0, 0)
        ))
        self.assertFalse(is_less_than(result))

    def test__should_be_true_if_RHS_is_approximately_a_day(self):
        result = self.prolog.query(less_than(
            time_point(0, 0, 0), time_point(1, 0, 0)
        ))
        self.assertTrue(is_less_than(result))

    def test__should_be_true_if_RHS_is_more_than_an_hour(self):
        result = self.prolog.query(less_than(
            time_point(1, 0, 0), time_point(1, 2, 0)
        ))
        self.assertTrue(is_less_than(result))

    def test__should_be_true_if_RHS_is_aproximately_an_hour(self):
        result = self.prolog.query(less_than(
            time_point(1, 0, 0), time_point(1, 1, 0)
        ))
        self.assertTrue(is_less_than(result))

    def test__should_be_true_if_RHS_is_more_than_a_minute(self):
        result = self.prolog.query(less_than(
            time_point(1, 1, 0), time_point(1, 1, 2)
        ))
        self.assertTrue(is_less_than(result))

    def test__should_be_true_if_RHS_is_approximately_a_minute(self):
        result = self.prolog.query(less_than(
            time_point(1, 1, 0), time_point(1, 1, 1)
        ))
        self.assertTrue(is_less_than(result))



class LessThanFalse(TestCase):
    def setUp(self) -> None:
        self.prolog = HzzProlog(TIME_PROCESSING_UTILITIES_PL_PATH)

    def test__should_be_false_if_they_are_equal(self):
        result = self.prolog.query(less_than(
            time_point(2, 2, 2), time_point(2, 2, 2)
        ))
        self.assertFalse(is_less_than(result))

    def test__should_be_false_if_LHS_is_more_than_a_day(self):
        result = self.prolog.query(less_than(
            time_point(2, 0, 0), time_point(0, 0, 0)
        ))
        self.assertFalse(is_less_than(result))

    def test__should_be_false_if_LHS_is_approximately_a_day(self):
        result = self.prolog.query(less_than(
            time_point(1, 0, 0), time_point(0, 0, 0)
        ))
        self.assertFalse(is_less_than(result))

    def test__should_be_false_if_LHS_is_more_than_an_hour(self):
        result = self.prolog.query(less_than(
            time_point(1, 2, 0), time_point(1, 0, 0)
        ))
        self.assertFalse(is_less_than(result))

    def test__should_be_false_if_LHS_is_aproximately_an_hour(self):
        result = self.prolog.query(less_than(
            time_point(1, 1, 0), time_point(1, 0, 0)
        ))
        self.assertFalse(is_less_than(result))

    def test__should_be_false_if_LHS_is_more_than_a_minute(self):
        result = self.prolog.query(less_than(
            time_point(1, 1, 2), time_point(1, 1, 0)
        ))
        self.assertFalse(is_less_than(result))

    def test__should_be_false_if_LHS_is_approximately_a_minute(self):
        result = self.prolog.query(less_than(
            time_point(1, 1, 1), time_point(1, 1, 0)
        ))
        self.assertFalse(is_less_than(result))


