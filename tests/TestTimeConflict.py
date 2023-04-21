from unittest import TestCase

from HzzProlog.ChainEquality import equality
from HzzProlog.HzzProlog import HzzProlog
from HzzProlog.test_util import assert_prolog_output_the_same
from definitions.functors import time_range
from definitions.predicates import available, time, have_time, time_conflict
from definitions.operators import time_point
from definitions.paths import MAIN_PROLOG_FILE_IO, HZZ_TIME_PL_IO
from definitions.variables import X, Y, Z, dont_care


def is_conflict(result):
    if len(result) > 1:
        return True
    if len(result) == 0:
        return False
    return result[0] == 'true'


class TestTimeConflict(TestCase):
    def setUp(self) -> None:
        self.prolog = HzzProlog(HZZ_TIME_PL_IO)

    def test__end_time_of_a_timeRange_should_be_considered_as_exclusive(self):
        result = self.prolog.query(time_conflict(
            time_range(time_point(0, 0, 0), time_point(1, 0, 0)),
            time_range(time_point(1, 0, 0), time_point(2, 0, 0)),
        ))
        self.assertFalse(is_conflict(result))

    def test__should_be_false_if_they_do_not_intersect(self):
        result = self.prolog.query(time_conflict(
            time_range(time_point(0, 0, 0), time_point(1, 0, 0)),
            time_range(time_point(2, 0, 0), time_point(3, 0, 0)),
        ))
        self.assertFalse(is_conflict(result))

    def test__should_be_true_if_they_intersects(self):
        query = time_conflict(
            time_range(time_point(0, 0, 0), time_point(1, 0, 0)),
            time_range(time_point(0, 0, 59), time_point(2, 0, 0)),
        )
        result = self.prolog.query(query)
        self.assertTrue(is_conflict(result))

    def test__should_be_true_if_they_intersects__should_be_commutative(self):
        result = self.prolog.query(time_conflict(
            time_range(time_point(0, 0, 59), time_point(2, 0, 0)),
            time_range(time_point(0, 0, 0), time_point(1, 0, 0)),
        ))
        self.assertTrue(is_conflict(result))

    def test__should_be_true_if_they_are_exactly_the_same(self):
        range_ = time_range(time_point(1, 0, 0), time_point(2, 0, 0))
        result = self.prolog.query(time_conflict(
            range_, range_
        ))
        self.assertTrue(is_conflict(result))

    def test__should_be_true_if_one_contains_another(self):
        result = self.prolog.query(time_conflict(
            time_range(time_point(1, 0, 0), time_point(2, 0, 0)),
            time_range(time_point(1, 1, 0), time_point(1, 2, 0)),
        ))
        self.assertTrue(is_conflict(result))

    def test__should_be_true_if_one_contains_another__should_be_commutative(self):
        result = self.prolog.query(time_conflict(
            time_range(time_point(1, 1, 0), time_point(1, 2, 0)),
            time_range(time_point(1, 0, 0), time_point(2, 0, 0)),
        ))
        self.assertTrue(is_conflict(result))




