from unittest import TestCase

from HzzProlog.HzzProlog import HzzProlog
from definitions.functors import time_range
from definitions.operators import time_point
from definitions.paths import TIME_PROCESSING_UTILITIES_PL_PATH
from definitions.predicates import time_conflict


def is_conflict(result):
    if len(result) > 1:
        return True
    if len(result) == 0:
        return False
    return result[0] == 'true'


class TestTimeConflict(TestCase):
    def setUp(self) -> None:
        self.prolog = HzzProlog(TIME_PROCESSING_UTILITIES_PL_PATH)

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
        query = time_conflict(
            time_range(time_point(1, 0, 0), time_point(2, 0, 0)),
            time_range(time_point(1, 1, 0), time_point(1, 2, 0)),
        )
        result = self.prolog.query(query)
        print(result)
        self.assertTrue(is_conflict(result))

    def test__should_be_true_if_one_contains_another__should_be_commutative(self):
        result = self.prolog.query(time_conflict(
            time_range(time_point(1, 1, 0), time_point(1, 2, 0)),
            time_range(time_point(1, 0, 0), time_point(2, 0, 0)),
        ))
        self.assertTrue(is_conflict(result))

    def test__should_return_correctly_for_small_differences__part1(self):
        result = self.prolog.query(time_conflict(
            time_range(time_point(3, 3, 3), time_point(3, 3, 10)),
            time_range(time_point(3, 3, 10), time_point(3, 15)),
        ))
        self.assertFalse(is_conflict(result))

    def test__should_return_correctly_for_small_differences__part2(self):
        result = self.prolog.query(time_conflict(
            time_range(time_point(3, 3, 3), time_point(3, 3, 10)),
            time_range(time_point(3, 3, 11), time_point(3, 3, 15)),
        ))
        self.assertFalse(is_conflict(result))

    def test__should_return_correctly_for_small_differences__part3(self):
        result = self.prolog.query(time_conflict(
            time_range(time_point(3, 3, 3), time_point(3, 3, 10)),
            time_range(time_point(3, 3, 9), time_point(3, 15)),
        ))
        self.assertTrue(is_conflict(result))

    def test__should_return_correctly_for_small_differences__part4(self):
        result = self.prolog.query(time_conflict(
            time_range(time_point(3, 3, 10), time_point(3, 15)),
            time_range(time_point(3, 3, 3), time_point(3, 3, 10)),
        ))
        self.assertFalse(is_conflict(result))


    def test__should_return_correctly_for_small_differences__part5(self):
        result = self.prolog.query(time_conflict(
            time_range(time_point(3, 3, 11), time_point(3, 3, 15)),
            time_range(time_point(3, 3, 3), time_point(3, 3, 10)),
        ))
        self.assertFalse(is_conflict(result))

    def test__should_return_correctly_for_small_differences__part6(self):
        result = self.prolog.query(time_conflict(
            time_range(time_point(3, 3, 9), time_point(3, 15)),
            time_range(time_point(3, 3, 3), time_point(3, 3, 10)),
        ))
        self.assertTrue(is_conflict(result))



