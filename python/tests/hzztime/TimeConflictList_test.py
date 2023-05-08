from unittest import TestCase

from HzzProlog.HzzProlog import HzzProlog
from definitions.functors import time_range
from definitions.operators import time_point
from definitions.paths import TIME_PROCESSING_UTILITIES_PL_PATH
from definitions.predicates import time_conflict_list


def is_conflict(result):
    if len(result) > 1:
        return True
    if len(result) == 0:
        return False
    return result[0] == 'true'


class TestTimeConflictList(TestCase):
    def setUp(self) -> None:
        self.prolog = HzzProlog(TIME_PROCESSING_UTILITIES_PL_PATH)

    def test__end_time_of_a_timeRange_should_be_considered_as_exclusive(self):
        result = self.prolog.query(time_conflict_list(
            time_range(time_point(1, 0, 0), time_point(2, 0, 0)),
            [
                time_range(time_point(2, 0, 0), time_point(3, 0, 0)),
                time_range(time_point(0, 0, 0), time_point(1, 0, 0)),
            ],
        ))
        self.assertFalse(is_conflict(result))

    def test__should_be_true_if_conflicting_with_the_first_item(self):
        result = self.prolog.query(time_conflict_list(
            time_range(time_point(1, 0, 0), time_point(2, 0, 1)),
            [
                time_range(time_point(2, 0, 0), time_point(3, 0, 0)),
                time_range(time_point(0, 0, 0), time_point(1, 0, 0)),
            ],
        ))
        self.assertTrue(is_conflict(result))

    def test__should_be_true_if_conflicting_with_the_second_item(self):
        result = self.prolog.query(time_conflict_list(
            time_range(time_point(1, 0, 0), time_point(2, 0, 0)),
            [
                time_range(time_point(2, 0, 0), time_point(3, 0, 0)),
                time_range(time_point(0, 0, 0), time_point(1, 0, 1)),
            ],
        ))
        self.assertTrue(is_conflict(result))

    def test__should_be_true_if_conflicting_with_the_nth_item(self):
        result = self.prolog.query(time_conflict_list(
            time_range(time_point(1, 0, 0), time_point(2, 0, 0)),
            [
                time_range(time_point(6, 0, 0), time_point(7, 0, 1)),
                time_range(time_point(2, 0, 0), time_point(3, 0, 0)),
                time_range(time_point(0, 0, 0), time_point(1, 0, 1)),  # conflicting
                time_range(time_point(4, 0, 0), time_point(5, 0, 1)),
            ],
        ))
        self.assertTrue(is_conflict(result))

    def test__should_ignore_any_conflicts_between_TimeRanges_within_the_list(self):
        result = self.prolog.query(time_conflict_list(
            time_range(time_point(10, 0, 0), time_point(11, 0, 0)),
            [
                time_range(time_point(0, 0, 0), time_point(7, 0, 1)),
                time_range(time_point(2, 0, 0), time_point(3, 0, 0)),
                time_range(time_point(5, 0, 0), time_point(8, 0, 1)),
                time_range(time_point(1, 0, 0), time_point(2, 0, 1)),
            ],
        ))
        self.assertFalse(is_conflict(result))
