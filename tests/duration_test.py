from unittest import TestCase

from HzzProlog.HzzProlog import HzzProlog
from tests.definitions import TIME_CONFLICT_PL_IO, duration, time_range, time_point, X


class TestDuration(TestCase):
    def setUp(self) -> None:
        self.prolog = HzzProlog(TIME_CONFLICT_PL_IO)

    def test__should_return_correctly(self):
        query = duration(
            time_range(time_point(1, 10), time_point(2, 20)),
            X)
        result = self.prolog.query(query)
        self.assertEqual(70, result[0]['X'])

    def test__should_return_negative_if_swapped(self):
        result = self.prolog.query(duration(
            time_range(time_point(2, 20), time_point(1, 10)),
            X
        ))
        self.assertEqual(-70, result[0]['X'])

    def test__should_return_zero_if_the_same(self):
        result = self.prolog.query(duration(
            time_range(time_point(2, 20), time_point(2, 20)),
            X
        ))
        self.assertEqual(0, result[0]['X'])

    def test__should_be_able_to_be_used_as_addition(self):
        result = self.prolog.query(duration(
            time_range(time_point(2, 20), X),
            35
        ))
        self.assertEqual(time_point(2, 55),
                         result[0]['X'])

    def test__should_be_able_to_be_used_as_addition_with_carry(self):
        result = self.prolog.query(duration(
            time_range(time_point(2, 20), X),
            160
        ))
        self.assertEqual(time_point(5, 0),
                         result[0]['X'])

    def test__should_be_able_to_be_used_as_substraction(self):
        result = self.prolog.query(duration(
            time_range(time_point(2, 20), X),
            -10
        ))
        self.assertEqual(time_point(2, 10),
                         result[0]['X'])

    def test__should_be_able_to_be_used_as_substraction_with_borrow(self):
        result = self.prolog.query(duration(
            time_range(time_point(5, 20), X),
            -3 * 60 - 15
        ))
        self.assertEqual(time_point(2, 5),
                         result[0]['X'])