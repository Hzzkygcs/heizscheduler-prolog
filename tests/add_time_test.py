from unittest import TestCase

from HzzProlog.HzzProlog import HzzProlog
from tests.definitions import HZZ_TIME_PL_IO, add_time, range, datetime, X, Y, Z, Result


JAM = 60
HARI = JAM * 60


class TestAddTime(TestCase):
    def setUp(self) -> None:
        self.prolog = HzzProlog(HZZ_TIME_PL_IO)

    def test__should_adds_correctly(self):
        result = self.prolog.query(add_time(
            datetime(0, 0, 0), 30, Result
        ))
        self.assertEqual(datetime(0, 0, 30), result[0]['Result'])

    def test__should_be_able_to_be_used_as_addition_with_carry(self):
        result = self.prolog.query(add_time(
            datetime(1, 2, 3), 2*HARI + 3*JAM + 4, Result
        ))
        self.assertEqual(datetime(3, 5, 7),
                         result[0]['Result'])

    def test__should_be_able_to_be_used_as_substraction(self):
        result = self.prolog.query(add_time(
            datetime(0, 0, 40), 30, Result
        ))
        self.assertEqual(datetime(0, 0, 10), result[0]['Result'])

    def test__should_be_able_to_be_used_as_substraction_with_borrow(self):
        result = self.prolog.query(add_time(
            datetime(3, 5, 7),
            -(2 * HARI + 3 * JAM + 4), Result
        ))
        self.assertEqual(datetime(1, 2, 3),
                         result[0]['Result'])
