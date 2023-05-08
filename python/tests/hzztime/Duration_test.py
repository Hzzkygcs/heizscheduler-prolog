from unittest import TestCase

from HzzProlog.HzzProlog import HzzProlog
from definitions.functors import time_range
from definitions.misc import define_tokenizer_regex
from definitions.operators import datetime
from definitions.paths import TIME_PROCESSING_UTILITIES_PL_PATH
from definitions.predicates import duration
from definitions.variables import X

JAM = 60
HARI = JAM * 24


class TestAddTime(TestCase):
    def setUp(self) -> None:
        self.prolog = HzzProlog(TIME_PROCESSING_UTILITIES_PL_PATH)
        define_tokenizer_regex(self.prolog)

    def test__should_return_0_if_starttime_and_endtime_are_equal(self):
        result = self.prolog.query(duration(
            time_range(datetime(0, 0, 0), datetime(0, 0, 0)),  X
        ))
        self.assertEqual(0, result[0][X])

    def test__should_return_1_if_endtime_is_1_minute_ahead_of_starttime(self):
        result = self.prolog.query(duration(
            time_range(datetime(0, 0, 0), datetime(0, 0, 1)),  X
        ))
        self.assertEqual(1, result[0][X])


    def test__should_return_10_if_endtime_is_10_minute_ahead_of_starttime(self):
        result = self.prolog.query(duration(
            time_range(datetime(0, 0, 0), datetime(0, 0, 10)),  X
        ))
        self.assertEqual(10, result[0][X])


    def test__should_return_60_if_endtime_is_1_hour_ahead_of_starttime(self):
        result = self.prolog.query(duration(
            time_range(datetime(0, 1, 0), datetime(0, 2, 0)),  X
        ))
        self.assertEqual(60, result[0][X])

    def test__should_return_80_if_endtime_is_1_hour_20_minutes_ahead_of_starttime(self):
        result = self.prolog.query(duration(
            time_range(datetime(0, 1, 0), datetime(0, 2, 20)),  X
        ))
        self.assertEqual(80, result[0][X])

    def test__should_return_180_if_endtime_is_3_hour_ahead_of_starttime(self):
        result = self.prolog.query(duration(
            time_range(datetime(0, 3, 10), datetime(0, 6, 10)),  X
        ))
        self.assertEqual(180, result[0][X])

    def test__should_return_1440_if_endtime_is_1_day_ahead_of_starttime(self):
        result = self.prolog.query(duration(
            time_range(datetime(0, 0, 0), datetime(1, 0, 0)),  X
        ))
        self.assertEqual(1440, result[0][X])

    def test__should_return_4400_if_endtime_is_3_day_1_hour_20_minutes_ahead_of_starttime(self):
        result = self.prolog.query(duration(
            time_range(datetime(2, 2, 2), datetime(5, 3, 22)),  X
        ))
        self.assertEqual(4400, result[0][X])


