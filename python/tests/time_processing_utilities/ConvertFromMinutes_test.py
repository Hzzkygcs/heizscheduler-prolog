from unittest import TestCase

from HzzProlog.HzzProlog import HzzProlog
from definitions.misc import define_tokenizer_regex
from definitions.operators import datetime
from definitions.paths import TIME_PROCESSING_UTILITIES_PL_PATH
from definitions.predicates import convert_from_minutes
from definitions.variables import Result

JAM = 60
HARI = JAM * 60


class TestConvertFromMinutes(TestCase):
    def setUp(self) -> None:
        self.prolog = HzzProlog(TIME_PROCESSING_UTILITIES_PL_PATH)
        define_tokenizer_regex(self.prolog)

    def test__should_convert_minutes_correctly(self):
        query = convert_from_minutes(
            1, Result
        )
        result = self.prolog.query(query)
        self.assertEqual(datetime(0, 0, 1), result[0]['Result'])

    def test__should_convert_hours_correctly(self):
        result = self.prolog.query(convert_from_minutes(
            60 + 2, Result
        ))
        self.assertEqual(datetime(0, 1, 2), result[0]['Result'])

    def test__should_convert_days_correctly(self):
        result = self.prolog.query(convert_from_minutes(
            1*24*60 + 2*60 + 3, Result
        ))
        self.assertEqual(datetime(1, 2, 3), result[0]['Result'])
