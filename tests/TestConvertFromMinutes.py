from unittest import TestCase

from HzzProlog.HzzProlog import HzzProlog
from definitions.predicates import convert_from_minutes
from definitions.misc import define_tokenizer_regex
from definitions.operators import datetime
from definitions.paths import HZZ_TIME_PL_IO
from definitions.variables import Result

JAM = 60
HARI = JAM * 60


class TestAddTime(TestCase):
    def setUp(self) -> None:
        self.prolog = HzzProlog(HZZ_TIME_PL_IO)
        self.prolog = define_tokenizer_regex(self.prolog)

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
