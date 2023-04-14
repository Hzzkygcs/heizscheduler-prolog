from unittest import TestCase

from HzzProlog.HzzProlog import HzzProlog
from definitions.predicates import duration
from definitions.misc import define_tokenizer_regex
from definitions.operators import time_range, time_point
from definitions.paths import TIME_CONFLICT_PL_IO
from definitions.variables import X


class TestDuration(TestCase):
    def setUp(self) -> None:
        self.prolog = HzzProlog(TIME_CONFLICT_PL_IO)
        self.prolog = define_tokenizer_regex(self.prolog)

    def test__should_return_correctly(self):
        temp1 = time_point(1, 10)
        temp2 = time_point(2, 20)
        query = duration(
            time_range(temp1, temp2),
            X
        )
        result = self.prolog.query(query)
        self.assertEqual(70, result[0]['X'])


    def test__should_return_zero_if_the_same(self):
        result = self.prolog.query(duration(
            time_range(time_point(2, 20), time_point(2, 20)),
            X
        ))
        self.assertEqual(0, result[0]['X'])
