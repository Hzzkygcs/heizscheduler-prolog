from unittest import TestCase

from HzzProlog.HzzProlog import HzzProlog
from HzzProlog.test_util import assert_prolog_output_the_same
from definitions.predicates import have_time, all_npm
from definitions.paths import MAIN_PROLOG_FILE_IO
from definitions.variables import X, dont_care


class TestAllNpm(TestCase):
    def setUp(self) -> None:
        self.prolog = HzzProlog(MAIN_PROLOG_FILE_IO)

    def test_all_npm__should_all_npm_from_haveTime(self):
        self.prolog.add_facts("have_time_definitions", [
            have_time(2006463162, dont_care, dont_care, dont_care),
            have_time(2006462664, dont_care, dont_care, dont_care),
            have_time(2006473535, dont_care, dont_care, dont_care),
            have_time(2006482426, dont_care, dont_care, dont_care),
            have_time(1234567890, dont_care, dont_care, dont_care),
        ])
        result = self.prolog.query(all_npm(X))
        expected = [
            {'X': 2006463162,},
            {'X': 2006462664,},
            {'X': 2006473535,},
            {'X': 2006482426,},
            {'X': 1234567890,},
        ]
        assert_prolog_output_the_same(self, expected, result)

