from unittest import TestCase

from HzzProlog.ChainEquality import equality
from HzzProlog.HzzProlog import HzzProlog
from HzzProlog.test_util import assert_prolog_output_the_same
from definitions.predicates import available, time, have_time
from definitions.operators import time_point
from definitions.paths import MAIN_PROLOG_FILE_IO
from definitions.variables import X, Y, Z, dont_care


class TestTime(TestCase):
    def setUp(self) -> None:
        self.prolog = HzzProlog(MAIN_PROLOG_FILE_IO)

    def test_time__should_return_list_of_endpoints_correctly_from_available(self):
        self.prolog.add_facts("available_definitions", [
            available(time_point(0, 1, 2), time_point(3, 4, 5)),
            available(time_point(6, 10, 10), time_point(6, 23, 59)),
        ])
        result = self.prolog.query(time(time_point(X, Y, Z)))
        expected = [
            {'X': 0, 'Y': 1, 'Z': 2},
            {'X': 3, 'Y': 4, 'Z': 5},
            {'X': 6} | equality('Y', 'Z', value=10),
            {'X': 6, 'Y': 23, 'Z': 59},
        ]
        assert expected[2]['Y'] == result[1]['Y']
        assert_prolog_output_the_same(self, expected, result)


    def test_time__should_return_list_of_endpoints_correctly_from_haveTime(self):
        self.prolog.add_facts("have_time_definitions", [
            have_time(dont_care, dont_care, time_point(3, 13, 45), time_point(3, 16, 00)),
            have_time(dont_care, dont_care, time_point(3, 13, 30), time_point(3, 18, 00)),
        ])
        result = self.prolog.query(time(time_point(X, Y, Z)))
        expected = [
            {'X': 3, 'Y': 13, 'Z': 45},
            {'X': 3, 'Y': 16, 'Z': 00},
            {'X': 3, 'Y': 13, 'Z': 30},
            {'X': 3, 'Y': 18, 'Z': 00},
        ]
        assert_prolog_output_the_same(self, expected, result)


    def test_time__should_return_list_of_endpoints_correctly_from_all_of_them(self):
        self.prolog.add_facts("available_definitions", [
            available(time_point(0, 1, 2), time_point(3, 4, 5)),
            available(time_point(6, 10, 10), time_point(6, 23, 59)),
            have_time(dont_care, dont_care, time_point(3, 13, 45), time_point(3, 16, 00)),
            have_time(dont_care, dont_care, time_point(3, 13, 30), time_point(3, 18, 00)),
        ])
        result = self.prolog.query(time(time_point(X, Y, Z)))
        expected = [
            {'X': 0, 'Y': 1, 'Z': 2},
            {'X': 3, 'Y': 4, 'Z': 5},
            {'X': 6} | equality('Y', 'Z', value=10),
            {'X': 6, 'Y': 23, 'Z': 59},

            {'X': 3, 'Y': 13, 'Z': 45},
            {'X': 3, 'Y': 16, 'Z': 00},
            {'X': 3, 'Y': 13, 'Z': 30},
            {'X': 3, 'Y': 18, 'Z': 00},
        ]
        assert_prolog_output_the_same(self, expected, result)


