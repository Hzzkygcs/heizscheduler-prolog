from unittest import TestCase

from HzzProlog.HzzProlog import HzzProlog
from HzzProlog.test_util import remove_trailing_false_or_true
from definitions.functors import time_range
from definitions.misc import define_tokenizer_regex
from definitions.operators import time_point
from definitions.paths import MAIN_PROLOG_FILE_IO
from definitions.predicates import available, have_time, find_jadwal
from definitions.variables import Result, dont_care

HOUR = 60
DAY = 24*HOUR

NPM_FIRST = 1000
NPM_SECOND = 2000


class TestFindJadwal(TestCase):    # TODO
    def setUp(self) -> None:
        self.prolog = HzzProlog(MAIN_PROLOG_FILE_IO)
        define_tokenizer_regex(self.prolog)

    def test__should_return_empty_when_no_facts_provided(self):
        result = self.prolog.query(find_jadwal(10, Result))
        result = remove_trailing_false_or_true(result)
        self.assertEqual([], result)

    def test__should_return_empty_when_the_only_fact_provided_is_available(self):
        self.prolog.add_facts('testing_definitions', [
            available(time_range(
                time_point(dont_care, dont_care, dont_care),
                time_point(dont_care, dont_care, dont_care),
            )),
        ])
        result = self.prolog.query(find_jadwal(10, Result))
        result = remove_trailing_false_or_true(result)
        self.assertEqual([], result)


    def test__should_return_empty_when_the_only_fact_provided_is_have_time(self):
        self.prolog.add_facts('testing_definitions', [
            have_time(NPM_FIRST, dont_care,
                      time_range(
                          time_point(0, 0, 0),
                          time_point(1, 1, 1),
                      )),
        ])
        result = self.prolog.query(find_jadwal(10, Result))
        result = remove_trailing_false_or_true(result)
        self.assertEqual([], result)


    def test__should_return_empty_when_available_and_haveTime_do_not_intersect(self):
        self.prolog.add_facts('testing_definitions', [
            available(time_range(
                time_point(1, 0, 0),
                time_point(1, 1, 0),
            )),
            have_time(dont_care, dont_care,
                      time_range(
                          time_point(2, 0, 0),
                          time_point(2, 1, 0),
                      )),
        ])
        result = self.prolog.query(find_jadwal(10, Result))
        result = remove_trailing_false_or_true(result)
        self.assertEqual([], result)

    def test__should_return_empty_when_available_and_haveTime_intersects_but_the_duration_provided_cannot_fit_into_it(self):
        duration = 10
        intersect_width = duration - 1
        assert intersect_width < duration   # for the purpose of this test case
        self.prolog.add_facts('testing_definitions', [
            available(time_range(
                time_point(1, 0, 0),
                time_point(1, 0, 59),
            )),
            have_time(dont_care, dont_care,
                      time_range(
                          time_point(1, 0, 59 - intersect_width),
                          time_point(2, 0, 0),
                      )),
        ])
        result = self.prolog.query(find_jadwal(duration, Result))
        result = remove_trailing_false_or_true(result)
        self.assertEqual([], result)

    def test__should_return_correctly_if_there_is_one_valid_option(self):
        return # TODO
        duration = 10
        self.prolog.add_facts('testing_definitions', [
            available(time_range(
                time_point(1, 0, 0),
                time_point(1, 1, 0),
            )),
            have_time(NPM_FIRST, 1,
                      time_range(
                          time_point(1, 0, 50), time_point(2, 0, 0)
                      )),
        ])

        result = self.prolog.query(find_jadwal(duration, Result))
        result = remove_trailing_false_or_true(result)
        self.assertEqual([], result)



