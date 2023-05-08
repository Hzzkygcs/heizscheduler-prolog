from itertools import permutations
from unittest import TestCase

from HzzProlog.HzzProlog import HzzProlog
from HzzProlog.test_util import remove_trailing_false_or_true, assert_prolog_output_the_same, \
    compare_list_just_like_a_set
from definitions.functors import time_range, booked_slot
from definitions.misc import define_tokenizer_regex
from definitions.operators import time_point
from definitions.paths import FIND_JADWAL_PATH
from definitions.predicates import available, have_time, find_jadwal
from definitions.variables import Result, dont_care
from tests.testing_utils.get_list_from_list_of_dicts import get_list_from_list_of_dicts

HOUR = 60
DAY = 24*HOUR

NPM_FIRST = 1000
NPM_SECOND = 2000
NPM_THIRD = 3000

PREFERRED = 1
NOT_PREFERRED = 0

class TestFindJadwal(TestCase):
    def setUp(self) -> None:
        self.prolog = HzzProlog(FIND_JADWAL_PATH)
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
        duration = 10
        self.prolog.add_facts('testing_definitions', [
            available(time_range(
                time_point(1, 0, 0),
                time_point(1, 1, 0),
            )),
            have_time(NPM_FIRST, PREFERRED,
                      time_range(time_point(1, 0, 50), time_point(2, 0, 0))),
        ])

        result = self.prolog.query(find_jadwal(duration, Result))
        result = remove_trailing_false_or_true(result)
        assert len(result) == 1
        result = result[0]['Result']
        self.assertEqual([booked_slot(NPM_FIRST, PREFERRED, time_range(time_point(1, 0, 50), time_point(1, 1, 0)))],
                         result)

    def test__should_return_all_valid_possibility__but_remember_dont_bruteforce_minute_by_minute(self):
        duration = 5
        self.prolog.add_facts('testing_definitions', [
            available(time_range(
                time_point(1, 0, 0),
                time_point(1, 1, 0),
            )),
            have_time(NPM_FIRST, PREFERRED,
                      time_range(time_point(1, 0, 30), time_point(2, 0, 0))),
            have_time(NPM_FIRST, NOT_PREFERRED,
                      time_range(time_point(1, 0, 55), time_point(2, 0, 0))),
        ])

        result = self.prolog.query(find_jadwal(duration, Result))
        result = remove_trailing_false_or_true(result)
        result = get_list_from_list_of_dicts(result, 'Result')
        self.assertCountEqual([[
                booked_slot(NPM_FIRST, PREFERRED, time_range(time_point(1, 0, 30), time_point(1, 0, 35)))
            ], [
                booked_slot(NPM_FIRST, PREFERRED, time_range(time_point(1, 0, 50), time_point(1, 0, 55)))
            ], [
                booked_slot(NPM_FIRST, PREFERRED, time_range(time_point(1, 0, 55), time_point(1, 1, 0)))
            ], [
                booked_slot(NPM_FIRST, NOT_PREFERRED, time_range(time_point(1, 0, 55), time_point(1, 1, 0)))
            ],
        ], result)

    def test__should_consider_all_npm(self):
        duration = 5
        self.prolog.add_facts('testing_definitions', [
            available(time_range(
                time_point(1, 0, 0),
                time_point(1, 1, 0),
            )),
            have_time(NPM_FIRST, PREFERRED,
                      time_range(time_point(1, 0, 30), time_point(2, 0, 0))),
            have_time(NPM_FIRST, NOT_PREFERRED,
                      time_range(time_point(1, 0, 30), time_point(2, 0, 0))),
            have_time(NPM_SECOND, NOT_PREFERRED,
                      time_range(time_point(1, 0, 55), time_point(2, 0, 0))),
        ])
        expected_second_npm_booked_slot = booked_slot(NPM_SECOND, NOT_PREFERRED,
                                                      time_range(time_point(1, 0, 55), time_point(1, 1, 0)))

        result = self.prolog.query(find_jadwal(duration, Result))
        result = remove_trailing_false_or_true(result)
        result = get_list_from_list_of_dicts(result, 'Result')
        assert_prolog_output_the_same(self, [
            [
                booked_slot(NPM_FIRST, PREFERRED, time_range(time_point(1, 0, 30), time_point(1, 0, 35))),
                expected_second_npm_booked_slot,
            ], [
                booked_slot(NPM_FIRST, NOT_PREFERRED, time_range(time_point(1, 0, 30), time_point(1, 0, 35))),
                expected_second_npm_booked_slot,
            ], [
                booked_slot(NPM_FIRST, PREFERRED, time_range(time_point(1, 0, 50), time_point(1, 0, 55))),
                expected_second_npm_booked_slot,
            ], [
                booked_slot(NPM_FIRST, NOT_PREFERRED, time_range(time_point(1, 0, 50), time_point(1, 0, 55))),
                expected_second_npm_booked_slot,
            ],
        ], result, ignore_duplicates=True, nested_ignore=True)


    def test__should_consider_all_npm__three_npm(self):
        self.__test__should_consider_all_npm__three_npm([
            available(time_range(
                time_point(1, 0, 0),
                time_point(1, 1, 0),
            )),
        ])


    def test__should_consider_all_npm__three_npm__multiple_available_time(self):
        available_lists = [
            available(time_range(
                time_point(1, 0, 0), time_point(1, 0, 50)
            )),
            available(time_range(
                time_point(1, 0, 50), time_point(1, 1, 0)
            )),
        ]
        self.__test__should_consider_all_npm__three_npm(available_lists)

    def __test__should_consider_all_npm__three_npm(self, available_lists):
        duration = 5
        self.prolog.add_facts('testing_definitions', available_lists + [

            # this should be possible. An example of the arrangement:
            # NPM1: 45-50. NPM2: 50-55. NPM3: 55-1:00
            have_time(NPM_FIRST, dont_care,
                      time_range(time_point(1, 0, 45), time_point(2, 0, 0))),
            have_time(NPM_SECOND, dont_care,
                      time_range(time_point(1, 0, 45), time_point(2, 0, 0))),
            have_time(NPM_THIRD, dont_care,
                      time_range(time_point(1, 0, 45), time_point(2, 0, 0))),
        ])
        result = self.prolog.query(find_jadwal(duration, Result))
        result = remove_trailing_false_or_true(result)
        result = get_list_from_list_of_dicts(result, 'Result')

        all_possible_solutions = []
        for npm_a, npm_b, npm_c in permutations((NPM_FIRST, NPM_SECOND, NPM_THIRD)):
            all_possible_solutions.append([
                booked_slot(npm_a, dont_care, time_range(time_point(1, 0, 45), time_point(1, 0, 50))),
                booked_slot(npm_b, dont_care, time_range(time_point(1, 0, 50), time_point(1, 0, 55))),
                booked_slot(npm_c, dont_care, time_range(time_point(1, 0, 55), time_point(1, 1, 0)))
            ])
        self.assertGreaterEqual(len(result), 1)
        for result_item in result:
            for one_of_possible_solution in all_possible_solutions:
                if compare_list_just_like_a_set(result_item, one_of_possible_solution):
                    break
            else:
                self.fail()

