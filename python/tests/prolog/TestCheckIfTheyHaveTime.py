from unittest import TestCase

from HzzProlog.HzzProlog import HzzProlog
from HzzProlog.PrologCallable import define_variable
from HzzProlog.test_util import remove_trailing_false_or_true
from definitions.functors import time_range
from definitions.misc import define_tokenizer_regex
from definitions.operators import time_point
from definitions.paths import FIND_JADWAL_PATH
from definitions.predicates import check_if_they_have_time, have_time
from definitions.variables import dont_care
from tests.testing_utils.get_list_from_list_of_dicts import get_list_from_list_of_dicts


def extract_bool(result):
    if len(result) > 1:
        return True
    if len(result) == 0:
        return False
    return result[0] == 'true'


NPM_FIRST = 123
NPM_SECOND = 456
ResultIsPreferred = define_variable("ResultIsPreferred")
time_0_0_0 = time_point(0, 0, 0)
time_1_0_0 = time_point(1, 0, 0)
time_2_0_0 = time_point(2, 0, 0)
time_3_0_0 = time_point(3, 0, 0)
time_4_0_0 = time_point(4, 0, 0)
time_5_0_0 = time_point(5, 0, 0)
time_6_0_0 = time_point(6, 0, 0)

range_0_to_4 = time_range(time_0_0_0, time_4_0_0)
range_1_to_6 = time_range(time_1_0_0, time_6_0_0)
range_2_to_3 = time_range(time_2_0_0, time_3_0_0)
range_5_to_6 = time_range(time_5_0_0, time_6_0_0)



class TestCheckIfTheyHaveTime(TestCase):
    def setUp(self) -> None:
        self.prolog = HzzProlog(FIND_JADWAL_PATH)
        define_tokenizer_regex(self.prolog)

    def test__should_be_false_if_no_haveTime_fact_provided(self):
        result = self.__test_if_given_timerange_conform_any_haveTime_fact(
            range_2_to_3,
            [])
        self.assertFalse(extract_bool(result))

    def test__should_be_false_if_haveTime_fact_provided_isnt_related_to_the_npm(self):
        result = self.__test_if_given_timerange_conform_any_haveTime_fact(
            range_2_to_3,
            [
                have_time(NPM_SECOND, dont_care, range_0_to_4),
            ])
        self.assertFalse(extract_bool(result))

    def test__should_be_false_if_haveTime_do_not_contains_the_time_range(self):
        result = self.__test_if_given_timerange_conform_any_haveTime_fact(
            range_2_to_3,
            [
                have_time(NPM_FIRST, dont_care, range_5_to_6),
            ])
        self.assertFalse(extract_bool(result))

    def test__should_return_correctly_otherwise__is_preferred_0(self):
        self.__test__should_return_correctly_otherwise(0)

    def test__should_return_correctly_otherwise__is_preferred_1(self):
        self.__test__should_return_correctly_otherwise(1)


    def __test__should_return_correctly_otherwise(self, is_preferred):
        result = self.__test_if_given_timerange_conform_any_haveTime_fact(
            range_2_to_3,
            [
                have_time(NPM_FIRST, is_preferred, range_1_to_6),
            ])
        result = remove_trailing_false_or_true(result)
        result = get_list_from_list_of_dicts(result, ResultIsPreferred)
        self.assertListEqual([is_preferred], result)


    def test__allow_presence_of_multiple_haveTime__should_return_correctly(self):
        result = self.__test_if_given_timerange_conform_any_haveTime_fact(
            range_2_to_3,
            [
                have_time(NPM_SECOND, 1, range_5_to_6),
                have_time(NPM_FIRST, 1, range_5_to_6),
                have_time(NPM_FIRST, 0, range_2_to_3),  # NPM_FIRST
            ])
        result = remove_trailing_false_or_true(result)
        result = get_list_from_list_of_dicts(result, ResultIsPreferred)
        self.assertListEqual([0], result)

    def test__allow_presence_of_multiple_haveTime__should_return_false(self):
        result = self.__test_if_given_timerange_conform_any_haveTime_fact(
            range_2_to_3,
            [
                have_time(NPM_SECOND, dont_care, range_5_to_6),
                have_time(NPM_SECOND, dont_care, range_2_to_3),  # NPM_SECOND
                have_time(NPM_FIRST, dont_care, range_5_to_6),
            ])
        self.assertFalse(extract_bool(result))

    def test__allow_presence_of_multiple_haveTime__should_return_all_relevant_isPreferred(self):
        result = self.__test_if_given_timerange_conform_any_haveTime_fact(
            range_2_to_3,
            [
                have_time(NPM_FIRST, 0, range_2_to_3),
                have_time(NPM_FIRST, 1, range_2_to_3),
            ])
        result = remove_trailing_false_or_true(result)
        result = get_list_from_list_of_dicts(result, ResultIsPreferred)
        self.assertCountEqual([0, 1], result)

    def __test_if_given_timerange_conform_any_haveTime_fact(self, given_time_range, list_of_have_time):
        self.prolog.add_facts('testing_definitions', list_of_have_time)
        result = self.prolog.query(check_if_they_have_time(
            NPM_FIRST, given_time_range, ResultIsPreferred))
        return result
