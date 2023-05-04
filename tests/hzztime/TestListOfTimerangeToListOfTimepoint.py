from unittest import TestCase

from HzzProlog.HzzProlog import HzzProlog
from HzzProlog.test_util import remove_trailing_false_or_true
from definitions.functors import time_range
from definitions.misc import define_tokenizer_regex
from definitions.operators import time_point
from definitions.paths import HZZ_TIME_PL_IO
from definitions.predicates import list_of_timeranges_to_list_of_timepoints
from definitions.variables import Result
from tests.testing_utils.get_list_from_list_of_dicts import get_list_from_list_of_dicts


class TestListOfTimerangeToListOfTimePoint(TestCase):
    def setUp(self) -> None:
        self.prolog = HzzProlog(HZZ_TIME_PL_IO)
        define_tokenizer_regex(self.prolog)

    def test__empty_input_should_result_in_empty_output(self):
        result = self.prolog.query(list_of_timeranges_to_list_of_timepoints(
            [], Result
        ))
        result = self.__preprocess_result(result)
        self.assertEqual([], result)

    def __preprocess_result(self, result):
        result = remove_trailing_false_or_true(result)
        result = get_list_from_list_of_dicts(result, 'Result')
        assert len(result) == 1
        result = result[0]
        return result

    def test__should_return_their_starting_and_ending_timepoint(self):
        result = self.prolog.query(list_of_timeranges_to_list_of_timepoints(
            [
                time_range(time_point(0, 0, 0), time_point(1, 0, 0)),
            ], Result
        ))
        result = self.__preprocess_result(result)
        self.assertEqual([
            time_point(0, 0, 0),
            time_point(1, 0, 0),
        ], result)

    def test__should_return_their_starting_and_ending_timepoint__multiple(self):
        result = self.prolog.query(list_of_timeranges_to_list_of_timepoints(
            [
                time_range(time_point(0, 0, 0), time_point(1, 0, 0)),
                time_range(time_point(2, 2, 0), time_point(2, 2, 2)),
                time_range(time_point(3, 2, 0), time_point(3, 2, 2)),
            ], Result
        ))
        result = self.__preprocess_result(result)
        self.assertEqual([
            time_point(0, 0, 0), time_point(1, 0, 0),
            time_point(2, 2, 0), time_point(2, 2, 2),
            time_point(3, 2, 0), time_point(3, 2, 2),
        ], result)
