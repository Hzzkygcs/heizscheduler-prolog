from unittest import TestCase

from HzzProlog.HzzProlog import HzzProlog
from HzzProlog.test_util import remove_trailing_false_or_true
from definitions.functors import booked_slot
from definitions.paths import MAIN_PROLOG_FILE_IO
from definitions.predicates import count_preferred
from definitions.variables import X, dont_care


class TestCountPreferred(TestCase):
    def setUp(self) -> None:
        self.prolog = HzzProlog(MAIN_PROLOG_FILE_IO)

    def test__should_return_0_for_empty_list(self):
        self._run_test([])


    def test__should_return_corrently_for_single_item_list__0_preferred(self):
        self._run_test([0])

    def test__should_return_corrently_for_single_item_list__1_preferred(self):
        self._run_test([1])

    def test__should_return_corrently_for_double_item_list__0_preferred(self):
        self._run_test([0, 0])

    def test__should_return_corrently_for_double_item_list__1_preferred__variant_1(self):
        self._run_test([0, 1])

    def test__should_return_corrently_for_double_item_list__1_preferred__variant_2(self):
        self._run_test([1, 0])

    def test__should_return_corrently_for_double_item_list__2_preferred(self):
        self._run_test([1, 1])

    def test__should_return_corrently_for_any_item_list__variant_1(self):
        self._run_test([1, 1, 0, 1, 0])

    def test__should_return_corrently_for_any_item_list__variant_2(self):
        self._run_test([0, 1, 0, 1, 0])

    def _run_test(self, list_of_preferred: list[int]):
        query = count_preferred([
            booked_slot(dont_care, preferred, dont_care) for preferred in list_of_preferred
        ], X)
        result = self.prolog.query(query)
        result = remove_trailing_false_or_true(result)
        self.assertEqual(1, len(result))
        result = result[0]['X']
        self.assertEqual(sum(list_of_preferred), result)
