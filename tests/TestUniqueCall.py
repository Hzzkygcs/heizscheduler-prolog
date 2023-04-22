from unittest import TestCase

from HzzProlog.HzzProlog import HzzProlog
from HzzProlog.PrologCallable import define_parameterized_predicate
from HzzProlog.test_util import remove_trailing_false_or_true
from definitions.builtin_predicates import member
from definitions.paths import HZZ_TIME_PL_IO
from definitions.predicates import unique_call
from definitions.variables import X, Result, _, Y, Y1, X1
from tests.list_utils.get_list_from_list_of_dicts import get_list_from_list_of_dicts

fact = define_parameterized_predicate("fact")



class TestUniqueCall(TestCase):
    def setUp(self) -> None:
        self.prolog = HzzProlog(HZZ_TIME_PL_IO)


    def test__should_be_able_to_handle_zero_result(self):
        result = self.prolog.query(unique_call(
            X, member(X, []), Result
        ))
        result = remove_trailing_false_or_true(result)
        result = get_list_from_list_of_dicts(result, "Result")
        self.assertCountEqual([], result)

    def test__should_not_alter_any_single_result(self):
        result = self.prolog.query(unique_call(
            X, member(X, [1]), Result
        ))
        result = remove_trailing_false_or_true(result)
        result = get_list_from_list_of_dicts(result, "Result")
        self.assertCountEqual([1], result)

    def test__should_remove_duplication_from_the_original_call(self):
        result = self.prolog.query(unique_call(
            X, member(X, [1, 1]), Result
        ))
        result = remove_trailing_false_or_true(result)
        result = get_list_from_list_of_dicts(result, "Result")
        self.assertCountEqual([1], result)

    def test__should_remove_duplication_from_the_original_call__preserve_distinct_values(self):
        result = self.prolog.query(unique_call(
            X, member(X, [1, 1, 2, 2, 3, 3]), Result
        ))
        result = remove_trailing_false_or_true(result)
        result = get_list_from_list_of_dicts(result, "Result")
        self.assertCountEqual([1, 2, 3], result)  # don't care order

    def test__should_remove_duplication_from_the_original_call__multiple_param_at_once(self):
        self.prolog.add_facts('hzztime_defintitions', [
            fact(1, 2, 3),
            fact(0, 2, 3),
            fact(1, 2, 4),
            fact(0, 2, 4),
            fact(1, 3, 2),
            fact(0, 3, 4),
            fact(1, 3, 4),
        ])
        result = self.prolog.query(unique_call(
            [X, Y],
            fact(_, X, Y),
            [X1, Y1]
        ), print_query=True)
        result = remove_trailing_false_or_true(result)
        self.assertCountEqual([
            {'X1': 2, 'Y1': 3},
            {'X1': 2, 'Y1': 4},
            {'X1': 3, 'Y1': 2},
            {'X1': 3, 'Y1': 4},
        ], result)  # don't care order
