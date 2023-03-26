from unittest import TestCase

from HzzProlog.ChainEquality import ChainedEquality
from HzzProlog.PrologOutputProcessor import PrologOutputProcessor, BinOp


class TestPrologOutputProcessor(TestCase):
    def instantiate(self, string):
        return PrologOutputProcessor(string, self.end_delimiter)

    def setUp(self) -> None:
        self.end_delimiter = "BACKTRACK"

    def test_process_token__allow_endDelimiter_prefix(self):
        instance = self.instantiate("BACKTRACK something.")
        result = instance.process_token()
        self.assertEqual(["something"], result)

    def test_process_token__allow_no_endDelimiter_prefix(self):
        instance = self.instantiate("something.")
        result = instance.process_token()
        self.assertEqual(["something"], result)

    def test_process_token__should_process_false_correctly(self):
        instance = self.instantiate("BACKTRACK false.")
        result = instance.process_token()
        self.assertEqual(["false"], result)

    def test_process_token__should_process_true_correctly(self):
        instance = self.instantiate("BACKTRACK true.")
        result = instance.process_token()
        self.assertEqual(["true"], result)

    def test_process_token__should_process_variables_correctly(self):
        instance = self.instantiate("BACKTRACK Var1 = value1 BACKTRACK false.")
        result = instance.process_token()
        self.assertEqual([
            {'Var1': 'value1'},
            "false",
        ], result)

    def test_process_token__should_process_variables_in_multiple_backtracks_correctly(self):
        instance = self.instantiate(
            "BACKTRACK Var1 = value1"
            + " BACKTRACK Var1 = value2"
            + " BACKTRACK Var1 = value3"
            + " BACKTRACK true.")
        result = instance.process_token()
        self.assertEqual([
            {'Var1': 'value1'},
            {'Var1': 'value2'},
            {'Var1': 'value3'},
            "true",
        ], result)

    def test_process_token__should_process_variables_with_multiple_variables_correctly(self):
        instance = self.instantiate("BACKTRACK Var1 = value1a, Var2 = value2a BACKTRACK false.")
        result = instance.process_token()
        expected = [
            {'Var1': 'value1a', 'Var2': 'value2a'},
            "false",
        ]
        self.assertEqual(expected, result)

    def test_process_token__should_process_variables_with_chained_equality_correctly(self):
        instance = self.instantiate("BACKTRACK Var1 = Var2 = Var3 false.")  # note no BACKTRACK before the false.
        result = instance.process_token()
        equality = ChainedEquality(["Var1", "Var2", "Var3"])
        expected = [
            {'Var1': equality, 'Var2': equality, 'Var3': equality},
            "false",
        ]
        self.assertEqual(expected, result)

    def test_process_token__should_process_variables_with_chained_equality_and_constant_value_correctly(self):
        instance = self.instantiate("BACKTRACK Var2a = Var2b = Var2c = some_value false.")
        result = instance.process_token()
        equality_var2 = ChainedEquality(["Var2a", "Var2b", "Var2c"], 'some_value')
        expected = [
            {'Var2a': equality_var2, 'Var2b': equality_var2, 'Var2c': equality_var2,},
            "false",
        ]
        self.assertEqual(expected, result)

    def test_process_token__should_process_variables_with_chained_equality_and_false_constant_value_correctly(self):
        instance = self.instantiate("BACKTRACK Var2a = Var2b = Var2c = false false.")
        result = instance.process_token()
        equality_var2 = ChainedEquality(["Var2a", "Var2b", "Var2c"], 'false')
        expected = [
            {'Var2a': equality_var2, 'Var2b': equality_var2, 'Var2c': equality_var2,},
            "false",
        ]
        self.assertEqual(expected, result)

    def test_process_token__should_process_variables_with_various_chained_equality(self):
        instance = self.instantiate("BACKTRACK Var1a = Var1b, Var2a = Var2b = Var2c = some_value BACKTRACK false.")
        result = instance.process_token()
        equality_var1 = ChainedEquality(["Var1a", "Var1b"])
        equality_var2 = ChainedEquality(["Var2a", "Var2b", "Var2c"], 'some_value')
        expected = [
            {
                'Var1a': equality_var1, 'Var1b': equality_var1,
                'Var2a': equality_var2, 'Var2b': equality_var2, 'Var2c': equality_var2,
            },
            "false",
        ]
        self.assertEqual(expected, result)

    def test_process_token__should_process_variables_with_multiple_variables_and_multiple_backtracks_correctly(self):
        instance = self.instantiate(
            "BACKTRACK Var1=value1a, Var2=value2a"
            + " BACKTRACK Var1=value1b, Var2=value2b"
            + " BACKTRACK true."
        )
        result = instance.process_token()
        self.assertEqual([
            {'Var1': 'value1a', 'Var2': 'value2a'},
            {'Var1': 'value1b', 'Var2': 'value2b'},
            "true",
        ], result)

    def test_process_token__should_handle_False_not_preceded_by_BACKTRACK(self):
        instance = self.instantiate(
            "BACKTRACK Var1=value1a, Var2=value2a"
            + " BACKTRACK Var1=value1b, Var2=value2b"
            + " false."  # usually `false` is not preceded by BACKTRACK
        )
        result = instance.process_token()
        self.assertEqual([
            {'Var1': 'value1a', 'Var2': 'value2a'},
            {'Var1': 'value1b', 'Var2': 'value2b'},
            "false"
        ], result)


    def test_process_token__should_be_able_to_handle_without_false_or_true(self):
        instance = self.instantiate('BACKTRACK\nSum = 2.\n\n\n')
        result = instance.process_token()
        self.assertEqual([{"Sum": 2}], result)

    def test_process_value__should_return_correctly_for_integers(self):
        result = self.instantiate("123").process_value()
        self.assertEqual(123, result)

    def test_process_value__should_be_able_to_handle_negatives(self):
        result = self.instantiate("-123").process_value()
        self.assertEqual(-123, result)

    def test_process_value__should_return_correctly_for_float(self):
        result = self.instantiate("123.456").process_value()
        self.assertEqual(123.456, result)

    def test_process_value__should_be_able_to_handle_negative_floats(self):
        result = self.instantiate("-123.456").process_value()
        self.assertEqual(-123.456, result)

    def test_process_value__should_be_able_to_differentiate_minus_and_negative(self):
        """should be performed in two steps"""
        inst = self.instantiate("123-123.456")
        result1 = inst.process_value()
        result2 = inst.process_value()
        self.assertEqual(123, result1)
        self.assertEqual(BinOp("-", 123, 123.456), result2)

    def test_process_value__should_return_correctly_for_normal_atom(self):
        result = self.instantiate('abc').process_value()
        self.assertEqual('abc', result)

    def test_process_value__should_return_correctly_for_string_atom(self):
        result = self.instantiate('"abc"').process_value()
        self.assertEqual('abc', result)

    def test_process_value__should_return_correctly_for_string_atom_with_space(self):
        result = self.instantiate('"abc  def  ghi"').process_value()
        self.assertEqual('abc  def  ghi', result)

    def test_process_value__should_return_correctly_for_string_atom_with_space_and_square_brackets(self):
        result = self.instantiate('"abc [def, ghi, jklm] ghi"').process_value()
        self.assertEqual('abc [def, ghi, jklm] ghi', result)

    def test_process_value__should_return_correctly_for_string_atom_with_escape(self):
        result = self.instantiate('"abc \\n\\t\\" def"').process_value()
        self.assertEqual('abc \n\t\" def', result)

    def test_process_value__should_return_correctly_for_empty_square_brackets(self):
        result = self.instantiate('[]').process_value()
        self.assertEqual([], result)

    def test_process_value__should_return_correctly_for_single_item_square_brackets(self):
        result = self.instantiate('[123]').process_value()
        self.assertEqual([123], result)

    def test_process_value__should_return_correctly_for_multi_item_square_brackets(self):
        result = self.instantiate('[123.5, "a b \\" cdef", 123, abc]').process_value()
        self.assertEqual([123.5, "a b \" cdef", 123, 'abc'], result)

    def test_process_value__should_return_correctly_for_nested_brackets(self):
        instance = self.instantiate('[[[1], 2], [3, 4, "abc def"], [], [[[5], 6, 7], 8], [[[[[]]]]]]')
        result = instance.process_value()
        self.assertEqual([[[1], 2], [3, 4, "abc def"], [], [[[5], 6, 7], 8], [[[[[]]]]]], result)
