from unittest import TestCase

from PrologOutputProcessor import PrologOutputProcessor


class TestPrologOutputProcessor(TestCase):
    def instantiate(self, string):
        return PrologOutputProcessor(string)

    def setUp(self) -> None:
        self.end_delimiter = "BACKTRACK"

    def test_process_token__allow_endDelimiter_prefix(self):
        instance = self.instantiate("BACKTRACK something.")
        result = instance.process_token(splitter_token=self.end_delimiter)
        self.assertEqual(["something"], result)

    def test_process_token__allow_no_endDelimiter_prefix(self):
        instance = self.instantiate("something.")
        result = instance.process_token(splitter_token=self.end_delimiter)
        self.assertEqual(["something"], result)

    def test_process_token__should_process_false_correctly(self):
        instance = self.instantiate("BACKTRACK false.")
        result = instance.process_token(splitter_token=self.end_delimiter)
        self.assertEqual(["false"], result)

    def test_process_token__should_process_true_correctly(self):
        instance = self.instantiate("BACKTRACK true.")
        result = instance.process_token(splitter_token=self.end_delimiter)
        self.assertEqual(["true"], result)

    def test_process_token__should_process_variables_correctly(self):
        instance = self.instantiate("BACKTRACK Var1 = value1 BACKTRACK false.")
        result = instance.process_token(splitter_token=self.end_delimiter)
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
        result = instance.process_token(splitter_token=self.end_delimiter)
        self.assertEqual([
            {'Var1': 'value1'},
            {'Var1': 'value2'},
            {'Var1': 'value3'},
            "true",
        ], result)

    def test_process_token__should_process_variables_with_multiple_variables_correctly(self):
        instance = self.instantiate("BACKTRACK Var1 = value1a, Var2 = value2a BACKTRACK false.")
        result = instance.process_token(splitter_token=self.end_delimiter)
        expected = [
            {'Var1': 'value1a', 'Var2': 'value2a'},
            "false",
        ]
        self.assertEqual(expected, result)

    def test_process_token__should_process_variables_with_chaining_multiple_variables_correctly(self):
        instance = self.instantiate("BACKTRACK Var1 = Var2 = Var3 = some_value BACKTRACK false.")
        result = instance.process_token(splitter_token=self.end_delimiter)
        expected = [
            {'Var1': 'some_value', 'Var2': 'some_value', 'Var3': 'some_value'},
            "false",
        ]
        self.assertEqual(expected, result)

    def test_process_token__should_process_variables_with_multiple_variables_and_multiple_backtracks_correctly(self):
        instance = self.instantiate(
            "BACKTRACK Var1=value1a, Var2=value2a"
            + " BACKTRACK Var1=value1b, Var2=value2b"
            + " BACKTRACK true."
        )
        result = instance.process_token(splitter_token=self.end_delimiter)
        self.assertEqual([
            {'Var1': 'value1a', 'Var2': 'value2a'},
            {'Var1': 'value1b', 'Var2': 'value2b'},
            "true",
        ], result)

    def test_process_token__should_handle_False_not_preceded_by_BACKTRACK(self):
        instance = self.instantiate(
            "BACKTRACK Var1=value1a, Var2=value2a"
            + " BACKTRACK Var1=value1b, Var2=value2b"
            + " false."  # usually false is not preceded by BACKTRACK
        )
        # in this case, we do not care whether the result is ended with False or not
        result = instance.process_token(splitter_token=self.end_delimiter)
        self.assertEqual([
            {'Var1': 'value1a', 'Var2': 'value2a'},
            {'Var1': 'value1b', 'Var2': 'value2b'},
        ], result[:2])


    def test_process_value__should_return_correctly_for_integers(self):
        result = self.instantiate("123")._process_value()
        self.assertEqual(123, result)

    def test_process_value__should_return_correctly_for_float(self):
        result = self.instantiate("123.456")._process_value()
        self.assertEqual(123.456, result)

    def test_process_value__should_return_correctly_for_normal_atom(self):
        result = self.instantiate('abc')._process_value()
        self.assertEqual('abc', result)

    def test_process_value__should_return_correctly_for_string_atom(self):
        result = self.instantiate('"abc"')._process_value()
        self.assertEqual('abc', result)

    def test_process_value__should_return_correctly_for_string_atom_with_space(self):
        result = self.instantiate('"abc  def  ghi"')._process_value()
        self.assertEqual('abc  def  ghi', result)

    def test_process_value__should_return_correctly_for_string_atom_with_space_and_square_brackets(self):
        result = self.instantiate('"abc [def, ghi, jklm] ghi"')._process_value()
        self.assertEqual('abc [def, ghi, jklm] ghi', result)

    def test_process_value__should_return_correctly_for_string_atom_with_escape(self):
        result = self.instantiate('"abc \\n\\t\\" def"')._process_value()
        self.assertEqual('abc \n\t\" def', result)

    def test_process_value__should_return_correctly_for_empty_square_brackets(self):
        result = self.instantiate('[]')._process_value()
        self.assertEqual([], result)

    def test_process_value__should_return_correctly_for_single_item_square_brackets(self):
        result = self.instantiate('[123]')._process_value()
        self.assertEqual([123], result)

    def test_process_value__should_return_correctly_for_multi_item_square_brackets(self):
        result = self.instantiate('[123.5, "a b \\" cdef", 123, abc]')._process_value()
        self.assertEqual([123.5, "a b \" cdef", 123, 'abc'], result)

    def test_process_value__should_return_correctly_for_nested_brackets(self):
        instance = self.instantiate('[[[1], 2], [3, 4, "abc def"], [], [[[5], 6, 7], 8], [[[[[]]]]]]')
        result = instance._process_value()
        self.assertEqual([[[1], 2], [3, 4, "abc def"], [], [[[5], 6, 7], 8], [[[[[]]]]]], result)