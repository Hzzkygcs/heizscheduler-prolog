from unittest import TestCase

from PrologOutputProcessor import PrologOutputProcessor


class TestPrologOutputProcessor(TestCase):
    def instantiate(self, string):
        return PrologOutputProcessor(string)

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