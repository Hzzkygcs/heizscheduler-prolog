from unittest import TestCase

from HzzProlog.PrologOutputProcessor import Tokenizer


class TestTokenizer(TestCase):
    def _instantiate(self, parameter):
        return Tokenizer(parameter)

    def test_tokenize__should_tokenize_integers_token_correctly(self):
        result = self._instantiate("123").tokenize
        self.assertEqual(["123"], result)

    def test_tokenize__should_tokenize_whitespace(self):
        result = self._instantiate(" ").tokenize
        self.assertEqual([" "], result)

    def test_tokenize__should_keep_number_of_whitespace(self):
        result = self._instantiate(" "*5).tokenize
        self.assertEqual([" "*5], result)

    def test_tokenize__should_tokenize_characters_correctly(self):
        result = self._instantiate("abc").tokenize
        self.assertEqual(["abc"], result)

    def test_tokenize__should_separate_integers_and_spaces(self):
        result = self._instantiate("123 456 789").tokenize
        self.assertEqual(["123", " ", "456", " ", "789"], result)

    def test_tokenize__should_separate_alphabets_and_spaces(self):
        result = self._instantiate("abc def ghi").tokenize
        self.assertEqual(["abc", " ", "def", " ", "ghi"], result)

    def test_tokenize__should_combine_alphabets_and_numbers(self):
        result = self._instantiate("abc123").tokenize
        self.assertEqual(["abc123"], result)
        result = self._instantiate("123abc").tokenize
        self.assertEqual(["123abc"], result)

    def test_tokenize__should_separate_alphanum_and_symbols(self):
        result = self._instantiate("abc123+def345").tokenize
        self.assertEqual(["abc123", "+", "def345"], result)
        result = self._instantiate("[abc123,def345]").tokenize
        self.assertEqual(["[", "abc123", ",", "def345", "]"], result)

    def test_tokenize__should_separate_adjacent_symbols(self):
        result = self._instantiate("10+(-12)").tokenize
        self.assertEqual(["10", "+", "(", "-", "12", ")"], result)

    def test_tokenize__should_separate_whitespace_and_symbols(self):
        result = self._instantiate("12 + 23 = 35").tokenize
        self.assertEqual(["12", " ", "+", " ", "23", " ", "=", " ", "35"], result)

    def test_tokenize__should_combine_underscore_and_alphabets(self):
        result = self._instantiate("ab_cd").tokenize
        self.assertEqual(["ab_cd"], result)

    def test_tokenize__custom_regex__should_separate_symbols_by_symbols_if_no_custom_regex(self):
        tokenizer = self._instantiate("Var1 = Var2, Var2 = 6:23:59")
        result = tokenizer.tokenize
        self.assertEqual(["Var1", ' ', '=', ' ', 'Var2', ',', ' ', 'Var2', ' ', '=', ' ', '6', ':', '23', ':', '59'], result)

    def test_tokenize__custom_regex__should_return_token_correctly(self):
        tokenizer = self._instantiate("Var1 = Var2, Var2 = 6:23:59")
        tokenizer.add_new_regex(100, '\d+:\d+:\d+')
        result = tokenizer.tokenize
        self.assertEqual(["Var1", ' ', '=', ' ', 'Var2', ',', ' ', 'Var2', ' ', '=', ' ', '6:23:59'], result)

    def test_tokenize__should_combine_underscore_alphabets_and_numbers(self):
        result = self._instantiate("ab_cd12").tokenize
        self.assertEqual(["ab_cd12"], result)
        result = self._instantiate("ab_12").tokenize
        self.assertEqual(["ab_12"], result)
        result = self._instantiate("ab___1_2").tokenize
        self.assertEqual(["ab___1_2"], result)




