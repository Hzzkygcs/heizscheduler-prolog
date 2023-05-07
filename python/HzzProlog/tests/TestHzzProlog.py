import unittest

from HzzProlog.HzzProlog import HzzProlog


class TestHzzProlog(unittest.TestCase):
    def setUp(self) -> None:
        self.p = HzzProlog(None)

    def test_query__should_return_true_for_true_query(self):
        result = self.p.query("true")
        self.assertEqual(["true"], result)

    def test_query__should_return_true_for_false_query(self):
        result = self.p.query("false")
        self.assertEqual(["false"], result)

    def test_query__should_show_list_completely(self):
        result = self.p.query("X=[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18]")
        self.assertEqual([{
            'X': [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18]
        }], result)

    def test_query__should_return_correctly(self):
        result = self.p.query("append(A, B, [1,2,3])")
        self.assertEqual([
            {'A': [], 'B': [1, 2, 3], },
            {'A': [1], 'B': [2, 3], },
            {'A': [1, 2], 'B': [3], },
            {'A': [1, 2, 3], 'B': [], },
            "false"
        ], result)

