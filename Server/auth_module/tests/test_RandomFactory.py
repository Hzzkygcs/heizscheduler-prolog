from unittest import TestCase

from auth_module.core.RandomFactory import RandomFactory


class TestRandomFactory(TestCase):
    def setUp(self) -> None:
        self.factory = RandomFactory()

    def test_generate_salt__should_return_correct_length(self):
        length = 10
        res = self.factory.random_bytes(length)
        self.assertEqual(length, len(res))

    def test_generate_salt__should_return_randomly(self):
        length = 15
        res1 = self.factory.random_bytes(length)
        res2 = self.factory.random_bytes(length)
        self.assertNotEqual(res1, res2)

    def test_random_string__should_return_correct_length(self):
        length = 10
        res = self.factory.random_string(length)
        self.assertEquals(length, len(res))

    def test_random_string__should_return_randomly(self):
        length = 15
        res1 = self.factory.random_string(length)
        res2 = self.factory.random_string(length)
        self.assertNotEquals(res1, res2)