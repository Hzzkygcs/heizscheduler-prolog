from unittest import TestCase
from unittest.mock import Mock

from auth_module.core.AuthManagement import AuthManagement
from auth_module.exceptions.InvalidTokenException import InvalidTokenException


class TestAuthManagement(TestCase):
    def setUp(self) -> None:
        self.auth_management = AuthManagement()

    def test__should_successfully_add_the_user(self):
        user = Mock()
        token = self.auth_management.register_token(user)
        res = self.auth_management.get_user(token)
        self.assertTrue(user is res)

    def test_register_token__should_generate_token_randomly(self):
        user = Mock()
        token1 = self.auth_management.register_token(user)
        token2 = self.auth_management.register_token(user)
        self.assertNotEquals(token1, token2)

    def test_get_user__should_throw_if_token_is_not_found(self):
        invalid_token = "a"
        self.assertRaises(InvalidTokenException,
                          lambda: self.auth_management.get_user(invalid_token))

