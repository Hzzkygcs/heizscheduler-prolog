from unittest import TestCase
from unittest.mock import Mock

from auth_module.core.decorator.AuthenticatedDecorator import AuthenticatedDecorator
from auth_module.exceptions.NotLoggedInException import NotLoggedInException


class TestAuthenticatedDecorator(TestCase):
    def setUp(self) -> None:
        self.auth_management = Mock()
        self.authenticated = AuthenticatedDecorator(self.auth_management)

    def test_auth_management__getter__should_return_correctly(self):
        self.assertTrue(self.authenticated.auth_management is self.auth_management)

    def test_auth_management__setter__should_set_correctly(self):
        auth_management = Mock()
        self.authenticated.auth_management = auth_management
        self.assertTrue(self.authenticated.auth_management is auth_management)

    def test__function_decorate__should_decorate_correctly(self):
        user = Mock()
        req = Mock()
        token = 'token'
        req.COOKIES = {'token': token}
        self.auth_management.get_user.return_value = user

        @self.authenticated
        def example(req_param, arg1, arg2, logged_in_user):
            self.assertTrue(req_param is req)
            self.assertEqual('b', arg1)
            self.assertEqual('c', arg2)
            self.assertTrue(logged_in_user is user)
            return 1

        ret = example(req, 'b', 'c')
        self.assertEqual(1, ret)
        self.auth_management.get_user.assert_called_with(token)


    def test__method_decorate__should_decorate_correctly(self):
        user = Mock()
        req = Mock()
        token = 'token'
        req.COOKIES = {'token': token}
        self.auth_management.get_user.return_value = user

        class SomeClass:
            @self.authenticated
            def example(inner_self, req_param, arg1, arg2, logged_in_user):
                self.assertTrue(inner_self is some_class)
                self.assertTrue(req_param is req)
                self.assertEqual('b', arg1)
                self.assertEqual('c', arg2)
                self.assertTrue(logged_in_user is user)
                return 1
        some_class = SomeClass()
        ret = some_class.example(req, 'b', 'c')

        self.assertEqual(1, ret)
        self.auth_management.get_user.assert_called_with(token)


    def test__should_throw_if_token_not_found(self):
        req = Mock()
        req.COOKIES = {}

        @self.authenticated
        def example(req, logged_in_user):
            return 1
        self.assertRaises(NotLoggedInException, lambda: example(req, 'b', 'c'))

    def test_add_user_mock__should_allow_adding_mock(self):
        user1 = Mock()
        user2 = Mock()
        req = Mock()
        req.COOKIES = {}

        received_users = []
        @self.authenticated
        def example(req, logged_in_user):
            received_users.append(logged_in_user)

        example.add_user_mock(user1)
        example.add_user_mock(user2)
        example(req)
        self.assertTrue(received_users.pop(0) is user1)
        example(req)
        self.assertTrue(received_users.pop(0) is user2)

    def test_set_user_mock__should_allow_setting_mock(self):
        user1 = Mock()
        user2 = Mock()
        req = Mock()
        req.COOKIES = {}

        received_users = []

        @self.authenticated
        def example(req, logged_in_user):
            received_users.append(logged_in_user)

        example.add_user_mock(user1)
        example.set_user_mock(user2)
        example(req)
        self.assertTrue(received_users.pop(0) is user2)

    def test_add_user_mock__should_call_auth_management_if_queue_empty(self):
        user1 = Mock()
        user2 = Mock()
        req = Mock()
        token = 'token'
        req.COOKIES = {'token': token}
        self.auth_management.get_user.return_value = user2

        received_users = []

        @self.authenticated
        def example(req, logged_in_user):
            received_users.append(logged_in_user)

        example.add_user_mock(user1)
        example(req)
        self.assertTrue(received_users.pop(0) is user1)
        example(req)
        self.assertTrue(received_users.pop(0) is user2)
        self.auth_management.get_user.assert_called_with(token)


    def test_set_user_mock__should_call_auth_management_if_queue_empty(self):
        user1 = Mock()
        user2 = Mock()
        req = Mock()
        token = 'token'
        req.COOKIES = {'token': token}
        self.auth_management.get_user.return_value = user2

        received_users = []

        @self.authenticated
        def example(req, logged_in_user):
            received_users.append(logged_in_user)

        example.set_user_mock(user1)
        example(req)
        self.assertTrue(received_users.pop(0) is user1)
        example(req)
        self.assertTrue(received_users.pop(0) is user2)
        self.auth_management.get_user.assert_called_with(token)



    def test_restore_auth_management__should_reset_auth_management(self):
        @self.authenticated
        def example(req, logged_in_user):
            return 0
        auth_management_before = example.auth_management

        example.auth_management = 0
        self.assertEqual(0, example.auth_management)

        example.restore_auth_management()
        auth_management_after = example.auth_management
        self.assertTrue(auth_management_before is auth_management_after)