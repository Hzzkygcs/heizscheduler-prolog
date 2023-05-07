import django
django.setup()


from unittest import TestCase

from auth_module.core.Factory.UserFactory import UserFactory
from auth_module.models import User


class TestUserFactory(TestCase):
    def test_create_user__should_create_new_user_correctly(self):
        factory = UserFactory()
        email = "a"
        password = b"b"

        new_user = factory.create_user(email, password)
        self.assertTrue(isinstance(new_user, User))
        self.assertTrue(new_user.is_password_valid(password))

    def test_create_user__should_create_new_user_correctly__negative_test(self):
        factory = UserFactory()
        email = "a"
        password = b"b"

        new_user = factory.create_user(email, password)
        self.assertTrue(isinstance(new_user, User))
        self.assertFalse(new_user.is_password_valid(b'false'))

    def test_create_user__should_accept_string_as_password(self):
        factory = UserFactory()
        email = "a"
        password = "b"
        new_user = factory.create_user(email, password)
        self.assertTrue(isinstance(new_user, User))
        self.assertTrue(new_user.is_password_valid(password.encode()))