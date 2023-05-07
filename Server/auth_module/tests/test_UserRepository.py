import django

from auth_module.exceptions.UsernameAlreadyExists import UsernameAlreadyExists

django.setup()


from django.test import TestCase

from auth_module.core.repository.UserRepository import UserRepository
from auth_module.models import User


class TestUserRepository(TestCase):
    def setUp(self) -> None:
        self.repository = UserRepository()

    def test_register_new_user__should_successfully_register_new_user(self):
        new_user = User()
        email = new_user.email = 'a'
        password = new_user.password = b'b'

        self.repository.register_new_user(new_user)
        registered_user = User.objects.get(email=email)
        self.assertEquals(email, registered_user.email)
        self.assertTrue(registered_user.is_password_valid(password))


    def test_register_new_user__should_throw_if_username_already_exists(self):
        new_user = User()
        new_user.email = 'a'
        new_user.password = b'b'
        self.repository.register_new_user(new_user)
        self.assertRaises(UsernameAlreadyExists, lambda: self.repository.register_new_user(new_user))


    def test_find_user_by_email(self):
        email = 'aaa'
        password = b'b'

        user = User.objects.create(email=email)
        user.password = password
        user.save()

        registered_user = self.repository.find_user_by_email(email)
        self.assertEquals(email, registered_user.email)
        self.assertTrue(registered_user.is_password_valid(password))
