from auth_module.exceptions.InvalidPasswordException import InvalidPasswordException
from auth_module.models.User import User
from django.test import TestCase
from parameterized import parameterized

NEWLY_CREATED_OBJ = "newly-created-user"
MEMORYVIEWED_OBJ = "memoryviewed-obj"

class TestUser(TestCase):


    def setUp(self) -> None:
        self.email = "b"
        self.password = b"passw"

        user = User.objects.create(email=self.email)
        user.password = self.password
        user.save()

        memoryview_user = User.objects.get(email=self.email)
        memoryview_user._salt = memoryview(memoryview_user._salt)
        memoryview_user._password = memoryview(memoryview_user._password)
        self.user = user
        self.users = {
            NEWLY_CREATED_OBJ:        user,
            MEMORYVIEWED_OBJ:   memoryview_user
        }

    def test_object_saved_successfully(self):
        User.objects.create(email="a", _password=b'b')
        user = User.objects.get(email="a")
        self.assertIsNotNone(user)
        self.assertEqual(user.email, 'a')
        self.assertEqual(user.password, b'b')

    def test_password_should_not_be_stored_as_plaintext(self):
        self.assertNotEqual(self.user.password, self.password)
        self.assertNotEqual(self.user._password, self.password)

    def test__is_password_valid__should_return_false_if_password_is_wrong(self):
        user = self.user
        wrong_password = self.password + b"a"
        self.assertFalse(user.is_password_valid(wrong_password))

    @parameterized.expand([NEWLY_CREATED_OBJ, MEMORYVIEWED_OBJ])
    def test__is_password_valid__should_return_true_if_password_is_correct(self, user_type):
        user = self.users[user_type]
        self.assertTrue(user.is_password_valid(self.password))

    def test__is_password_valid__should_accept_string(self):
        user = self.user
        self.assertTrue(user.is_password_valid(self.password.decode('utf-8')))

    def test__validate_password__should_throw_if_password_is_wrong(self):
        wrong_password = self.password + b"a"
        self.assertRaises(InvalidPasswordException, lambda: self.user.validate_password(wrong_password))

    def test__validate_password__should_not_throw_if_password_is_correct(self):
        try:
            self.user.validate_password(self.password)
        except InvalidPasswordException:
            self.fail("InvalidPasswordException is raised unexpectedly!")
