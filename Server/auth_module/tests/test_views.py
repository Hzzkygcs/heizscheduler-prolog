import mockito
from auth_module.core.repository.UserRepository import UserRepository
from django.test import TestCase
from kink import di


class TestRegisterView(TestCase):
    def setUp(self) -> None:
        self.user_repo_mock = mockito.mock(UserRepository)
        di[UserRepository] = self.user_repo_mock
        self.user_info = {"email": "a@gmail.com", "password": "b"}

    def test_get(self):
        self.client.get("/auth/register", self.user_info)
        self.user_repo_mock
