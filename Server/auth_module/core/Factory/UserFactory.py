from typing import Union

from auth_module.models import User


class UserFactory:
    def create_user(self, email: str, password: Union[bytes, str]):
        user = User()
        user.email = email

        if isinstance(password, str):
            password = password.encode()
        user.password = password

        return user
