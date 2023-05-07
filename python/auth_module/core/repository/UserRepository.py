from kink import inject

from auth_module.exceptions.InvalidUserException import InvalidUserException
from auth_module.exceptions.UsernameAlreadyExists import UsernameAlreadyExists
from auth_module.models import User


@inject
class UserRepository:
    def register_new_user(self, user: User):
        try:
            User.objects.get(npm=user.npm)
            raise UsernameAlreadyExists()
        except User.DoesNotExist:
            user.save()

    def find_user_by_npm(self, npm: str) -> User:
        try:
            return User.objects.get(npm=npm)
        except User.DoesNotExist:
            raise InvalidUserException()






