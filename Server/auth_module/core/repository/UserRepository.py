from auth_module.exceptions.InvalidUserException import InvalidUserException
from auth_module.exceptions.UsernameAlreadyExists import UsernameAlreadyExists
from auth_module.models import User
from kink import inject


@inject
class UserRepository:
    def register_new_user(self, user: User):
        try:
            User.objects.get(email=user.email)
            raise UsernameAlreadyExists()
        except User.DoesNotExist:
            user.save()

    def find_user_by_email(self, email: str) -> User:
        try:
            return User.objects.get(email=email)
        except User.DoesNotExist:
            raise InvalidUserException()






