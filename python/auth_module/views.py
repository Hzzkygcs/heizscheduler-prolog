import abc
import string

from django.http import HttpResponse, HttpResponseRedirect
from django.shortcuts import render, redirect
from django.urls import reverse
from django.views import View
from kink import di, inject

from auth_module.core.AuthManagement import AuthManagement
from auth_module.core.Factory.UserFactory import UserFactory
from auth_module.core.decorator.AuthenticatedDecorator import authenticated
from auth_module.core.repository.UserRepository import UserRepository
from auth_module.exceptions.IncompleteDataException import validate_fields_exist
from auth_module.exceptions.InvalidUsernameException import InvalidUsernameException
from auth_module.models import User


# Create your views here

class BaseAuthView(View):
    def __init__(self):
        super().__init__()
        self.__user_repository = di[UserRepository]
        self.__auth_management = di[AuthManagement]

    @property
    def user_repository(self) -> UserRepository:
        return self.__user_repository
    @user_repository.setter
    def user_repository(self, value: UserRepository):
        self.__user_repository = value
    @property
    def auth_management(self) -> AuthManagement:
        return self.__auth_management
    @auth_management.setter
    def auth_management(self, value: AuthManagement):
        self.__auth_management = value




class RegisterView(BaseAuthView):
    def __init__(self):
        super(RegisterView, self).__init__()
        self.__user_factory = UserFactory()

    @property
    def user_factory(self):
        return self.__user_factory
    @user_factory.setter
    def user_factory(self, value):
        self.__user_factory = value

    def get(self, req):
        return render(req, "auth/register.html", {})

    def post(self, req):
        validate_fields_exist(req.POST, ['npm', 'password'])
        username_or_npm = req.POST['npm']
        validate_username(username_or_npm)
        password = req.POST['password']
        new_user = self.user_factory.create_user(username_or_npm, password)
        self.user_repository.register_new_user(new_user)
        return redirect('login')


def validate_username(username):
    if len(username) > 15:
        raise InvalidUsernameException("Username cannot be longer than 15 characters")
    if len(username) <= 1:
        raise InvalidUsernameException("Username cannot smaller than 2 characters")
    consist_non_alphanum_char = bool(set(username) - set(string.ascii_lowercase + string.digits + "_"))
    if consist_non_alphanum_char:
        raise InvalidUsernameException("Username can only contain lower case alphanumeric and underscore")



@inject
class LoginView(BaseAuthView):
    def get(self, req):
        return render(req, "auth/login.html", {})

    def post(self, req):
        validate_fields_exist(req.POST, ['npm', 'password'])
        npm = req.POST['npm']
        password = req.POST['password']
        user = self.user_repository.find_user_by_npm(npm)
        user.validate_password(password)

        response = HttpResponseRedirect(reverse("event_list"))
        token = self.auth_management.register_token(user)
        response.set_cookie("npm", npm)
        response.set_cookie("token", token)
        return response



@inject
class LogoutView(BaseAuthView):
    @authenticated
    def get(self, req, _logged_in_user):
        token = req.COOKIES['token']

        response = HttpResponseRedirect(reverse("login"))
        self.auth_management.delete_token(token)
        response.delete_cookie("npm")
        response.delete_cookie("token")
        return response



