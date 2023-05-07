import queue
from inspect import ismethod, getfullargspec

from kink import di, inject

from auth_module.core.AuthManagement import AuthManagement
from auth_module.exceptions.NotLoggedInException import NotLoggedInException


# decorator
# accepts:  func(self, req, *args, logged_in_user, **kwargs)
# injects: logged_in_user
# returns callable: func(self, req, *args, **kwargs)

class AuthenticatedDecorator:
    def __init__(self, auth_management):
        self.__auth_management = auth_management

    @property
    def auth_management(self) -> AuthManagement:
        return self.__auth_management

    @auth_management.setter
    def auth_management(self, value: AuthManagement):
        self.__auth_management = value

    def __call__(self, method):
        args = getfullargspec(method).args
        is_method = False
        if len(args) > 1 and ("self" in args[0].lower()):
            is_method = True
        return self.__decorate(method, is_method)

    def __decorate(self, method_or_func, is_method):
        def wrapper(arg1, *args_list, **kwargs):
            req = arg1
            if is_method:
                req = args_list[0]  # args2

            logged_in_user = self.get_user_object(req, wrapper)
            args_list = args_list + (logged_in_user,)
            return method_or_func(arg1, *args_list, **kwargs)
        wrapper.user_mock = queue.Queue()
        wrapper.auth_management = self.auth_management
        def restore_auth_management():
            wrapper.auth_management = self.auth_management
        wrapper.restore_auth_management = restore_auth_management
        wrapper.add_user_mock = lambda user_mock: wrapper.user_mock.put(user_mock)
        def set_user_mock(user_mock):
            wrapper.user_mock = queue.Queue()
            wrapper.add_user_mock(user_mock)
        wrapper.set_user_mock = set_user_mock

        return wrapper

    def get_user_object(self, req, wrapper):
        if not wrapper.user_mock.empty():  # for mocking purpose
            return wrapper.user_mock.get()
        if 'token' not in req.COOKIES:
            raise NotLoggedInException()
        token = req.COOKIES['token']
        return wrapper.auth_management.get_user(token)



def authenticated(func):
    decorator = AuthenticatedDecorator(di[AuthManagement])
    return decorator(func)