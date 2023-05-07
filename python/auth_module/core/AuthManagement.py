import pickle

from cachetools import TTLCache
import os

from django.conf import settings
from kink import inject

from auth_module.core.RandomFactory import RandomFactory
from auth_module.exceptions.InvalidTokenException import InvalidTokenException



DEVELOPMENT = True
# DEVELOPMENT = DEVELOPMENT and not settings.TESTING


@inject
class AuthManagement:
    instance = None

    @staticmethod
    def get_instance():
        if AuthManagement.instance is None:
            AuthManagement.instance = AuthManagement()
        return AuthManagement.instance

    def __init__(self):
        self._random_factory = RandomFactory()
        ONE_HOUR = 60*60
        ONE_DAY = 24 * ONE_HOUR
        ONE_MONTH = 31 * ONE_DAY
        INF = float('inf')
        self._token_to_user_mapping = TTLCache(ttl=ONE_MONTH, maxsize=INF)
        if DEVELOPMENT:
            self.development_init()

    def development_init(self):
        self._token_to_user_mapping = {}
        file_path = "token.pkl"
        if os.path.exists(file_path):
            with open(file_path, 'rb') as f:
                self._token_to_user_mapping = pickle.load(f)

    def development_save(self):
        if not DEVELOPMENT:
            return
        with open("token.pkl", 'wb') as f:
            pickle.dump(self._token_to_user_mapping, f)

    def get_user(self, token: str):
        if token not in self._token_to_user_mapping:
            raise InvalidTokenException()
        return self._token_to_user_mapping[token]

    def register_token(self, user):
        token = self.__generate_random_token()
        self._token_to_user_mapping[token] = user
        self.development_save()
        return token

    def delete_token(self, token):
        del self._token_to_user_mapping[token]

    def __generate_random_token(self) -> str:
        while True:
            random_token = self._random_factory.random_string(40)
            if random_token not in self._token_to_user_mapping:
                break
        return random_token


AuthManagement.get_instance()  # initialize
