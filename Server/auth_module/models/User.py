import hashlib

from auth_module.core.RandomFactory import RandomFactory
from auth_module.exceptions.InvalidPasswordException import InvalidPasswordException
from django.db import models
from django.db.models import Model


class User(Model):
    app_label = 'auth_manager'

    email = models.EmailField(primary_key=True)
    _password = models.BinaryField(editable=True, max_length=128)
    _salt = models.BinaryField(editable=True, max_length=4)

    _salt_factory = RandomFactory()

    def get_list_of_events(self):
        return self.event_set.all()


    @property
    def password(self) -> bytes:
        if isinstance(self._password, memoryview):
            return self._password.tobytes()
        return self._password

    @property
    def salt(self) -> bytes:
        if isinstance(self._salt, memoryview):
            return self._salt.tobytes()
        return self._salt

    @password.setter
    def password(self, passw: bytes):
        if isinstance(passw, str):
            passw = passw.encode('utf-8')
        self._salt = self._salt_factory.random_bytes()
        self._password = hashlib.sha512(self._salt + passw).digest()

    def is_password_valid(self, password):
        if isinstance(password, str):
            password = password.encode('utf-8')
        salt = self.salt
        hashed = hashlib.sha512(salt + password).digest()
        return hashed == self.password

    def validate_password(self, password):
        if not self.is_password_valid(password):
            raise InvalidPasswordException()