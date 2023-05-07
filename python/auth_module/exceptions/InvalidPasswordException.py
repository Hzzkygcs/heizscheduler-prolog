from http import HTTPStatus

from requests import status_codes

from global_exception.exceptions.AutomaticallyHandledException import AutomaticallyHandledException


class InvalidPasswordException(AutomaticallyHandledException):
    def __init__(self):
        super().__init__(
            HTTPStatus.BAD_REQUEST,
            "Invalid password"
        )