from http import HTTPStatus

from global_exception.exceptions.AutomaticallyHandledException import AutomaticallyHandledException


class UsernameAlreadyExists(AutomaticallyHandledException):
    def __init__(self):
        super().__init__(
            HTTPStatus.BAD_REQUEST,
            "Username already exists"
        )