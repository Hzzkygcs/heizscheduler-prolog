from http import HTTPStatus

from requests import status_codes

from global_exception.exceptions.AutomaticallyHandledException import AutomaticallyHandledException


class InvalidUsernameException(AutomaticallyHandledException):
    def __init__(self, message):
        super().__init__(
            HTTPStatus.BAD_REQUEST,
            message=message
        )

