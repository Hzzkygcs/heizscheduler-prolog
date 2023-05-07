from http import HTTPStatus

from global_exception.exceptions.AutomaticallyHandledException import AutomaticallyHandledException


class InvalidBookingExceptions(AutomaticallyHandledException):
    def __init__(self):
        super().__init__(status_code=HTTPStatus.BAD_REQUEST, message="Invalid booking")

