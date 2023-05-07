from http import HTTPStatus

from global_exception.exceptions.AutomaticallyHandledException import AutomaticallyHandledException


class BadRequestException(AutomaticallyHandledException):
    def __init__(self, message):
        super(BadRequestException, self).__init__(HTTPStatus.BAD_REQUEST, message)

