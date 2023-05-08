from http import HTTPStatus

from global_exception.exceptions.AutomaticallyHandledException import AutomaticallyHandledException


class EventNotFoundException(AutomaticallyHandledException):
    def __init__(self):
        super(EventNotFoundException, self).__init__(
            status_code=HTTPStatus.BAD_REQUEST, message="Given event ID is not found")

