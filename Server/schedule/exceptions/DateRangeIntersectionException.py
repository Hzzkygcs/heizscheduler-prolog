from http import HTTPStatus

from global_exception.exceptions.AutomaticallyHandledException import AutomaticallyHandledException


class DateRangeIntersectionException(AutomaticallyHandledException):
    def __init__(self):
        super(DateRangeIntersectionException, self).__init__(
            status_code=HTTPStatus.BAD_REQUEST, message="DateRange Intersection detected")

