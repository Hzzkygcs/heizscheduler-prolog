from http import HTTPStatus

from global_exception.exceptions.AutomaticallyHandledException import AutomaticallyHandledException


class InvalidImpossibleScheduleException(AutomaticallyHandledException):
    def __init__(self):
        super().__init__(status_code=HTTPStatus.BAD_REQUEST,
                         message="Your schedule is rejected: "
                                 "no schedule combinations could be generated to satisfy your schedules")

