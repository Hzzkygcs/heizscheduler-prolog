from http import HTTPStatus

from django.http import HttpResponseBase
from django.shortcuts import redirect

from global_exception.exceptions.AutomaticallyHandledException import AutomaticallyHandledException


class InvalidTokenException(AutomaticallyHandledException):
    def __init__(self):
        super().__init__(
            HTTPStatus.BAD_REQUEST,
            "Invalid Token"
        )

    def get_response(self, _req) -> HttpResponseBase:
        return redirect('login')