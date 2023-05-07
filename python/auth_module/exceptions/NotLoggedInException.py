from http import HTTPStatus

from django.http import HttpResponseBase
from django.shortcuts import redirect

from global_exception.exceptions.AutomaticallyHandledException import AutomaticallyHandledException


class NotLoggedInException(AutomaticallyHandledException):
    def __init__(self):
        super().__init__(
            HTTPStatus.UNAUTHORIZED,
            "Please Log In"
        )

    def get_response(self, _req) -> HttpResponseBase:
        return redirect('login')