import json

from django.http import HttpResponseBase, HttpResponse


class AutomaticallyHandledException(Exception):
    def __init__(self, status_code: int, message):
        super(Exception, self).__init__(message)
        self.status_code = status_code
        self.err_msg = message
        self.HttpResponse = HttpResponse

    def get_response(self, _req) -> HttpResponseBase:
        data = json.dumps({
            'status_code': self.status_code,
            'err_msg': self.err_msg,
        })
        return self.HttpResponse(data, content_type="application/json", status=self.status_code)