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
            'reason': {
                'error_code': self.__class__.__name__,
                'status_code': self.status_code,
                'message': self.err_msg,
            }
        })
        return self.HttpResponse(data, content_type="application/json", status=self.status_code)