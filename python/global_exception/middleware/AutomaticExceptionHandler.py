from global_exception.exceptions.AutomaticallyHandledException import AutomaticallyHandledException


class AutomaticExceptionHandler:
    def __init__(self, get_response):
        self.get_response = get_response

    def __call__(self, req):
        return self.get_response(req)

    def process_exception(self, req, exception: Exception):
        if not isinstance(exception, AutomaticallyHandledException):
            return None
        auto_exception: AutomaticallyHandledException = exception
        return auto_exception.get_response(req)