from http import HTTPStatus

from requests import status_codes

from global_exception.exceptions.AutomaticallyHandledException import AutomaticallyHandledException


class IncompleteDataException(AutomaticallyHandledException):
    def __init__(self, field_name):
        super().__init__(
            HTTPStatus.BAD_REQUEST,
            f"Field {field_name} is required"
        )

def validate_fields_exist(dct, field_names):
    for field_name in field_names:
        validate_field_exist(dct, field_name)

def validate_field_exist(dct, field_name):
    if field_name not in dct:
        raise IncompleteDataException(field_name)