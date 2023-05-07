from datetime import datetime
from django.utils import timezone



def batch_convert_to_datetime(array_of_schedules):
    for arr_item in array_of_schedules:
        for key in ("start", "end"):
            arr_item[key] = convert_to_datetime(arr_item[key])
    return array_of_schedules


def convert_to_datetime(date_time_str):
    date_time = datetime.strptime(date_time_str, "%Y-%m-%dT%H:%M:%S.%fZ")
    return timezone.make_aware(date_time)
