import json

from schedule.models import DateRange


class BookingAvailability:  # not for database
    def __init__(self, schedule_id, datetime_range: DateRange, available: bool, booker_name=None):
        self.schedule_id = schedule_id
        self.datetime_range = datetime_range
        self.available = available
        self.booker_name = booker_name

    def to_dict(self):
        return {
            'available': self.available,
            'booker_name': self.booker_name,
            'schedule_id': self.schedule_id,
            'datetime_range': self.datetime_range.to_dict(),
        }

    def to_json(self):
        return json.dumps(self, default=lambda o: o.__dict__, sort_keys=True, indent=4)

