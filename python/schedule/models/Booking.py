from django.db import models
from django.db.models import Model

from auth_module.models import User
from schedule.models.DateRange import DateRange
from schedule.models.Schedule import Schedule


class Booking(Model):
    ID = models.AutoField(primary_key=True)

    name = models.CharField(max_length=30, null=True)
    datetime_range = models.OneToOneField(DateRange, on_delete=models.CASCADE)
    user = models.ForeignKey(User, null=True, on_delete=models.CASCADE)
    schedule = models.ForeignKey(Schedule, on_delete=models.CASCADE)

    @property
    def start_date_time(self):
        return self.datetime_range.start_date_time

    @property
    def end_date_time(self):
        return self.datetime_range.end_date_time


