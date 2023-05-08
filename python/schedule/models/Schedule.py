from django.db import models
from django.db.models import Model
from django.db.models.signals import post_delete
from django.dispatch import receiver

from auth_module.models import User
from schedule.models.DateRange import DateRange
from schedule.models.Event import Event


class Schedule(Model):
    ID = models.AutoField(primary_key=True)
    datetime_range = models.OneToOneField(DateRange, on_delete=models.CASCADE)
    event = models.ForeignKey(Event, on_delete=models.CASCADE)
    booker = models.ForeignKey(User, on_delete=models.CASCADE)

    @property
    def start_date_time(self):
        return self.datetime_range.start_date_time

    @property
    def end_date_time(self):
        return self.datetime_range.end_date_time




@receiver(post_delete, sender=Schedule)
def post_delete_book(sender, instance: Schedule, **kwargs):
    instance.datetime_range.delete()
