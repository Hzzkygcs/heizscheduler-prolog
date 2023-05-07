from django.db import models
from django.db.models import Model
from django.db.models.signals import post_delete
from django.dispatch import receiver

from schedule.core.AvailableBookStrategy.AvailableBookStrategy import AvailableBookStrategy
from schedule.core.AvailableBookStrategy.ContiguousBookStrategy import ContiguousBookStrategy
from schedule.models.DateRange import DateRange
from schedule.models.Event import Event
from schedule.models.Label import Label


class Schedule(Model):
    ID = models.AutoField(primary_key=True)
    datetime_range = models.OneToOneField(DateRange, on_delete=models.CASCADE)
    label = models.ForeignKey(Label, on_delete=models.RESTRICT, null=True)
    event = models.ForeignKey(Event, on_delete=models.CASCADE)

    @property
    def start_date_time(self):
        return self.datetime_range.start_date_time

    @property
    def end_date_time(self):
        return self.datetime_range.end_date_time

    @property
    def available_book_strategy(self) -> AvailableBookStrategy:
        return ContiguousBookStrategy(self.event, self)

    def get_available_slots(self):
        return self.available_book_strategy.get_available_slots()



@receiver(post_delete, sender=Schedule)
def post_delete_book(sender, instance: Schedule, **kwargs):
    instance.datetime_range.delete()
