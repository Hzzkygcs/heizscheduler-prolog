from django.db import models
from django.db.models import Model
from kink import inject

from schedule.models import Schedule
from schedule.models.DateRange import DateRange
from schedule.models.Event import Event
from schedule.models.Label import Label


@inject
class ScheduleRepository():
    def save(self, model):
        model.save()

    def create(self):
        return Schedule()