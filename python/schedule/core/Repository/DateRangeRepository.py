from __future__ import annotations
from django.core.exceptions import ValidationError
from django.db import models
from django.db.models import Model
from kink import inject

from schedule.core.Intersection import Intersection
from schedule.models import DateRange


@inject
class DateRangeRepository():
    def save(self, model):
        model.save()

    def create(self, start_date_time, end_date_time) -> DateRange:
        return DateRange(start_date_time=start_date_time, end_date_time=end_date_time)

    def create_and_save(self, start_date_time, end_date_time) -> DateRange:
        return DateRange.objects.create(start_date_time=start_date_time, end_date_time=end_date_time)
