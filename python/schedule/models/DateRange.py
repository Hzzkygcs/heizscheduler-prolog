from __future__ import annotations

from datetime import timedelta

from django.core.exceptions import ValidationError
from django.db import models
from django.db.models import Model

from schedule.core.Intersection import Intersection
from schedule.exceptions.DateRangeIntersectionException import DateRangeIntersectionException


class DateRange(Model):

    ID = models.AutoField(primary_key=True)
    start_date_time = models.DateTimeField()
    end_date_time = models.DateTimeField()

    def to_dict(self):
        return {
            'start_date_time': self.start_date_time,
            'end_date_time': self.end_date_time,
        }

    def intersection_status(self, other: DateRange) -> Intersection:
        if self.end_date_time <= other.start_date_time:
            return Intersection.BEFORE  # self comes before other
        if other.end_date_time <= self.start_date_time:
            return Intersection.AFTER  # self comes after other
        if other.start_date_time < self.start_date_time and self.end_date_time < other.end_date_time:
            return Intersection.INSIDE  # self is inside the other
        if self.start_date_time < other.start_date_time and other.end_date_time < self.end_date_time:
            return Intersection.CONTAINS  # self contains the other
        return Intersection.INTERSECT  # self contains the other

    def contains_or_equal(self, other: DateRange):
        if self.start_date_time <= other.start_date_time and other.end_date_time <= self.end_date_time:
            return True
        return False

    def more_or_less_the_same(self, other: DateRange, tolerance=timedelta(seconds=5)):
        diff = self.start_date_time - other.start_date_time
        if abs(diff) > tolerance:
            return False
        diff = self.end_date_time - other.end_date_time
        if abs(diff) > tolerance:
            return False
        return True


    @staticmethod
    def assert_no_intersection(date_ranges: list[DateRange]):
        for i in range(len(date_ranges)):
            first_range = date_ranges[i]
            for j in range(i+1, len(date_ranges)):
                second_range = date_ranges[j]
                if first_range.intersection_status(second_range).intersects_or_contains():
                    raise DateRangeIntersectionException()

    def clean(self):
        if self.start_date_time > self.end_date_time:
            raise ValidationError('Start date is after end date')

    def __repr__(self):
        return f"{repr(self.start_date_time)} {repr(self.end_date_time)}"