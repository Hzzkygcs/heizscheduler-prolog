from django.db import models
from django.db.models import Model
from colorfield.fields import ColorField
from kink import inject

from schedule.models import Label


@inject
class LabelRepository():
    def save(self, model):
        model.save()

    def create(self):
        return Label()