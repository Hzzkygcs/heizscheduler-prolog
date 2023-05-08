from django.db import models
from django.db.models import Model

from auth_module.models import User


class Event(Model):
    ID = models.AutoField(primary_key=True)
    name = models.CharField(max_length=25)
    owner = models.ForeignKey(User, on_delete=models.CASCADE)

    slot_book_minute_width = models.IntegerField()
