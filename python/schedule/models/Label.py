from django.db import models
from django.db.models import Model
from colorfield.fields import ColorField


class Label(Model):
    ID = models.AutoField(primary_key=True)
    name = models.CharField(max_length=25)
    color = ColorField(default='#99e4ff')
    keterangan = models.CharField(max_length=200)