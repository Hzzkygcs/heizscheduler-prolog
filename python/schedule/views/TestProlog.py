from django.http import HttpResponse
from django.shortcuts import render, redirect

from HzzProlog.HzzProlog import HzzProlog
from auth_module.core.decorator.AuthenticatedDecorator import authenticated
from definitions.paths import MAIN_PROLOG_PATH
from schedule.views.BaseScheduleView import BaseScheduleView


class TestProlog(BaseScheduleView):
    def get(self, req):

        return HttpResponse()
