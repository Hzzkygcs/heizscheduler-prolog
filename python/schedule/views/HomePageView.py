from django.shortcuts import render, redirect

from auth_module.core.decorator.AuthenticatedDecorator import authenticated
from schedule.views.BaseScheduleView import BaseScheduleView


class HomePageView(BaseScheduleView):
    @authenticated
    def get(self, req, user):
        return redirect('event_list')
