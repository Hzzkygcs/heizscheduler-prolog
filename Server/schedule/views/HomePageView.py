from auth_module.core.decorator.AuthenticatedDecorator import authenticated
from django.shortcuts import redirect
from schedule.views.BaseScheduleView import BaseScheduleView


class HomePageView(BaseScheduleView):
    @authenticated
    def get(self, req, user):
        return redirect('event_list')
