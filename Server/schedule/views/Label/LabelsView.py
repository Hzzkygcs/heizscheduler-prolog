from auth_module.core.decorator.AuthenticatedDecorator import authenticated
from auth_module.models import User
from django.shortcuts import render
from schedule.views.BaseScheduleView import BaseScheduleView


# Create your views here

class LabelsView(BaseScheduleView):
    @authenticated
    def get(self, req, logged_in_user: User):
        events = logged_in_user.get_list_of_events()
        print(events)
        return render(req, "labels/label-list.html", {})