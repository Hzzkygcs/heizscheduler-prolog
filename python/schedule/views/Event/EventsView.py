from django.http import HttpResponse
from django.shortcuts import render, redirect
from django.views import View
from kink import di, inject

from auth_module.core.AuthManagement import AuthManagement
from auth_module.core.Factory.UserFactory import UserFactory
from auth_module.core.decorator.AuthenticatedDecorator import authenticated
from auth_module.core.repository.UserRepository import UserRepository
from auth_module.models import User
from schedule.views.BaseScheduleView import BaseScheduleView


# Create your views here

class EventsView(BaseScheduleView):
    @authenticated
    def get(self, req, logged_in_user: User):
        events = logged_in_user.get_list_of_events()
        print(events)

        event_data = []
        user_events = logged_in_user.event_set.all()
        for event in user_events:
            event_data.append({
                'name': event.name,
                'id': event.ID
            })

        return render(req, "events/event-list.html", {
            'event_data': event_data,
        })