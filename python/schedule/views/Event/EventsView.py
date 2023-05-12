from django.shortcuts import render, redirect
from django.urls import reverse

from auth_module.core.decorator.AuthenticatedDecorator import authenticated
from auth_module.models import User
from schedule.views.BaseScheduleView import BaseScheduleView


# Create your views here

class EventsView(BaseScheduleView):
    @authenticated
    def get(self, req, logged_in_user: User):
        event_data = []
        user_events = logged_in_user.event_set.all()
        for event in user_events:
            event_data.append({
                'name': event.name,
                'id': event.ID,
                'click_url': reverse('event_booking_details', args=(event.pk,))
            })

        return render(req, "events/event-list.html", {
            'event_data': event_data,
        })