from auth_module.core.decorator.AuthenticatedDecorator import authenticated
from auth_module.models import User
from django.shortcuts import render
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