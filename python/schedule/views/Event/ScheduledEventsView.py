from django.shortcuts import render
from django.urls import reverse

from auth_module.core.decorator.AuthenticatedDecorator import authenticated
from auth_module.models import User
from schedule.models import BookingResult
from schedule.views.BaseScheduleView import BaseScheduleView
from schedule.views.Event.BookingResults import get_template_data_for_event_details_and_booking_result, \
    booking_result__or__schedule__to_dict


class ScheduledEventsViews(BaseScheduleView):
    def __init__(self):
        super().__init__()

    def setup(self, request, *args, **kwargs):
        super().setup(request, *args, **kwargs)

    @authenticated
    def get(self, req, logged_in_user: User):
        his_booking_results = list(logged_in_user.bookingresult_set.all())
        his_booking_results.sort(key=by_start_time)
        schedules = []

        for booking_result in his_booking_results:
            sub_dict = booking_result__or__schedule__to_dict(booking_result)
            sub_dict['onclick_redirect'] = reverse('booking_results', args=(booking_result.event_id,))

            schedules.append({
                # must be unique
                'user': f"id {booking_result.event_id} - {booking_result.event.name} "
                        f"(by {booking_result.event.owner.npm})",
                'slots': [sub_dict]
            })


        return render(req, "events-detail/scheduled-events.html", {
            'event_id': -1,
            'schedules': schedules,
            'event_name': "My Schedules",
        })


def by_start_time(booking_result: BookingResult):
    return booking_result.start_date_time
