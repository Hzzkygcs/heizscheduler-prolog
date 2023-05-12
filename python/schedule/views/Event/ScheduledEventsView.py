from django.shortcuts import render
from django.urls import reverse

from auth_module.core.decorator.AuthenticatedDecorator import authenticated
from auth_module.models import User
from schedule.models import BookingResult, Event
from schedule.views.BaseScheduleView import BaseScheduleView
from schedule.service.utils_for_event_details__my_schedule__booking_result import \
    get_template_data_for_event_details_and_booking_result, booking_result__or__schedule__to_dict


class ScheduledEventsViews(BaseScheduleView):
    def __init__(self):
        super().__init__()

    def setup(self, request, *args, **kwargs):
        super().setup(request, *args, **kwargs)

    @authenticated
    def get(self, req, logged_in_user: User):
        all_his_events = logged_in_user.event_set.all()
        all_his_appointments_as_event_host = []
        for event in all_his_events:
            all_his_appointments_as_event_host.extend(list(event.bookingresult_set.all()))

        his_schedules = list(logged_in_user.bookingresult_set.all())
        his_schedules += all_his_appointments_as_event_host
        his_schedules.sort(key=by_start_time)
        schedules = []

        for booking_result in his_schedules:
            sub_dict = booking_result__or__schedule__to_dict(booking_result)
            sub_dict['onclick_redirect'] = reverse('booking_results', args=(booking_result.event_id,))

            partner_info = f"(host: {booking_result.event.owner.npm})"
            hosted_by_me = (booking_result.event.owner.npm == logged_in_user.pk)
            if hosted_by_me:
                partner_info = f"(host: me, with  {booking_result.owner.npm})"

            schedules.append({
                # must be unique
                'user': f"id {booking_result.event_id} - {booking_result.event.name}"
                        f" {partner_info}",
                'slots': [sub_dict]
            })


        return render(req, "events-detail/scheduled-events.html", {
            'event_id': -1,
            'schedules': schedules,
            'event_name': "My Schedules",
        })


def by_start_time(booking_result: BookingResult):
    return booking_result.start_date_time
