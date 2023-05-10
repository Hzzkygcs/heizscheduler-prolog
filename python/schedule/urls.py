from django.urls import path

from schedule.views.Booking.BookingDetail import BookingDetail
from schedule.views.Event.BookingResults import BookingResultsViews
from schedule.views.Event.EventCreate import EventCreate
from schedule.views.Event.EventEdit import EventEdit
from schedule.views.Event.EventDetail import EventDetail
from schedule.views.Event.EventsView import EventsView
from schedule.views.Event.ScheduledEventsView import ScheduledEventsViews
from schedule.views.HomePageView import HomePageView
from schedule.views.Label.LabelsView import LabelsView
from schedule.views.PrologDemo import PrologDemo
from schedule.views.PrologDemo2 import PrologDemo2

urlpatterns = [
    path('events/', EventsView.as_view(), name='event_list'),
    path('prolog-demo/', PrologDemo.as_view(), name='prolog-demo'),
    path('prolog-demo/<int:event_id>', PrologDemo2.as_view(), name='prolog-demo'),

    path('events/create', EventCreate.as_view(), name='event_create'),
    path('events/edit/<int:event_id>', EventEdit.as_view(), name='event_edit'),
    path('events/<int:event_id>', EventDetail.as_view(), name='event_create'),
    path('events/result/<int:event_id>', BookingResultsViews.as_view(), name='booking_results'),

    path('scheduled-events', ScheduledEventsViews.as_view(), name='scheduled_event_list'),
    path('booking/<int:event_id>', BookingDetail.as_view(), name='event_booking_details'),
    path('labels', LabelsView.as_view(), name='label_list'),
    path('', HomePageView.as_view(), name='homepage'),
]

