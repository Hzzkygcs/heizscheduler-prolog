from django.urls import path

from schedule.views.Booking.BookingDetail import BookingDetail
from schedule.views.Event.EventCreate import EventCreate
from schedule.views.Event.EventEdit import EventEdit
from schedule.views.Event.EventDetail import EventDetail
from schedule.views.Event.EventsView import EventsView
from schedule.views.HomePageView import HomePageView
from schedule.views.Label.LabelsView import LabelsView
from schedule.views.TestProlog import TestProlog

urlpatterns = [
    path('events', EventsView.as_view(), name='event_list'),
    path('test', TestProlog.as_view(), name='event_list'),

    path('events/create', EventCreate.as_view(), name='event_create'),
    path('events/edit/<int:event_id>', EventEdit.as_view(), name='event_edit'),
    path('events/<int:event_id>', EventDetail.as_view(), name='event_create'),
    path('booking/<int:event_id>', BookingDetail.as_view(), name='event_create'),
    path('labels', LabelsView.as_view(), name='label_list'),
    path('', HomePageView.as_view(), name='homepage'),
]

