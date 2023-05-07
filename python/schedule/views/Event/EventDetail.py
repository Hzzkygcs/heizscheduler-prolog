import json

from django.http import HttpResponse

from auth_module.core.Factory.UserFactory import UserFactory
from auth_module.core.decorator.AuthenticatedDecorator import authenticated
from auth_module.core.repository.UserRepository import UserRepository
from auth_module.models import User
from schedule.views.BaseScheduleView import BaseScheduleView


# Create your views here

class EventDetail(BaseScheduleView):
    @authenticated
    def delete(self, req, logged_in_user: User, event_id):
        event_to_be_deleted = logged_in_user.event_set.get(ID=event_id)
        event_to_be_deleted.delete()

        response = {'success': 1}
        return HttpResponse(json.dumps(response), content_type='application/json')



