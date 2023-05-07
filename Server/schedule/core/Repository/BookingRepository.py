from kink import inject


@inject
class BookingRepository:
    
    def save(self, model):
        model.save()

    def create(self, booker_name, datetime_range_id, schedule_id):
        from schedule.models.Booking import Booking
        return Booking.objects.create(name=booker_name, datetime_range_id=datetime_range_id, schedule_id=schedule_id)

