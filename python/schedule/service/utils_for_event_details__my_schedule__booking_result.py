import warnings
from typing import Any
from schedule.models import BookingResult


def apply_label_modifier(event_owner: str, logged_in: str,
                         data:  list[dict[str, Any]]):
    for datum in data:
        user = datum['user']
        datum['user'] = add_label_to_npm(event_owner, logged_in, user)
    return data


def add_label_to_npm(event_owner: str, logged_in: str, npm_to_be_modified: str):
    modifier = []
    if logged_in == npm_to_be_modified:
        modifier.append("Me")
    if event_owner == npm_to_be_modified:
        modifier.append("Host")
    if not modifier:
        return npm_to_be_modified
    return f"{npm_to_be_modified} ({', '.join(modifier)})"


def find_owner_and_move_to_top_of_dict(event_owner_npm: str,
                                       data:  list[dict[str, list[dict[str, Any]]]]):
    # Useless because the javascript's dictionary will scramble the ordering again
    owner_index = -1
    for index, user_and_slot in enumerate(data):
        if user_and_slot['user'] == event_owner_npm:
            owner_index = index
            break
    if owner_index == -1:
        warnings.warn("ERROR owner_index==-1")
        return data
    obj = data.pop(owner_index)
    data.insert(0, obj)
    return data


def get_penalty_score_from_booked_slots(booked_slots: list[BookingResult]):
    penalty_score = '-'
    if len(booked_slots) != 0:
        penalty_score = booked_slots[0].penalty_score
        for slot in booked_slots:
            assert penalty_score == slot.penalty_score
    return penalty_score


def get_template_data_for_event_details_and_booking_result(event, booked_slots):
    set_user = set()
    for slot in booked_slots:
        set_user.add(slot.owner.npm)

    users_and_slots = []

    for user in set_user:
        users_and_slots.append({
            'user': user,
            'slots': []
        })

    for slot in booked_slots:
        for booked_slot in users_and_slots:
            if booked_slot['user'] == slot.owner.npm:
                booked_slot['slots'].append(booking_result__or__schedule__to_dict(slot))

    return {
        'event_id': event.pk,
        'schedules': users_and_slots,
        'event_name': event.name,
    }


def booking_result__or__schedule__to_dict(slot):
    return {
        'start': slot.start_date_time,
        'end': slot.end_date_time,
        'is_preferred': slot.is_preferred,
    }
