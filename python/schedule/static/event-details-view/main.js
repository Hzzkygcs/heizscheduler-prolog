$(document).ready(function () {
    const parentEl = $(".list-of-schedules");
    const schedules = getSchedules();
    console.log(schedules)
    reloadListOfSchedule(schedules, parentEl);
})

function getSchedules(script_id='available-bookings'){
    const availBookings = load_json_data_from_script_tag(script_id);
    const convertedAvailBookings = {};

    for (const user of availBookings) {
        const userNpm = user.user;
        convertedAvailBookings[userNpm] = [];

        for (const availBooking of user.slots) {
            let start = new Date(availBooking.start);
            let end = new Date(availBooking.end);
            const startSplitted = splitDateTime(start);
            const endSplitted = splitDateTime(end);

            console.assert(startSplitted.date.valueOf() === endSplitted.date.valueOf());
            convertedAvailBookings[userNpm].push({
                schedule: new Schedule(startSplitted.date, startSplitted.time, endSplitted.time),
                start: start,
                end: end,
                is_preferred: availBooking.is_preferred,
                booker_name: userNpm,
            });
        }
    }
    console.log(convertedAvailBookings)
    return convertedAvailBookings;
}


/**
 * @param bookings  --> dict[NPM, list[bookings]]
 * @param parentElement
 */
function reloadListOfSchedule(bookings, parentElement){
    console.log("reloaded");
    for (const userNpm of Object.keys(bookings)){
        const bookingsOfTheUser = bookings[userNpm];
        reloadListOfScheduleOfAUser(userNpm, bookingsOfTheUser);
    }
}
function reloadListOfScheduleOfAUser(npm, bookings){
    console.log(npm);
    bookings = bookings.sort((a, b) => SCHEDULE_SORT(a.schedule, b.schedule))
    const parentElement = newListItemOfSchedulesForEachNpmTemplate(npm);

    let index = 0;
    for (const booking of bookings) {
        console.log(booking.is_preferred)
        const newEl = instantiateItem(index, parentElement, booking,  booking.is_preferred, booking.booker_name)
        parentElement.append(newEl);
        index++;
    }
}
function newListItemOfSchedulesForEachNpmTemplate(npm) {
    const template = $(".list-of-schedules-for-each-npm-template");
    const ret = $(template.html());
    ret.addClass(`${npm}`);
    ret.find('.user-id').text(npm);

    $(".list-of-schedules-for-each-npm").append(ret);
    return ret.find('.list-of-schedules');
}



function instantiateItem(index, parentElement, booking, is_preferred, booker_name, on_click=null) {
    const schedule = booking.schedule;
    const date = dateObjToDateStringFormat(schedule.date);
    const startTime = schedule.startTime.toString();
    const endTime = schedule.endTime.toString();

    const newEl = initializeScheduleItem(date, startTime, endTime, false);
    newEl.click(on_click ?? (() => {}));
    if (is_preferred)
        newEl.addClass("starred");
    if (on_click == null)
        newEl.removeClass("clickable");
    return newEl;
}





function showModal(date, timeRepr){
    throw new Error("not implemented");

}
function hideModal(){
    $('#my-modal').css('display', 'none');
}
function submitModal(startDateObj, endDateObj){
    throw new Error("not implemented");

}

