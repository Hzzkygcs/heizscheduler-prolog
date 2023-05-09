

function getSchedules(script_id='existed-schedules'){
    const availBookings = load_json_data_from_script_tag(script_id);
    const convertedAvailBookings = [];

    for (const availBooking of availBookings) {
        let start = new Date(availBooking.start);
        let end = new Date(availBooking.end);
        const startSplitted = splitDateTime(start);
        const endSplitted = splitDateTime(end);

        console.assert(startSplitted.date.valueOf() === endSplitted.date.valueOf());
        convertedAvailBookings.push({
            schedule: new Schedule(startSplitted.date, startSplitted.time, endSplitted.time),
            start: start,
            end: end,
            is_preferred: availBooking.is_preferred,
            booker_name: availBooking.booker_name,
        });
    }
    console.log(convertedAvailBookings)
    return convertedAvailBookings;
}


$(document).ready(() => {
    reloadListOfSchedule(schedules, $(LIST_OF_SCHEDULES_EL));
});



function validate(){
    if ($("#event_name").val().length == 0){
        throw new ValidationError("Event Name should not be null");
    }
}

const saveToServer =

function saveToServer(){
    commonSaveSchedulesToServer("/events/create", validate);
}