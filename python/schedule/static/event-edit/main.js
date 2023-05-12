
$(document).ready(() => {
    console.log("test");
    getExistingSchedules();
    reloadListOfSchedule(schedules, $(LIST_OF_SCHEDULES_EL));
});


function getExistingSchedules(script_id='existed-schedules'){
    const availBookings = load_json_data_from_script_tag(script_id);
    const convertedAvailBookings = [];

    for (const availBooking of availBookings) {
        let start = new Date(availBooking.start);
        let end = new Date(availBooking.end);
        const startSplitted = splitDateTime(start);
        const endSplitted = splitDateTime(end);

        console.assert(startSplitted.date.valueOf() === endSplitted.date.valueOf());

        const existing_schedule = new Schedule(
            startSplitted.date, startSplitted.time, endSplitted.time, availBooking.is_preferred);
        schedules.push(existing_schedule)
        schedules.sort(SCHEDULE_SORTING_FUNCTION)
    }
    console.log(schedules)
}



function saveToServer(){
    const event_id = load_json_data_from_script_tag('event_id')
    const url = `/events/edit/${event_id}` ;
    commonSaveSchedulesToServer(url, ()=>{}, ()=>{
        window.location = `/events/${event_id}`;
    });
}