


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

$(document).ready(function () {
    $(".is-preferred").hide();
})