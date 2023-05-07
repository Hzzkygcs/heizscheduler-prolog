




function initialize(el, schedules){
    let callendar = getCallendar(el);
    let callendarContent = callendar.find(".callendar-content");

    for (let minutes = 0; minutes < 96; minutes++) {
        let actualMinutes = 15*minutes;
        let hour = Math.floor(actualMinutes / 60);
        actualMinutes = actualMinutes % 60;
        let string = `${hour}:${actualMinutes}`;

        let new_div = $("<div>");
        new_div.css("grid-column", 1);
        new_div.css("grid-row", minutes+1);
        new_div.text(string);
        callendarContent.append(new_div);
    }

}

function getCallendar(el) {
    return $("#callendar");
}
