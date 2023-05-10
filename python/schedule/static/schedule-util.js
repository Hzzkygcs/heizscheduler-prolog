const SCHEDULE_SORTING_FUNCTION = (a, b) => a.compare(b);

function initializeScheduleItem(date, startTime, endTime, deletable=false, selector="#item-template") {
    const newEl = $($(selector).html());

    newEl.find(".date").text(date);
    newEl.find(".start-time").text(startTime);
    newEl.find(".end-time").text(endTime);

    if (deletable)
        newEl.addClass("deletable");

    return newEl;
}


function load_json_data_from_script_tag(script_id){
    let text_content = document.getElementById(script_id).textContent;
    return JSON.parse(text_content);
}