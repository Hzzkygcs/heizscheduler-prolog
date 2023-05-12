function reload(eventsData, parentEl){
    console.log("reload")
    parentEl = $(parentEl);
    const eventItemTemplate = $("#event-item-template");

    for (const event of eventsData) {
        const newEl = $(eventItemTemplate.html());
        const {name, id, click_url} = event;
        newEl.find(".event-name").text(name);

        newEl.click(((click_url) => (e) => {
            window.location.href = click_url;
        })(click_url));

        const delBtn = newEl.find('.delete-btn');
        delBtn.click(((id) => (e) => {
            console.log(id);
            const isConfirmed = confirm(`Do you really want to delete this schedule?`);
            if (isConfirmed){
                deleteEvent(id);
            }
            e.stopPropagation();
        })(id));


        parentEl.append(newEl);
    }
}

function deleteEvent(eventId){
    $.ajax({
        url: '/events/' + eventId,
        type: 'DELETE',
        success: function(result) {
            console.assert(result.success === 1);
            location.reload();
        }
    });
}


$(document).ready((e) => {
    let eventsData = load_json_data_from_script_tag('events-data');
    reload(eventsData, $(".event-list"));
});