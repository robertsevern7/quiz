function listAlbums (band) {
    jQuery.ajax({
        success: function(msg) {
            $('#output').text('');
            if (msg['error'] !== undefined) {
                $('#output').text(msg['error']);
            } else {
                $(msg['name']).each(function(idx,val) {
                  $('#output').append('<li>' + val + '</li>');
                });
            }
        },
        url: '/albums/' + band
    });
}

$(document).ready(function() {
	$('#answer').keyup(function() {
		if (sanitise($('#answer').val()) === sanitise($('#hiddenanswer').attr('text'))) {
			alert('Correct!!');
			location.reload();
		}
	})
});

function sanitise(input) {
	return input.toLowerCase().replace(" ", "");
}