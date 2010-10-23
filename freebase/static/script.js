$(document).ready(function() {
	$('#identifyFromAnswer').attr("value","");
	$('#identifyAnswer').attr("value","");
	
	$('#identifyFromAnswer').keyup(function() {
		if (sanitise($('#identifyFromAnswer').val()) === sanitise($('#identifyFromHiddenAnswer').attr('text'))) {
			alert('Correct!!');
			location.reload();
		}
	});
	
	$('#identifyAnswer').keyup(function() {
		$('.hiddenAnswer').each(function(index, answer) {
			if (sanitise($('#identifyAnswer').val()) === sanitise($(answer).attr('text'))) {
				alert('Too bloody right');
			}
		});
	})
});

function sanitise(input) {
	return input.toLowerCase().replace(" ", "");
}