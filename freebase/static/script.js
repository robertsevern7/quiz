$(document).ready(function() {
	$('#identifyFromAnswer').val("");
	$('#identifyAnswer').val("");
	
	$('#identifyFromAnswer').keyup(function() {
		if (sanitise($('#identifyFromAnswer').val()) === sanitise($('#identifyFromHiddenAnswer').attr('text'))) {
			alert('Correct!!');
			location.reload();
		}
	});
	
	$('#identifyAnswer').keyup(function() {
		$('.hiddenAnswer').each(function(index, answer) {
			if (sanitise($('#identifyAnswer').val()) === sanitise($(this).text())) {
				$(answer).removeClass('hiddenAnswer');
				$(answer).addClass('revealedAnswer');
				$('#identifyAnswer').val("");
				
				//TODO when only 1 remains show the next button
			}
		});
	});
    
    $('#revealAnswers').click(function() {
        $('.hiddenAnswer').each(function(index, answer) {
            $(answer).removeClass('hiddenAnswer');
            $(answer).addClass('revealedAnswer');
		});
    })
    
    $('#nextQuestion').click(function() {
        location.reload();
    })
});

function sanitise(input) {
	return input.toLowerCase().replace(" ", "");
}