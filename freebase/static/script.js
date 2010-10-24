$(document).ready(function() {
	$('#identifyFromAnswer').val("");
	$('#identifyAnswer').val("");
	
	$('#identifyFromAnswer').keyup(function() {
		if (sanitise($('#identifyFromAnswer').val()) === sanitise($('#identifyFromHiddenAnswer').attr('text'))) {
			alert('Correct!!');
			location.reload();
		}
	});
    
    function showNextButton() {
        $('#nextQuestion').removeClass('hiddenButton');
        $('#nextQuestion').addClass('visibleButton');
    }
    
    function showAnswer(index, answer) {
        $(answer).removeClass('hiddenAnswer');
        $(answer).addClass('revealedAnswer');
    }
	
	$('#identifyAnswer').keyup(function() {
		$('.hiddenAnswer').each(function(index, answer) {
			if (sanitise($('#identifyAnswer').val()) === sanitise($(this).text())) {
				showAnswer(index, answer);
				$('#identifyAnswer').val("");
				
                if (!$('.hiddenAnswer').size()) {
                    showNextButton();
                }
			}
		});
	});
    
    $('#revealAnswers').click(function() {
        $('.hiddenAnswer').each(showAnswer);
        showNextButton()
    })
    
    $('#nextQuestion').click(function() {
        location.reload();
    })
});

function sanitise(input) {
	return input.toLowerCase().replace(" ", "");
}