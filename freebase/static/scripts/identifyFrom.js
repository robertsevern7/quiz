$(document).ready(function() {
	$('#identifyAnswer').val("");
    $('#revealAnswers,#nextQuestion').button();
    
    function showNextButton() {
        $('#nextQuestion').removeClass('hiddenButton');
        $('#nextQuestion').addClass('visibleButton');
    }
    
    function showAnswer(index, answer) {
        $(answer).fadeIn('slow', function() {
            $(answer).removeClass('hiddenAnswer');
            $(answer).addClass('revealedAnswer');
            
            if (!$('.hiddenAnswer').size()) {
                showNextButton();
            }
        });
    }
	
	$('#identifyAnswer').keyup(function() {
		$('.hiddenAnswer').each(function(index, answer) {
			if (sanitise($('#identifyAnswer').val()) === sanitise($(this).text())) {
				showAnswer(index, answer);
				$('#identifyAnswer').val("");
			}
		});
	});
    
    $('#revealAnswers').click(function() {
        $('.hiddenAnswer').each(showAnswer);
        showNextButton()
    })
});