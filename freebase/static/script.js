// A Jquery plugin to do shuffling.
// Available under MIT license, so just nicked and placed here
// Original source: http://yelotofu.com/labs/jquery/snippets/shuffle/jquery.shuffle.js
(function($){

	$.fn.shuffle = function() {
		return this.each(function(){
			var items = $(this).children().clone(true);
			return (items.length) ? $(this).html($.shuffle(items)) : this;
		});
	}
	
	$.shuffle = function(arr) {
		for(var j, x, i = arr.length; i; j = parseInt(Math.random() * i), x = arr[--i], arr[i] = arr[j], arr[j] = x);
		return arr;
	}
	
})(jQuery);

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

function sanitise(input) {
	return input.toLowerCase().replace(/[^0-9A-Za-z]/g, "");
}