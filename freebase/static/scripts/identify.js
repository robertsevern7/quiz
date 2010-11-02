$(document).ready(function() {
    $('.answerLeft').hover(
        function(mouseInEventObject) {
            $(this).addClass("answerHover");
        },
        function(mouseOutEventObject) {
            $(this).removeClass("answerHover");
        }
    );
    
    $('.answerLeft').click(function() {
        $('.answerLeft').each(function() {
            $(this).removeClass("leftAnswerSelect");
        });
        
        $(this).addClass("leftAnswerSelect");
        check()
    });
    
    $('.answerRight').hover(
        function(mouseInEventObject) {
            $(this).addClass("answerHover");
        },
        function(mouseOutEventObject) {
            $(this).removeClass("answerHover");
        }
    );
    
    $('.answerRight').click(function() {
        $('.answerRight').each(function() {
            $(this).removeClass("rightAnswerSelect");
        });
        
        $(this).addClass("rightAnswerSelect");
        check()
    });
    
    function check() {
        var left = $('.leftAnswerSelect').attr('hiddenAnswer');
        var right = $('.rightAnswerSelect').text();
        
        if (left && right && left === right) {
            alert('Genius');
        }
    }
});