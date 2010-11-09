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

function sanitise(input) {
	return input.toLowerCase().replace(/[^0-9A-Za-z]/g, "");
}