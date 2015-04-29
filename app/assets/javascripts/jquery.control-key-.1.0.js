/*
 * jQuery plugin to control key typed in input field like input:text and textarea 
 * Repo: https://bitbucket.org/channainfo/control-key-input
*/

(function($){
  jQuery.caretPosition = function(input){
     var inputType = input.tagName.toLowerCase();
   	 if( inputType == "input" || inputType == "textarea") {
   	   var iCaretPos = 0;

	   // IE Support
	   if (document.selection) {
	     input.focus ();
	     var oSel = document.selection.createRange ();
	     oSel.moveStart ('character', - input.value.length);
	     iCaretPos = oSel.text.length;
	    }

	    // Firefox support
	    else if (input.selectionStart || input.selectionStart == '0')
	      iCaretPos = input.selectionStart;

	    // Return results
	    return iCaretPos;	
	 }	
  };

  $.fn.controlKeyInput = function(options){
    
  	var defaults = {
  		allowChar: /./,
  		// allowKeyCode: [8,13,9,37,38,39,40]
      allowKeyCode: [8,13,9]
  	};

    var settings = $.extend(defaults, options);

    return this.each(function(){
		      $this = $(this);
		      $this.on('keypress', function(event){
			      var success = true ;	
			      var key = event.keyCode || event.which ;

			      settings.allowKeyCode.indexOf(key);

			      var char = $.trim(String.fromCharCode(key));

			      if(!char.match(settings.allowChar) ) {
			      	if(settings.allowKeyCode) {
			      	  if(settings.allowKeyCode.indexOf(key) == -1)
			      	     success = false ;
			      	}	 
			      }
			      if(success){
				    if($.isFunction(settings.allow)){
				      if(!settings.allow(this, char)){
				          success = false;
				      }
				    }
				  }
				  if(!success){
				  	event.preventDefault();
				    if($.isFunction(settings.failed))
				       settings.failed(this, char);  	
				  }
				  else{
				  	if($.isFunction(settings.success)){
				  		settings.success(this, char);
				  	}
				  }
		    });
    	   });
  }
}(jQuery));