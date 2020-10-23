/*ratingtool.js
 * 
 */
(function() {

// Localize jQuery variable
var jQuery;

var CURRENT_SCRIPT = document.getElementById('ratingtooljs').src;
var CODE_BASE = CURRENT_SCRIPT.substring(0, CURRENT_SCRIPT.lastIndexOf("/js"));
var PAGE = window.location;  //The rated application's url 
var PAGE_TITLE = document.title; //The rated application's title
var rating = ''; //binary value will be set to 1 for thumbs up, 0 for thumbs down
var FeedbackDate = ''; //variable to hold date for database.
var textArea = '';
var delaydisplay = '150';
var delaythanks = '3000';

/********** Load jQuery if not present *********/

if (window.jQuery === undefined || window.jQuery.fn.jquery !== '1.4.2') {
    var script_tag = document.createElement('script');
    script_tag.setAttribute("type","text/javascript");
    script_tag.setAttribute("src",CODE_BASE +"/js/jquery-1.4.2.min.js");
    if (script_tag.readyState) {
      script_tag.onreadystatechange = function () { // For old versions of IE
          if (this.readyState == 'complete' || this.readyState == 'loaded') {
              scriptLoadHandler();
          }
      };
    } else {
      script_tag.onload = scriptLoadHandler;
    }
    // Try to find the ratingtooljquery div, otherwise default to the documentElement
    (document.getElementById("ratingtooljquery")|| document.documentElement).appendChild(script_tag);
} else {
    // The jQuery version on the window is the one we want to use
    jQuery = window.jQuery;
    main();
}

/******** Called once jQuery has loaded ******/
function scriptLoadHandler() {
    // Restore $ and window.jQuery to their previous values and store the
    // new jQuery in our local jQuery variable
    jQuery = window.jQuery.noConflict(true);
    // Call our main function
    main(); 
}

/******** Our main function ********/
function main() { 
	
    jQuery(document).ready(function($) { 
  
    	//Function to call php file to insert data into database
    	function pushData(arr) {    		
    	    $.ajax({
    	        url: CODE_BASE + "/storeFeedback.php", 
    	        type: "post",
    	        data: arr,
    	        cache: false,
    	        success: function(data){
    	        	//alert(data);
    	        },
    	        error: function(xhr){
    	            alert("An error has occurred.");
    	       }
    	     });
    	}

    	//use the browser's built-in functionality to quickly and safely escape the string
    	//Not used at this time
    	function escapeHTML(str) {
    	    var div = document.createElement('div');
    	    div.appendChild(document.createTextNode(str));
    	    return div.innerHTML;
    	}

    	//Handles the toggle and timeout of the thank you for your feedback popup.
    	//This is called after SUBMIT button and "No, thanks" are clicked.
    	//Timeout set to 2 seconds. The variable delaythanks is set in ajax call
    	//returning json data from constants.json
//    	function thankyou(){
//    		
//    		$('#thnx_div').slideToggle(function(){
//    			setTimeout(function() {
//    		        $('#thnx_div').slideToggle();
//    		    }, 3000);
//    		});	
//    	}
//    	function thankyou(){
//    		if($("#thnx_div").is(':visible')){
//    			setTimeout(function() {
//		        $('#thnx_div').slideToggle();
//		      }, 3000);
//    		}
//    		
//    	}
//    	function thankyou(){
//		$('#thnx_div').slideToggle(function(){
//		        $('#thnx_div').Toggle();	
//		});	
//    	}
		//reset form when page is refreshed
		$('#rtform').trigger('reset');
    		
    	//Create new div for thumbsup for X close
//    	var xdiv = document.createElement('div');
//    	xdiv.id = 'CloseThumbs';
//    	xdiv.innerHTML = 'X';
//    	xdiv.style["font-size"] = "11.5px";
//    	document.getElementById('thumbs_div').appendChild(xdiv);
    	
    	//Function to get constants stored in json file
    	//The display of the ratingtool depends on the ajax return
   	setTimeout(function() {
           $('#thumbs_div').slideToggle();
       //   $('#thumbs_div').focus();
    	}, delaydisplay);
/*
    	$.ajax({
    		type: "GET",
    		url: CODE_BASE + "/constants.json",
                dataType: 'json',
    		success: function(data)
    		{
                  
    			//var myObj = JSON.parse(data);
    			var myObj = data;
    			delaydisplay=myObj.delaydisplay;
    			delaythanks=myObj.delaythanks;
    			
    			//Don't display until ajax success. 
    			//Use delay found in constants.json
    	    	setTimeout(function() {
    	    		$('#thumbs_div').slideToggle();
       //   $('#thumbs_div').focus();
    		    }, delaydisplay);
    		},
    		error: function (data) {
               // alert("Error reading constants.json");
    		}
    	});
*/
    	//"Was this page helpful?" thumbs_div X close
    	//do not update database 
    	$("#CloseThumbs").click(function() {
    		$('#thumbs_div').slideToggle();
    	});
    		
    	//If thumbsup clicked, slide popupRating up
    	$('#thumbsup').click(function(){  
            thumbsUporYes();
/*
    		rating = 1;
    		$('#thumbs_div').hide();
    		$('#thnx_div').slideToggle(function(){
			setTimeout(function() {
		        $('#thnx_div').slideToggle();
		    }, 3000);
		    });	
*/
    	});

    	$("#CloseThumbs").keydown(function(event) {
           //console.log('closethumbs keydown');
           var keycode = (event.keyCode ? event.keyCode : event.which);
	   if(keycode == '13'){
	      //console.log('You pressed a "enter" key ');	
              $('#thumbs_div').slideToggle();
	   } 
        });



        $('#thumbsup').keydown(function(event) {
           //console.log('yes return');
           var keycode = (event.keyCode ? event.keyCode : event.which);
	   if(keycode == '13'){
	      //console.log('You pressed a "enter" key in somewhere');	
              thumbsUporYes();
	   } 
        });

        $('#thumbsdown').keydown(function(event) {
           //console.log('no return');
           var keycode = (event.keyCode ? event.keyCode : event.which);
	   if(keycode == '13'){
	      //console.log('You pressed a "enter" key in somewhere');	
              thumbsDownorNo();
	   } 
        });

    	//If "yes" text clicked, slide popupRating up
    	$('#yes').click(function(){  
            //console.log('yes clicked');
            thumbsUporYes();
/*
    		rating = 1;
    		$('#thumbs_div').hide();
    		$('#thnx_div').slideToggle(function(){
    			setTimeout(function() {
    		        $('#thnx_div').slideToggle();
    		    }, 3000);
    		    });	
*/
    	});

        /* Called if the Thumbs up icon or Yes text is click or reached by 
         * tabbing and then pressing the return key. 
         */
        function thumbsUporYes() {
    //console.log('thumbsUporYes entered');
    	   rating = 1;
    	   $('#thumbs_div').hide();
    	   $('#thnx_div').slideToggle(function(){
    	       setTimeout(function() {
    	           $('#thnx_div').slideToggle();
    	       }, 3000);
    	   });	
           $("#linkcomments").focus(); 
        } 

        function thumbsDownorNo() {
    //console.log('thumbsDownorNo entered');
       	   rating = 0;
    	   $('#thumbs_div').hide();
    	   $('#thnx_div').slideToggle(function(){
    	      setTimeout(function() {
    	         $('#thnx_div').slideToggle();
    	      }, 3000);
           });	
           $("#linkcomments").focus(); 
        } 


        function comments(event) {
//console.log('navigating to third window');
     	   $('#thnx_div').hide();
    	   $('#popupRating').slideToggle();
    	   document.getElementById("thnx_div").style.visibility = "hidden";
    	   $('.popupRatingHeading').text("Comments or suggestions?");
    	   rating = rating;    		
//$('#NoThanksDiv').get(0).focus();
//$('.NoThanks').get(0).focus();
$('.NoThanks').focus();
event.preventDefault();
//console.log('linkcomments clicked');
        } 

    	$('#linkcomments').click(function(event){  
             comments(event);
//console.log('linkcomments clicked');
//$("#RatingTool_textarea").trigger("click"); 
/*
    		$('#thnx_div').hide();
    		$('#popupRating').slideToggle();
    		document.getElementById("thnx_div").style.visibility = "hidden";
    		$('.popupRatingHeading').text("Comments or suggestions?");
    		rating = rating;    		
*/
    	});

        $('#linkcomments').keydown(function(event) {
           //console.log('linkcomments keydown');
           var keycode = (event.keyCode ? event.keyCode : event.which);
	   if(keycode == '13'){
	      //console.log('You pressed a "enter" key in somewhere');	
              comments(event);
	   } 
        });



    	//If thumbsdown clicked, slide popupRating up
    	$('#thumbsdown').click(function(){  
           thumbsDownorNo();
/*
    		rating = 0;
    		$('#thumbs_div').hide();
    		$('#thnx_div').slideToggle(function(){
    			setTimeout(function() {
    		        $('#thnx_div').slideToggle();
    		    }, 3000);
    		    });	
*/
    	});
    	
    	//If "no" text is clicked, slide popupRating up
      	$('#no').click(function(){  
             thumbsDownorNo();
/*
    		$('#thumbs_div').hide();
    		rating = 0;
    		$('#thnx_div').slideToggle(function(){
    			setTimeout(function() {
    		        $('#thnx_div').slideToggle();
    		    }, 3000);
    		    });	
*/

    	});
    

       function noThanks() {
    	  $('#popupRating').slideToggle();
    	  $('#RatingTool_textarea').val('');
    	  var args = {};
    	  args['page'] = PAGE_TITLE;
    	  args['url'] = PAGE;
    	  args['rating'] = rating;
    	  args['feedback'] = '';
    	  args['allowed'] = 1;
    	  var arg_string = jQuery.param(args,true);
    	  pushData(arg_string);
       }

	
        $('.NoThanks').keydown(function(event) {
           //console.log('linkcomments keydown');
           var keycode = (event.keyCode ? event.keyCode : event.which);
	   if(keycode == '13'){
	      //console.log('You pressed a "enter" key in somewhere');	
              noThanks();
	   } 
        });

    		
    	//If No, thanks is clicked, slide popupRating down
    	//and display thank you for 30 seconds
    	$('.NoThanks').click(function(){
/*
    		$('#popupRating').slideToggle();
    		$('#RatingTool_textarea').val('');
    		var args = {};
    		args['page'] = PAGE_TITLE;
    		args['url'] = PAGE;
    		args['rating'] = rating;
    		args['feedback'] = '';
    		var arg_string = jQuery.param(args,true);
    		pushData(arg_string);
*/
              noThanks();
    	});
    		
    	//When characters are entered into textarea, count characters and output count
    	//in span.
    	$('#RatingTool_textarea').keyup(function() {
    		var $this = $(this);
    		var textlen = $this.attr("maxlength") - $this.val().length;
    		$('#rchars').text(textlen);
    	});
    		
    	//Submit button
    	$('#RatingToolSubmit').click(function(){
            //console.log("RatingToolSubmit clicked");
    	});
    		
    	//Feedback Form
    	//Display thank you for 30 seconds
    	$('#rtform').submit(function(e){
    		$('#thnx_div').hide();
    		e.preventDefault(); /*prevent refresh of page*/
    		$('#popupRating').slideToggle();
    		$('#thumbs_div').hide();
    		//textArea = escapeHTML($("#RatingTool_textarea").val());
    		textArea = $('#RatingTool_textarea').val();
    		var args = {};
    		args['page'] = PAGE_TITLE;
    		args['url'] = PAGE;
    		args['rating'] = rating;
    		args['feedback'] = textArea;
    		args['allowed'] = 1;
    		var arg_string = jQuery.param(args,true);
    		pushData(arg_string);
    		$('#RatingTool_textarea').val('');
 //   		$('#thnx_div').slideToggle();
    	});
    	

        function closeX() {
       	   $('#popupRating').slideToggle();
    	   $('#RatingTool_textarea').val('');
    	   var args = {};
    	   args['page'] = PAGE_TITLE;
    	   args['url'] = PAGE;
    	   args['rating'] = rating;
    	   args['feedback'] = '';
    	   args['allowed'] = 1;
    	   var arg_string = jQuery.param(args,true);
    	   pushData(arg_string);
        }

	
        $('#closeX').keydown(function(event) {
           //console.log('closeX keydown');
           var keycode = (event.keyCode ? event.keyCode : event.which);
	   if(keycode == '13'){
              closeX();
	   } 
        });

    	//Feedback form "X" close
    	$("#closeX").click(function() {
            closeX();
/*
    		$('#popupRating').slideToggle();
    		$('#RatingTool_textarea').val('');
    		var args = {};
    		args['page'] = PAGE_TITLE;
    		args['url'] = PAGE;
    		args['rating'] = rating;
    		args['feedback'] = '';
    		var arg_string = jQuery.param(args,true);
    		pushData(arg_string);
*/
    	});
    

	
    	$("#closeXX").click(function() {
                //console.log('closeXX triggered');
              closeXX();
/*
    		$('#thnx_div').slideToggle(function(){
    			setTimeout(function() {
    		        $('#thnx_div').slideToggle();
    		    }, 000);
        		document.getElementById("thnx_div").style.visibility = "hidden";
    		    });	
*/
    	});

        $('#closeXX').keydown(function(event) {
           //console.log('closeXX keydown');
           var keycode = (event.keyCode ? event.keyCode : event.which);
	   if(keycode == '13'){
              closeXX();
	   } 
        });
       /* This will close the second level window. 
        */
        function closeXX () {
           $('#thnx_div').slideToggle(function(){
    	       setTimeout(function() {
    	          $('#thnx_div').slideToggle();
    	       }, 000);
               document.getElementById("thnx_div").style.visibility = "hidden";
    	   });	
        }




    }); //end of document ready function
} //end of main function
})(); // Call the anonymous function immediately
