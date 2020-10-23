/*
 * Use this as a location to add polyfill JS for features not supported by browsers (like IE) 
 */

/*
 * IE doesn't have a startsWith function for strings.
 */
if (!String.prototype.startsWith) {
    String.prototype.startsWith = function(searchString, position){
      return this.substr(position || 0, searchString.length) === searchString;
  };
}
$(document).ready(function(){
	$(".data-uscb-banner-text").each(function(element) {
        var height = $(this).height();
        var padding = 20;
        if (height < 200) {
            padding = (200 - height) / 2;
        } 
        $(this).css("padding-top", padding + "px");
        $(this).css("padding-bottom", padding + "px");
    });
    $(".data-uscb-banner-alert-container").each(function(element) {
        var container = $(this);
        var text = container.find(".data-uscb-banner-alert-content");
        if ( text.length !== 0 ) {
            var heightDiff = container.height() - text.height();
            text.css("padding-top", heightDiff / 2 + "px");
            text.css("padding-bottom", heightDiff / 2 + "px");
        }
    });
});
( function( $ ){

	var ONE_DAY = 1000 * 60 * 60 * 24;
	var BREAKPOINT = 768;
	
	var CACHED_EVENT_DATA = 'eventcalendarCachedEvents';
	var LINK_EVENT_ID_DATA = 'eventcalendarLinkId';
	var DATE_DATA = 'eventcalendarJSDate';
	var DISPLAY_DATE_DATA = 'eventcalendar-dates';

	var PLACEHOLDER_DOW = '%DOW%',
		PLACEHOLDER_MONTH = '%MONTH%',
		PLACEHOLDER_DATE = '%DATE%',
		PLACEHOLDER_YEAR = '%YEAR%';

	var isEditMode = (typeof $.cookie != 'undefined' && $.cookie('wcmmode') == "edit");

	var openedEventWindow;
	var $eventEles, $dayViews = $();

	var today = new Date();	
	today.setHours( 0, 0, 0, 0 );

	$( document ).ready( function() {
		$eventEles = $( '[data-eventcalendar]' );

		if( !$eventEles.length ) { return; } // only continue if there are events

		extractEvents();
		buildEvents();

		if( $dayViews.length ) {
			$( window ).on( 'resize.eventcalendar', function(){
				buildEvents();
			} );
		}
	} );

	/* Load the page in normal or mobile mode */
	function buildEvents() {
		var pageWindow = $( this );
		var childCount = 0;

		$eventEles.each( function () {
			if( $( this ).data( 'eventcalendar' ) == 'events' ) {
				//Check for children
				childCount = $( this ).find( '[data-eventcalendar-events] li' ).length;

				if( childCount === 0 ){
					loadUpcoming( $( this ) );
				}
			} else {
				$dayViews = $dayViews.add( this );

				if( pageWindow.width() >= BREAKPOINT ) {
					//Check for children
					childCount = $( this ).find( '[data-eventcalendar-lg] [data-eventcalendar-days] li' ).length;

					if( childCount == 0 ){
						loadDayLgView( $( this ) ); 
					}
				} else {
					//Check for children
					childCount = $( this ).find( '[data-eventcalendar-sm] option' ).length;

					if( childCount == 0 ){
						loadDaySmView( $( this ) );
					}
				}
			}
		} );
	}

	function addTo( date, days ) {
		return days == 0 ? date : new Date( date.getTime() + ( days * ONE_DAY ) );
	}

	function returnURL( linkId ) {
		//"http://www.calendarwiz.com/calendars/popup.php?op=view&id=122467054&crd=cens1sample";
		var eventId = '';
		var i = 0;
		var i2 = 0;
		var theURL;

		var theBegin = 'http://www.calendarwiz.com/calendars/popup.php?op=view&id=';
		var theEnd   = '&crd=cens1sample';

		if( linkId ){
			i = linkId.indexOf( '(' ) + 1;
			i2 = linkId.indexOf( ')' );

			if ( i > -1 ){ eventId = linkId.substring( i, i2 ); }
		}
		
		theURL = theBegin + eventId.toString() + theEnd;

		return theURL;
	}

	function extractEvents() {
		var CALENDARWIZ_MONTHS = ["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"];
		// Single date regular expression: Saturday, (Dec)ember (10), (2011)
		var SINGLE_REGEXP = /[A-Z]{3,9}, ([A-Z]{3})[A-Z]{0,6} ([0-9]{1,2}), ([0-9]{4})(.*)/i;
		// Range of dates regular expression: Saturday, (Dec)ember (10)[, (2011)] to Sunday, (Dec)ember (11), (2011)
		var RANGE_REGEXP = /[A-Z]{3,9}, ([A-Z]{3})[A-Z]{0,6} ([0-9]{1,2})(?:, ([0-9]{4}))? to [A-Z]{3,9}, ([A-Z]{3})[A-Z]{0,6} ([0-9]{1,2}), ([0-9]{4})(.*)/i;
		// HH:MM regular expression: 10:00 or 8:30
		var TIME_REGEX = /(^|\s)(1[0-2]|0?[1-9]):([0-5]?[0-9])/;

		$eventEles.each( function() {
			var events = [];

			$( this ).find( '[data-eventcalendar-calendarwiz]' ).find( '.cwuceeventtitle' ).each( function() {
				var titleCell = $( this ),
					title = $.trim( titleCell.text() ),
					onClick = titleCell.children( 'a' ).attr( 'onclick' ),
					dateCell = titleCell.parent( 'tr' ).children( 'td' ),
					dateText = $.trim( dateCell.text() ),
					range = dateText.match( RANGE_REGEXP ),
					single, start, end, details;

				if( range ) {
					start = {
						month: $.inArray( range[1], CALENDARWIZ_MONTHS ),
						day: range[2],
						year: range[3] || range[6]
					};
					end = {
						month: $.inArray( range[4], CALENDARWIZ_MONTHS ),
						day: range[5],
						year: range[6]
					};
					details = $.trim( range[7] );
				} else {
					single = dateText.match( SINGLE_REGEXP );
					if( single ) {
						start = end = {
							month: $.inArray( single[1], CALENDARWIZ_MONTHS ),
							day: single[2],
							year: single[3]
						};
						details = $.trim( single[4] );
					}
				}

				if ( start && end ) {
					if( details.search( TIME_REGEX ) == -1 ) {
						details = '';
					} else {
						title = title.replace( details, '' );
					}

					var startDate = new Date( start.year, start.month, start.day ),
						endDate = new Date( end.year, end.month, end.day );

					events.push( {
						title: title,
						start: startDate,
						end: endDate,
						details: details,
						onClick: onClick
					} );
				}
			} );

			$( this ).data( CACHED_EVENT_DATA, events );
		} );
	}

	function displayPopup( event ){
		event.preventDefault();
		if( openedEventWindow ) openedEventWindow.close();

		var eventUrl = $( this ).data( LINK_EVENT_ID_DATA );

		openedEventWindow = window.open( eventUrl, 'popup_event', 'toolbar=no,location=no,status=no,menubar=no,scrollbars=yes,resizable=yes,width=475,height=475' );
		return false;
	}

	function getAuthorableDataAttributes( $eventEle ) {
		var errors = [],
			attr = $eventEle.data( DISPLAY_DATE_DATA ),

			// Default values if it's malformed
			validDow = ['Sun', 'Mon', 'Tues', 'Wed', 'Thur', 'Fri', 'Sat'],
			validMonths = ['January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December'],
			validFormat = '%DOW%, %MONTH% %DATE%, %YEAR%';

		if( typeof attr == 'object' ) {

			// Validate the day of week array
			if( typeof attr.dow == 'object' ) {
				if( attr.dow.length > 7 ) {
					errors.push( '<strong>Error!</strong> You have provided too many names for the days of the week. There must be exactly 7, starting with Sunday. Default values will be used until this is corrected.');
				} else if( attr.dow.length < 7 ) {
					errors.push( '<strong>Error!</strong> You have provided too few names for the days of the week. There must be exactly 7, starting with Sunday. Default values will be used until this is corrected.');
				} else {
					validDow = attr.dow;
				}
			} else {
				errors.push( '<strong>Error!</strong> The array for the names of the days of the week is malformed. Please check that all quotes are paired correctly, there are commas between words, and that the days of the week array begins with <code>[</code> and ends with <code>]</code>. Default values will be used until this is corrected.' );
			}

			// Validate the months array
			if( typeof attr.months == 'object' ) {
				if( attr.months.length > 12 ) {
					errors.push( '<strong>Error!</strong> You have provided too many names for the months. There must be exactly 12, starting with January. Default values will be used until this is corrected.');
				} else if( attr.months.length < 12 ) {
					errors.push( '<strong>Error!</strong> You have provided too few names for the months. There must be exactly 12, starting with January. Default values will be used until this is corrected.');
				} else {
					validMonths = attr.months;
				}
			} else {
				errors.push( '<strong>Error!</strong> The array for the names of the months is malformed. Please check that all quotes are paired correctly, there are commas between words, and that the month array begins with <code>[</code> and ends with <code>]</code>. Default values will be used until this is corrected.' );
			}

			validFormat = attr.format;
		} else {
			errors.push( '<strong>Error!</strong> One of the Advanced Language Attributes is malformed. Please check that all quotes are paired correctly, there are commas between words, and that the day and month arrays begin with <code>[</code> and end with <code>]</code>. Default values will be used until this is corrected.' );
		}

		if( errors.length && isEditMode) {
			// Show error messages 
			var $errors = $();
			for( var i = 0; i < errors.length; i++ ) {
				$errors = $errors.add( $( '<p/>' ).html( errors[i] ) );
			}

			$eventEle.prepend( $errors.wrapAll( $( '<div />' ) ).parent().addClass( 'alert alert-danger eventcalendar-errors' ) );

			console.error( '[eventcalendar] advanced language attribute(s) are malformed', attr ); // eslint-disable-line no-console
		} else {
			$eventEle.find( '.eventcalendar-errors' ).remove();
		}

		return { 
			dow: validDow,
			months: validMonths,
			format: validFormat
		};
	}

	/** Full Page View  **/
	function loadDayLgView( $eventEle ){	
		var events = $eventEle.data( CACHED_EVENT_DATA ),
			showWeekends = $eventEle.data( 'eventcalendar-showweekends' ) || false,

			$lgView = $eventEle.find( '[data-eventcalendar-lg]' ),
			$dayList = $lgView.find( '[data-eventcalendar-days]' ),
			$eventList = $lgView.find( '[data-eventcalendar-events]' ),

			noEventsMsg = $eventEle.find( '[data-eventcalendar-error]' ).html(),
			dateFormatting = getAuthorableDataAttributes( $eventEle );

		for ( var i = 0, j = 0; i < 7; i++ ) {
			var date = addTo( today, i ),
				day = date.getDate(),
				dow = dateFormatting.dow[date.getDay()],
				month = dateFormatting.months[date.getMonth()],
				year = date.getFullYear(),
				displayDateStr = dateFormatting.format.replace( PLACEHOLDER_DOW, dow ).replace( PLACEHOLDER_MONTH, month ).replace( PLACEHOLDER_DATE, day ).replace( PLACEHOLDER_YEAR, year );

			//Skip Sat and Sun
			if ( ( dow == dateFormatting.dow[0] || dow == dateFormatting.dow[6] ) && !showWeekends ) continue;

			$dayList.append(
				$( '<li/>' ).addClass( 'day_new uscb-filter-parent uscb-layout-column' ).toggleClass( 'selected', j++ == 0 ).attr( 'style', 'width:-moz-available; text-align:left; border-style:none;' ).append(
					$( '<a/>' ).attr( 'href', '#' ).data( DATE_DATA, date ).addClass( 'uscb_navigation_links uscb-w-100' ).attr( 'style', 'width:100%;' ).append(
						$( '<span/>' ).text( displayDateStr )
					).on( 'click.eventcalendar', function( event ) {
						var date = $( this ).data( DATE_DATA ),
							count = 0;
							
						$eventList.empty();
						$dayList.find( '.selected' ).removeClass( 'selected' ).attr( 'style', 'background-color: inherit;' );
						$( this ).parent( '.day_new' ).addClass( 'selected' ).attr( 'style', 'background-color: #f7f9f9;' );

						$.each( events, function( k, event ) {
							//Added code to account for DST (Math.ceil(x/ONE_DAY)).
							var sTmp = event.onClick.toString();
							var time = Math.ceil( date.getTime() / ONE_DAY );

							if( time >= Math.ceil( event.start.getTime() / ONE_DAY ) && time <= Math.ceil( event.end.getTime() / ONE_DAY ) ) {
								count++;

								$eventList.append(
									$( '<a/>' ).addClass( 'uscb-list-item' ).attr( 'href', '#' ).attr( 'title', event.title.replace( /\uFFFD/g, '' ) ).attr( 'style', 'cursor:pointer;' )
								).append(
									$( '<div/>' ).addClass( 'uscb-list-item-container' ).append(
										$( '<p/>' ).addClass( 'uscb-title-3 uscb-color-primary uscb-margin-TB-02' ).text( event.title.replace( /\uFFFD/g, '' ) ),
										$( '<p/>' ).addClass( 'uscb-sub-heading-2 uscb-color-secondary-1 uscb-line-height-20-18 uscb-margin-TB-02' ).text( event.details ),
										$( '<hr/>' ).addClass( 'uscb-light-teal-hr' )
									).data( LINK_EVENT_ID_DATA, returnURL( sTmp ) ).click( displayPopup )
								);
							} //end of if
						} ); //end of "each"

						if( count == 0 ) {
							$eventList.append(
								$( '<p/>' ).html( noEventsMsg ),
								$( '<hr/>' ).addClass( 'uscb-light-teal-hr' )
							);
						}

						$( this )[0].blur();
						event.preventDefault();

						return false;
					} ) //end of click event & function
				) //'<li> uscb-filter-parent
			); //norm_dd
		} //end of the "for loop

		$dayList.find( '.selected a' ).trigger( 'click' );
	} //End of function

	/*** MOBILE / TABLET VIEW  ***/
	function loadDaySmView( $eventEle ){
		var events = $eventEle.data( CACHED_EVENT_DATA ),
			showWeekends = $eventEle.data( 'eventcalendar-showweekends' ) || false,
			dayListOptionsHTML = '',

			$smView = $eventEle.find( '[data-eventcalendar-sm]' ),
			$dayList = $smView.find( 'select' ),
			$eventList = $smView.find( '[data-eventcalendar-events]' ),

			noEventsMsg = $eventEle.find( '[data-eventcalendar-error]' ).html(),
			dateFormatting = $eventEle.data( DISPLAY_DATE_DATA );

		for( var i = 0; i < 7; i++ ) {
			var date = addTo( today, i ),
				day = date.getDate(),
				dow = dateFormatting.dow[date.getDay()],
				month = dateFormatting.months[date.getMonth()],
				year = date.getFullYear(),
				displayDateStr = dateFormatting.format.replace( PLACEHOLDER_DOW, dow ).replace( PLACEHOLDER_MONTH, month ).replace( PLACEHOLDER_DATE, day ).replace( PLACEHOLDER_YEAR, year ),
				dateVal = date.getTime();

			//Skip Saturday and Sunday
			if ( ( dow == dateFormatting.dow[0] || dow == dateFormatting.dow[6] ) && !showWeekends ) continue;

			dayListOptionsHTML += '<option value="' + dateVal.toString() +  '" >' + displayDateStr.toString() + '</option>';
		} //end of 'for'

		$dayList.append( dayListOptionsHTML ).on( 'change.eventcalendar', changeView );

		function changeView( e ) {
			var selValue = $( this ).val();
			var count = 0; 

			$eventList.empty();

			$.each( events, function( k, event ) {
				var sTmp = event.onClick.toString();
				var time = Math.ceil( selValue / ONE_DAY );

				if( time >= Math.ceil( event.start.getTime() / ONE_DAY ) && time <= Math.ceil( event.end.getTime() / ONE_DAY ) ) {
					count++;
					$eventList.append(
						$( '<a/>' ).addClass( 'uscb-list-item' ).attr( 'href', '#' ).attr( 'title', event.title.replace( /\uFFFD/g, '' ) ).attr( 'style', 'cursor:pointer;' )
					).append(
						$( '<div/>' ).addClass( 'uscb-list-item-container' ).append(
							$( '<p/>' ).addClass( 'uscb-title-3 uscb-color-primary uscb-margin-TB-02' ).text( event.title.replace( /\uFFFD/g, '' ) ),
							$( '<p/>' ).addClass( 'uscb-sub-heading-2 uscb-color-secondary-1 uscb-line-height-20-18 uscb-margin-TB-02' ).text( event.details ),
							$( '<hr/>' ).addClass( 'uscb-light-teal-hr' )
						).data( LINK_EVENT_ID_DATA, returnURL( sTmp ) ).click( displayPopup )
					);
				}//end of 'if'
			} );//end of 'each'
			
			if( count == 0 ) {
				$eventList.append(
					$( '<div/>' ).addClass( 'uscb-list-item-container' ).append(
						$( '<p/>' ).addClass( 'uscb-title-3 uscb-color-primary uscb-margin-TB-02' ).html( noEventsMsg ),
						$( '<hr/>' ).addClass( 'uscb-light-teal-hr' )
					)
				);
			}

			$( this )[0].blur();
			if( e ) { e.preventDefault(); }
			return false;
		}

		changeView.apply($dayList);
	} //end of loadDaySmView

	/*** Upcoming Events View ***/
	function loadUpcoming( $eventEle ) {
		var events = $eventEle.data( CACHED_EVENT_DATA ),
			showWeekends = $eventEle.data( 'eventcalendar-showweekends' ) || false,
			$eventList = $eventEle.find( '[data-eventcalendar-events]' ),

			noEventsMsg = $eventEle.find( '[data-eventcalendar-error]' ).html(),
			dateFormatting = $eventEle.data( 'eventcalendar-dates' ),

			count = 0;
		
		$.each( events, function( k, event ) {

			if( !showWeekends ) {
				var startDow = event.start.getDay(),
					endDow = event.end.getDay();

				// If the start and end date are on a weekend, don't show them (count does not get incremented)
				if ( ( startDow === 0 || startDow == 6 ) && ( endDow === 0 || endDow == 6 ) ) {
					return false;
				}
			}

			var eventUrl = returnURL( event.onClick.toString() );

			var startDate = event.start,
				displayStartDay = startDate.getDate(),
				displayStartDow = dateFormatting.dow[startDate.getDay()],
				displayStartMonth = dateFormatting.months[startDate.getMonth()],
				displayStartYear = startDate.getFullYear();

			var displayDateStr = dateFormatting.format.replace( PLACEHOLDER_DOW, displayStartDow ).replace( PLACEHOLDER_MONTH, displayStartMonth ).replace( PLACEHOLDER_DATE, displayStartDay ).replace( PLACEHOLDER_YEAR, displayStartYear );
			
			var endDate = event.end;

			startDate.setHours( 0, 0, 0, 0 ); // set to 0 for comparison
			endDate.setHours( 0, 0, 0, 0 ); // set to 0 for comparison
			if( startDate.getTime() != endDate.getTime() ) {

				var displayEndDay = endDate.getDate(),
					displayEndDow = dateFormatting.dow[endDate.getDay()],
					displayEndMonth = dateFormatting.months[endDate.getMonth()],
					displayEndYear = endDate.getFullYear();

				displayDateStr += ' - <br />' + dateFormatting.format.replace( '%DOW%', displayEndDow ).replace( '%MONTH%', displayEndMonth ).replace( '%DATE%', displayEndDay ).replace( '%YEAR%', displayEndYear );
			}

			$eventList.append(
				$( '<li />' ).addClass( 'uscb-w-100' ).append(
					$( '<a/>' ).addClass( 'uscb-layout-row uscb-wrap uscb-layout-align-vert-center uscb-color-primary' ).attr( 'href', eventUrl ).attr( 'title', event.title.replace( /\uFFFD/g, '' ) ).attr( 'style', 'cursor:pointer;' ).append(
						$( '<div/>' ).addClass( 'uscb-layout-column uscb-flex-row-md-100 uscb-flex-row-lg-40 uscb-flex-row-gt-lg-30' ).append(
							$( '<div/>' ).html( displayDateStr )
						),
						$( '<div/>' ).addClass( 'uscb-layout-column uscb-flex-row-md-100 uscb-flex-row-lg-60 uscb-flex-row-gt-lg-70' ).append(
							$( '<p/>' ).addClass( 'uscb-title-3 uscb-color-primary uscb-margin-TB-02' ).text( event.title.replace( /\uFFFD/g, '' ) ),
							$( '<p/>' ).addClass( 'uscb-sub-heading-2 uscb-color-secondary-1 uscb-line-height-20-18 uscb-margin-TB-02' ).text( event.details )
						)
					).data( LINK_EVENT_ID_DATA, eventUrl ).click( displayPopup ),
					$( '<hr/>' ).addClass( 'uscb-light-teal-hr' )
				)
			);

			count++;
		} ); //end of "each"

		if( count == 0 ) {
			$eventList.append(
				$( '<li />' ).addClass( 'uscb-w-100' ).append(
					$( '<hr/>' ).addClass( 'uscb-light-teal-hr' ),
					$( '<p/>' ).html( noEventsMsg ),
					$( '<hr/>' ).addClass( 'uscb-light-teal-hr' )
				)
			);
		}
	}
} )( jQuery );
//tabbed list
// JavaScript Document

// Wait until the DOM has loaded before querying the document
            $(document).ready(function(){
                
                $('li.button-tabs').each(function() //the button tabs li is the div that contains the dropdown and all the list divs
                {
                
                    
                    var $dropdown = $(this).parent().find("li.button_tabs_dropdown"); //get the div that contains the dropdown button and extra years
                    var $expand = $(this).find("a.toggle_dropdown");
                    var $allTabs = $(this).find('a.tabs'); //allTabs is an anchor with the tabs class
                    var k = 0;
                    var $tabsArray = [];
                    $tabsArray.push('#taball');
                    var $nextTab = $('a[href="'+"#tab"+k+'"]');
                    while ($nextTab.length > 0) //while there is a tab with an href like #tab[n]
                    {
                        
                        $nextTab = $('a[href="'+"#tab"+k+'"]'); //get the tab with href like #tab[n]
                        $tabsArray.push($nextTab.attr('href'));
                        k++;
                    }
                    
                    //on click display the drop down image
                    $($expand).on('click', function(e) //this is the list items div and also includes the dropdown menu
                    {
                        
                        if ($dropdown.attr('style') != undefined && $dropdown.attr('style').indexOf('none') >0)
                            {$dropdown.removeAttr('style');} //either show or hide the dropdown
                        else
                            {$dropdown.attr('style', 'display:none');}
                            
                    });
                    $($allTabs).on('click', function(e) //when one of the years in the dropdown is clicked
                    {
                        var $listTabs = document.getElementById('listMenu');
                        var $tabNum = ($allTabs.attr('href')); //tabNum is a string of format #tab[1-n] represnting which tab was clicked
                        var index = $tabNum.replace('#tab', '');
                        if (index == 'all')
                            index=0;
                        
                        var i=2;
                        var j =2;
                        
                        if (k-index < 2)
                        {
                            
                            j = parseInt(index,10)+4-k;
                            i=k-index;                            
                        }
                        else if (index < 2)
                        {
                            i=4-index;
                            j=index;                            
                        }
                        
                        var updateNum = 0;
                        for (var n=0; n<$tabsArray.length; n++)
                        {
                           
                            var $currentTab = $('a[href="'+$tabsArray[n]+'"]');
                            if (n>= (index-j) && n<=(index+i))
                            {
                                //set as active
                                $currentTab.parent().parent().parent().show();
                                if (n+1 == index)
                                {
                                   
                                    $currentTab.parent().parent().parent().addClass('current');
                                    $currentTab.addClass('active');
                                    //$currentTab.trigger('click');
                                }
                            } 
                            
                            else
                            {
                                //set as inactive
                                hideTab($currentTab);
                                //need some functinality to place this link in the dropdown
                                $dropdownAnchors = $(document).find('a.tabs').eq(updateNum++);
                                $dropdownAnchors.contents().filter(function() {return this.nodeType==3;})
                                    .replaceWith($currentTab.text());
                                $dropdownAnchors.attr('href', $tabsArray[n]);
                            }
                        }
                        var $active = $(document).find('ul.current'); //active is the currently selected tab ul
                        
                    });
                    
                 
                    
                });
                var hideTab = (function(currentTab)
                {
                    currentTab.parent().parent().parent().hide();
                    currentTab.parent().parent().parent().removeAttr('class');
                    currentTab.removeAttr('class');
                    
                    
                });
                $('ul.listTabs').on('click', 'li', function(e){
                        if ($(this).find('a').attr('class') == 'toggle_dropdown')
                            {
                            e.preventDefault();
                            return;}
                        var $content = $(this).find('a').attr('href');
                        window.open($content, "_self");
                        
                });
                $('ul.listTabs').on('hover', 'li', function(e)
                {
                    if ($(this).parent().attr('class') == 'listTabs')
                        return;
                    var $anchor = $(this).find('a');
                    var $color = $anchor.css("color");
                    
                    if ($anchor.css('color') == 'rgb(14, 114, 162)')
                        $anchor.css('color','#084e84' ); //"#084e84"
                    else
                        $anchor.removeAttr('style');
                    
                });
                $('li.button-tabs').on('hover', 'a',function(e)
                {
                    var $img = $(this).find('img');
                    var $src = $(this).find('img').attr('src');
                    if ($img.attr('class') == "selected")
                        return;
                    if ($src.indexOf("active") > 0 )                    
                        $img.attr('src', $src.replace("_active", ""));
                    
                    else
                        $img.attr('src', $src.replace(".jpg", "_active.jpg"));
                });
                $('li.button-tabs').on('click', 'a', function(e)
                {
                    var $img = $(this).find('img');
                    var $src = $(this).find('img').attr('src');
                    var $button_cls = $(this).find('img').attr('class');
                    
                    if ($button_cls===undefined && $src!=undefined)
                    {
                        $img.attr('class', 'selected');
                        $img.attr('src', $src.replace("n.jpg", "n_active.jpg"));
                    }
                    
                    else
                    {
                        $img.removeAttr('class');
                        //$img.attr('src', $src.replace("_active", ""));
                    }
                    
                });
                $('ul.tabs').each(function(){
                    
                    // For each set of tabs, we want to keep track of
                    // which tab is active and its associated content
                    var $active, $content, $links = $(this).find('a');

                    // If the location.hash matches one of the links, use that as the active tab.
                    // If no match is found, use the first link as the initial active tab.
                    $active = $($links.filter('[href="'+location.hash+'"]')[0] || $links[0]);
                    $active.addClass('active'); 
                    
                    //In case the href is an actual ref to another page, then abort as there's no need                    
                    //to hide/show the other tabs as the page will be redrawn.                    
                    if ( $active.attr('href') === undefined || $active.attr('href').indexOf('#') != 0)
                        return;
                    $content = $($active.attr('href'));
                    // Hide the remaining content
                    $links.not($active).each(function () {
                    	if (Modernizr.mq('only screen and (min-width : 960px)')) {
                    		$($(this).attr('href')).hide();
                    	}
                    });

                    // Bind the click event handler
                    $(this).on('click', 'li', function(e){
                        // Make the old tab inactive.
                        $active.removeAttr('class');
                        $($active.parent().siblings()[0]).removeClass('current-arrow');
                        $($active.parent().siblings()[0]).addClass('arrow-row');
                        $(".current").removeClass('current');
                        $content.hide(); 

                        // Update the variables with the new link and content
                        $active = $(this).find('a');
                        $content = $($(this).find('a').attr('href'));

                        // Make the tab active.
                        $active.attr('class','active');
                        
                        $($active.parent().siblings()[0]).removeClass('arrow-row');
                        $($active.parent().siblings()[0]).addClass('current-arrow');
                        $($($active).parent().parent()).addClass('current');
                        $content.show();

                        // Prevent the anchor's default click action
                        e.preventDefault();
                    });
                });
            });
var accordionChevronUp = "/etc.clientlibs/census/clientlibs/census-pattern-library/resources/images/upward-chevron.svg";
var accordionChevronDown = "/etc.clientlibs/census/clientlibs/census-pattern-library/resources/images/downward-chevron.svg";
var mobileWidth = 576;

function initTabAccordions(numTabs) {
	resizeTabAccordions(numTabs);
	
	for (x=1; x<=numTabs; x++) {
		var tabId = '#tab' + x;
		var accordionId = tabId + "_accd";
		
		if (Modernizr.mq('only screen and (max-width : ' + mobileWidth + 'px)')) {
			$(tabId).show(); 	
		}
		$(accordionId).on('hide.bs.collapse', function () {
			var arrowId = '#' + $(this).attr('id') + "_a";
    		$(arrowId).removeClass("glyphicon-chevron-down").addClass("glyphicon-chevron-right");
   		 });
		$(accordionId).on('show.bs.collapse', function () {
			var arrowId = '#' + $(this).attr('id') + "_a";
       		$(arrowId).removeClass("glyphicon-chevron-right").addClass("glyphicon-chevron-down");
    	});
	}
}

function initTabAccordionChevrons(selectedTabSpan) {
	$(selectedTabSpan).removeClass("glyphicon-chevron-right").addClass("glyphicon-chevron-down");
}

	
function resizeTabAccordions(numTabs) {
	for (x=1; x<=numTabs; x++) {
		var state = 'show';
		var tabId = '#tab' + x;
		var accordionId = tabId + "_accd";
		if (Modernizr.mq('only screen and (max-width : ' + mobileWidth + 'px)')) {
			state = 'hide';
			$(accordionId).addClass('collapse'); 
			$(tabId).show();
		} else {
			if ($("a[href='" + tabId + "']").hasClass('active')) 
				$(tabId).show();
			else
				$(tabId).hide();
		}	
		$(accordionId).collapse(state); 
	}
}
	

function initAccordionGroup(group, collapsedWidth) {
	if (collapsedWidth===undefined)
		collapsedWidth = mobileWidth + 1;
	else 
		collapsedWidth = collapsedWidth + 1;
	resizeAccordionGroup(group, collapsedWidth);
	$(group).find('.panel-collapse').each(function() {
		$(this).on('hide.bs.collapse', function () {
	        $(this).parent().find(".glyphicon-chevron-down").removeClass("glyphicon-chevron-down").addClass("glyphicon-chevron-right");
	    });
		
		$(this).on('show.bs.collapse', function () {
	       $(this).parent().find(".glyphicon-chevron-right").removeClass("glyphicon-chevron-right").addClass("glyphicon-chevron-down");
	    });
	});
	$(group).find('a[data-toggle="collapse"]').click(function(e){		 
		  if (Modernizr.mq('only screen and (min-width : ' + collapsedWidth + 'px)')) { 
			e.preventDefault()
		    e.stopPropagation();
		  } 
	});
}

function resizeAccordionGroup(group, collapsedWidth) {
	if (collapsedWidth===undefined)
		collapsedWidth = mobileWidth;
	var state = 'show';
	if (Modernizr.mq('only screen and (max-width : ' + collapsedWidth + 'px)')) {
        state = 'hide'; // collapse accordion
        $(group).find('.panel-collapse').addClass('collapse');
    }
	$(group).find('.panel-collapse').collapse(state);
}

/**
 * A function used to add Accordion HTML to each columns of a Column Control component.
 * @param colTitleArray[] - An array contains the title for each column
 * @param index - integer representing the index value of each column control...if you have 3 column controls in a page index will be 0 for the frist  ...1...2 etc
 * @param baseCSSClass - the CSS class for the column control - its a CQ class...
 */
function wrapColumnsWithAccordionHTML(colTitleArray, index, baseCSSClass) {
	// Add only title for large screens, accordions for the rest.
	if (!Modernizr.mq('only screen and (max-width : ' + mobileWidth + 'px)')) {
		insertTitleToColumns(colTitleArray, index, baseCSSClass);	
		return;
	}
	
	var stringContent = "";			    						    			
	var thisControl = $('.' + baseCSSClass)[index];			    			
	var uniqueId = 'columnControlComponent' + colTitleArray.length + index;			    			
	$(thisControl).attr('id', uniqueId);
	
	var colCtr = 1;
	$(thisControl).children('div').each(function(){                                                                                                                        
        var uniqueColumnId = 'columnControlComponent' + colTitleArray.length + index + '-c' + colCtr;                                                                      
        var uniqueAccordionId = "acc-" + uniqueColumnId;
        $(this).attr('id', uniqueColumnId);		
        
        stringContent = 	"<div " + "id='" + uniqueAccordionId + "' class='uscb-accordion'>";
        stringContent += 	"<div class='uscb-layout-row uscb-layout-align-start-center uscb-accordion-header uscb-accordion_panel_head'>";
        stringContent +=	"<img class='uscb-chevron-icon-img uscb-chevron-right-align uscb-padding-R-4' style='height: 30px' src='" + accordionChevronDown + "' alt=''>";
        stringContent +=	"<span class='uscb-sub-heading-2 uscb-bold-text uscb-padding-L-8'>" + colTitleArray[(colCtr-1)] + "</span>";
        stringContent += 	"</div>"; // end uscb-layout-row...
        stringContent += 	"<div class='uscb-accordion_panel uscb-accordion-show-panel'>";
        stringContent += 	$(this).html();
        stringContent += 	"</div>"; // end uscb-accordion_panel...
        stringContent += 	"</div>"; // end uscb-accordion
					    			
		var customDiv = $(stringContent);			
		$('#' + uniqueColumnId ).html(customDiv);
		
		// Attach expand/close handler for each accordion
		$("#" + uniqueAccordionId).click(function () {
			$(this).children('.uscb-accordion_panel').toggleClass("uscb-accordion-show-panel");
			if ($(this).children('.uscb-accordion_panel').hasClass("uscb-accordion-show-panel")) {
				$(this).children('.uscb-accordion_panel_head').children('.uscb-chevron-icon-img').attr('src', accordionChevronDown);		
			} else {
				$(this).children('.uscb-accordion_panel_head').children('.uscb-chevron-icon-img').attr('src', accordionChevronUp);		
			}
		});
		
		colCtr++;
	});//end .children('div').each loop
}//end function

/**
 * A function used to add ONLY the column titles to the column control component when viewed in EDIT mode...
 * @param colTitleArray[] - An array containing the title for each column
 * @param index - integer representing the index value of each column control...if you have 3 column controls in a page index will be 0 for the frist  ...1...2 etc
 * @param baseCSSClass - the CSS class for the column control - its a CQ class...
 */
function insertTitleToColumns(colTitleArray, index, baseCSSClass) {
	var thisControl = $('.' + baseCSSClass)[index];
	var colCtr = 1;
	$(thisControl).children('div').each(function(){ 
		$(this).prepend('<h3>' + colTitleArray[(colCtr - 1)] + '</h3>');
		colCtr++;
	});
}//end function
var $ = jQuery.noConflict();

$(document).ready(function(){
    var $searchBox = $("#searchInput");
    Shadowbox.init({
            handleOversize: "drag",
            modal: true
        });        
    // set StateGeo 
    if(google.loader.ClientLocation){
        var stateMap = {'ak':'alaska','al':'alabama','ar':'arkansas','az':'arizona','ca':'california','co':'colorado','ct':'connecticut','de':'delaware','fl':'florida','ga':'georgia','hi':'hawaii','ia':'iowa','id':'idaho','il':'illinois','in':'indiana','ks':'kansas','ky':'kentucky','la':'louisiana','ma':'massachusetts','md':'maryland','me':'maine','mi':'michigan','mn':'minnesota','mo':'missouri','ms':'mississippi','mt':'montana','nc':'north_carolina','nd':'north_dakota','ne':'nebraska','nh':'new_hampshire','nj':'new_jersey','nm':'new_mexico','nv':'nevada','ny':'new_york','oh':'ohio','ok':'oklahoma','or':'oregon','pa':'pennsylvania','ri':'rhode_island','sc':'south_carolina','sd':'south_dakota','tn':'tennessee','tx':'texas','ut':'utah','va':'virginia','vt':'vermont','wa':'washington','wi':'wisconsin','wv':'west_virginia','wy':'wyoming','dc':'district of columbia'}
        $('#stateGeo').val(stateMap[google.loader.ClientLocation.address.region]);
    }else{
        $('#stateGeo').val("none");
    }
    
    // Focus the cursor on the text box
    $searchBox.focus();

    // bring cursor to the end of the text
    cursorToEnd();
    
    $searchBox.blur();
    
    // Submit the search form when the search button is clicked
    $("#searchButton").click(function() { $("#searchForm").submit(); })
    
    // Disambiguation Dialog
    $('#disambigSelect').click(function() {
        $("#disambigDiv").toggle();
        $('#disambigSelect > div').toggleClass("disambigArrowRight disambigArrowDown");
        
        return false;
    });

    $("#searchTooltip").dialog({ autoOpen:false, width: "auto", minHeight: "170px", resizable: false,
                                dialogClass: "searchTooltipWidget", 
                               position: { my: "left top", at: "right-365 top", of: "#searchFormContainer" } }); 


    $("#qaAnnotationTooltip").dialog({ autoOpen:false, width: "auto", minHeight: "170px", resizable: false,
                               dialogClass: "qaAnnotationTooltipWidget", 
                               position: { my: "left top", at: "right-365 top", of: "#searchFormContainer" } });

    $("#infoPanelAnnotationTooltip").dialog({ autoOpen:false, width: "auto", minHeight: "170px", resizable: false,
                               dialogClass: "infoPanelAnnotationTooltipWidget", 
                               position: { my: "left top", at: "right-365 top", of: "#searchFormContainer" } });


});

//bring cursor to the end of the text 
function cursorToEnd(){
    var $searchBox = $("#searchInput");
    var val = $searchBox.val();
    $searchBox.val('');
    $searchBox.val(val);
}

// Note: Plan b for video search. Pull image via ajax
// http://jquery-howto.blogspot.com/2009/02/how-to-get-youtube-video-screenshot.html
var $ = jQuery.noConflict();

$(document).ready(function(){
    $( "#searchbox, #mobile_searchbox" ).catcomplete({
        source: function( request, response ) {
            $.ajax({
                url: "/suggest", // Needs to set up apache forwarding to hit SOA typeahead url or JSP/Java forwarding
                dataType: "xml",
                data: {
                    query: request.term,
                    stateGeo: $("stateGeo").val(),
                    operationName: "httpsearch"
                },
                success: function( data ) {
                    // Merge the two types we want to display and iterate over them
                    response( $.map($.merge($(data).find("Answer"), $(data).find("result")), function(item) {
                        // Only the answer xml has a value component
                        if($(item).find("Value").text() != ""){ // for the answers before results
                            return {
                                ansValue: $(item).find("Value").text(),
                                ansGeography: $(item).find("Geography").text(),                                                                
                                prefix: $(item).find("Prefix").text(),
                                suffix: $(item).find("Suffix").text(),
                                ansLastUpdated: $(item).find("LastUpdated").text(),
                                // Don't grab the system desc
                                ansDescription: $(item).find("Description").first().text(),
                                ansSystemDesc: $(item).find("System").find("Description").text(),
                                ansSystemURL: $(item).find("System").find("URL").text()
                            }
                        }else if($(item).attr("name") != ""){ // for regular typeahead results
                            return {
                                label: $(item).attr("name"),
                                value: $(item).attr("name"),
                                category: $(item).attr("category")
                            }
                        }
                    }));
                    
                }
            });
        },
        minLength: 3,
        select: function( event, ui ) {
            if(ui.item){
                $('#searchbox, #mobile_searchbox').val(ui.item.value);
                $('input[name=cssp]').val('Typeahead');
            }
            
            $('#searchbox, #mobile_searchbox').closest("form").submit();
        },
        messages: {
            noResults: '',
                results: function() {}
        },
        open: function() {
            $( this ).removeClass( "ui-corner-all" ).addClass( "ui-corner-top" ); 
            if( Modernizr.mq('only screen and (min-width : 1260px)') ){		
            	var inputBoxWidth = $('#searchbox').width();                
                $('.ui-autocomplete').css('width', inputBoxWidth + 'px');
            }else{                
            	var inputBoxWidth = $('#mobile_searchbox').width();                
                $('.ui-autocomplete').css('width', (inputBoxWidth + 35) + 'px');
            }                        
        },
        close: function() {
            $( this ).removeClass( "ui-corner-top" ).addClass( "ui-corner-all" );
        }
    }); 
});

// Custom Typeahead
$.widget( "custom.catcomplete", $.ui.autocomplete, {
    _renderMenu: function( ul, items ) {
        var self = this, currentCategory = "";
        
        $.each( items, function( index, item ) {
            // double check for blank nodes from the jquery find method
            if(item.ansValue != null || item.label != null){
                // Decorate the system field to include latest estimate date
                var sysName = "";
                
                if(item.ansLastUpdated != ""){
                    var d = new Date(item.ansLastUpdated * 1000); // Given seconds originally
                    sysName = d.getFullYear() + " " + item.ansSystemDesc;
                }else{
                    sysName = item.ansSystemDesc;
                }
                
                if(item.ansValue != null){

                        ul.append("<li class='autocomplete-instant-answer'> " +
                                "<div class='ia-label'><span class='ia-geo'>" + item.ansGeography + "</span><br/><span class='ia-stat'>" + item.ansDescription + 
                                "</span></div>" +
                                "<div class='ia-stat-value'>"+ item.prefix + "" + item.ansValue.replace(/(\d)(?=(\d\d\d)+(?!\d))/g, "$1,") + " " +item.suffix + "</div>" +
                                
                                "<div class='ia-system'><a href='" + item.ansSystemURL  + "?cssp=Typeahead'>Source: " + sysName + "</a></div>" +
                                "</li>");
                    
                }else if ( item.category != currentCategory ) {
                    ul.append( "<li class='ui-autocomplete-category'>" + item.category + "</li>" );
                    currentCategory = item.category;
                }
                
                self._renderItemData( ul, item );
            }
            
        });
    }
});
/*
 * Shadowbox.js, version 3.0.3
 * http://shadowbox-js.com/
 *
 * Copyright 2007-2010, Michael J. I. Jackson
 * Date: 2011-05-14 08:09:50 +0000
 */
(function(au,k){var Q={version:"3.0.3"};var J=navigator.userAgent.toLowerCase();if(J.indexOf("windows")>-1||J.indexOf("win32")>-1){Q.isWindows=true}else{if(J.indexOf("macintosh")>-1||J.indexOf("mac os x")>-1){Q.isMac=true}else{if(J.indexOf("linux")>-1){Q.isLinux=true}}}Q.isIE=J.indexOf("msie")>-1;Q.isIE6=J.indexOf("msie 6")>-1;Q.isIE7=J.indexOf("msie 7")>-1;Q.isGecko=J.indexOf("gecko")>-1&&J.indexOf("safari")==-1;Q.isWebKit=J.indexOf("applewebkit/")>-1;var ab=/#(.+)$/,af=/^(light|shadow)box\[(.*?)\]/i,az=/\s*([a-z_]*?)\s*=\s*(.+)\s*/,f=/[0-9a-z]+$/i,aD=/(.+\/)shadowbox\.js/i;var A=false,a=false,l={},z=0,R,ap;Q.current=-1;Q.dimensions=null;Q.ease=function(K){return 1+Math.pow(K-1,3)};Q.errorInfo={fla:{name:"Flash",url:"http://www.adobe.com/products/flashplayer/"},qt:{name:"QuickTime",url:"http://www.apple.com/quicktime/download/"},wmp:{name:"Windows Media Player",url:"http://www.microsoft.com/windows/windowsmedia/"},f4m:{name:"Flip4Mac",url:"http://www.flip4mac.com/wmv_download.htm"}};Q.gallery=[];Q.onReady=aj;Q.path=null;Q.player=null;Q.playerId="sb-player";Q.options={animate:true,animateFade:true,autoplayMovies:true,continuous:false,enableKeys:true,flashParams:{bgcolor:"#000000",allowfullscreen:true},flashVars:{},flashVersion:"9.0.115",handleOversize:"resize",handleUnsupported:"link",onChange:aj,onClose:aj,onFinish:aj,onOpen:aj,showMovieControls:true,skipSetup:false,slideshowDelay:0,viewportPadding:20};Q.getCurrent=function(){return Q.current>-1?Q.gallery[Q.current]:null};Q.hasNext=function(){return Q.gallery.length>1&&(Q.current!=Q.gallery.length-1||Q.options.continuous)};Q.isOpen=function(){return A};Q.isPaused=function(){return ap=="pause"};Q.applyOptions=function(K){l=aC({},Q.options);aC(Q.options,K)};Q.revertOptions=function(){aC(Q.options,l)};Q.init=function(aG,aJ){if(a){return}a=true;if(Q.skin.options){aC(Q.options,Q.skin.options)}if(aG){aC(Q.options,aG)}if(!Q.path){var aI,S=document.getElementsByTagName("script");for(var aH=0,K=S.length;aH<K;++aH){aI=aD.exec(S[aH].src);if(aI){Q.path=aI[1];break}}}if(aJ){Q.onReady=aJ}P()};Q.open=function(S){if(A){return}var K=Q.makeGallery(S);Q.gallery=K[0];Q.current=K[1];S=Q.getCurrent();if(S==null){return}Q.applyOptions(S.options||{});G();if(Q.gallery.length){S=Q.getCurrent();if(Q.options.onOpen(S)===false){return}A=true;Q.skin.onOpen(S,c)}};Q.close=function(){if(!A){return}A=false;if(Q.player){Q.player.remove();Q.player=null}if(typeof ap=="number"){clearTimeout(ap);ap=null}z=0;aq(false);Q.options.onClose(Q.getCurrent());Q.skin.onClose();Q.revertOptions()};Q.play=function(){if(!Q.hasNext()){return}if(!z){z=Q.options.slideshowDelay*1000}if(z){R=aw();ap=setTimeout(function(){z=R=0;Q.next()},z);if(Q.skin.onPlay){Q.skin.onPlay()}}};Q.pause=function(){if(typeof ap!="number"){return}z=Math.max(0,z-(aw()-R));if(z){clearTimeout(ap);ap="pause";if(Q.skin.onPause){Q.skin.onPause()}}};Q.change=function(K){if(!(K in Q.gallery)){if(Q.options.continuous){K=(K<0?Q.gallery.length+K:0);if(!(K in Q.gallery)){return}}else{return}}Q.current=K;if(typeof ap=="number"){clearTimeout(ap);ap=null;z=R=0}Q.options.onChange(Q.getCurrent());c(true)};Q.next=function(){Q.change(Q.current+1)};Q.previous=function(){Q.change(Q.current-1)};Q.setDimensions=function(aS,aJ,aQ,aR,aI,K,aO,aL){var aN=aS,aH=aJ;var aM=2*aO+aI;if(aS+aM>aQ){aS=aQ-aM}var aG=2*aO+K;if(aJ+aG>aR){aJ=aR-aG}var S=(aN-aS)/aN,aP=(aH-aJ)/aH,aK=(S>0||aP>0);if(aL&&aK){if(S>aP){aJ=Math.round((aH/aN)*aS)}else{if(aP>S){aS=Math.round((aN/aH)*aJ)}}}Q.dimensions={height:aS+aI,width:aJ+K,innerHeight:aS,innerWidth:aJ,top:Math.floor((aQ-(aS+aM))/2+aO),left:Math.floor((aR-(aJ+aG))/2+aO),oversized:aK};return Q.dimensions};Q.makeGallery=function(aI){var K=[],aH=-1;if(typeof aI=="string"){aI=[aI]}if(typeof aI.length=="number"){aF(aI,function(aK,aL){if(aL.content){K[aK]=aL}else{K[aK]={content:aL}}});aH=0}else{if(aI.tagName){var S=Q.getCache(aI);aI=S?S:Q.makeObject(aI)}if(aI.gallery){K=[];var aJ;for(var aG in Q.cache){aJ=Q.cache[aG];if(aJ.gallery&&aJ.gallery==aI.gallery){if(aH==-1&&aJ.content==aI.content){aH=K.length}K.push(aJ)}}if(aH==-1){K.unshift(aI);aH=0}}else{K=[aI];aH=0}}aF(K,function(aK,aL){K[aK]=aC({},aL)});return[K,aH]};Q.makeObject=function(aH,aG){var aI={content:aH.href,title:aH.getAttribute("title")||"",link:aH};if(aG){aG=aC({},aG);aF(["player","title","height","width","gallery"],function(aJ,aK){if(typeof aG[aK]!="undefined"){aI[aK]=aG[aK];delete aG[aK]}});aI.options=aG}else{aI.options={}}if(!aI.player){aI.player=Q.getPlayer(aI.content)}var K=aH.getAttribute("rel");if(K){var S=K.match(af);if(S){aI.gallery=escape(S[2])}aF(K.split(";"),function(aJ,aK){S=aK.match(az);if(S){aI[S[1]]=S[2]}})}return aI};Q.getPlayer=function(aG){if(aG.indexOf("#")>-1&&aG.indexOf(document.location.href)==0){return"inline"}var aH=aG.indexOf("?");if(aH>-1){aG=aG.substring(0,aH)}var S,K=aG.match(f);if(K){S=K[0].toLowerCase()}if(S){if(Q.img&&Q.img.ext.indexOf(S)>-1){return"img"}if(Q.swf&&Q.swf.ext.indexOf(S)>-1){return"swf"}if(Q.flv&&Q.flv.ext.indexOf(S)>-1){return"flv"}if(Q.qt&&Q.qt.ext.indexOf(S)>-1){if(Q.wmp&&Q.wmp.ext.indexOf(S)>-1){return"qtwmp"}else{return"qt"}}if(Q.wmp&&Q.wmp.ext.indexOf(S)>-1){return"wmp"}}return"iframe"};function G(){var aH=Q.errorInfo,aI=Q.plugins,aK,aL,aO,aG,aN,S,aM,K;for(var aJ=0;aJ<Q.gallery.length;++aJ){aK=Q.gallery[aJ];aL=false;aO=null;switch(aK.player){case"flv":case"swf":if(!aI.fla){aO="fla"}break;case"qt":if(!aI.qt){aO="qt"}break;case"wmp":if(Q.isMac){if(aI.qt&&aI.f4m){aK.player="qt"}else{aO="qtf4m"}}else{if(!aI.wmp){aO="wmp"}}break;case"qtwmp":if(aI.qt){aK.player="qt"}else{if(aI.wmp){aK.player="wmp"}else{aO="qtwmp"}}break}if(aO){if(Q.options.handleUnsupported=="link"){switch(aO){case"qtf4m":aN="shared";S=[aH.qt.url,aH.qt.name,aH.f4m.url,aH.f4m.name];break;case"qtwmp":aN="either";S=[aH.qt.url,aH.qt.name,aH.wmp.url,aH.wmp.name];break;default:aN="single";S=[aH[aO].url,aH[aO].name]}aK.player="html";aK.content='<div class="sb-message">'+s(Q.lang.errors[aN],S)+"</div>"}else{aL=true}}else{if(aK.player=="inline"){aG=ab.exec(aK.content);if(aG){aM=ad(aG[1]);if(aM){aK.content=aM.innerHTML}else{aL=true}}else{aL=true}}else{if(aK.player=="swf"||aK.player=="flv"){K=(aK.options&&aK.options.flashVersion)||Q.options.flashVersion;if(Q.flash&&!Q.flash.hasFlashPlayerVersion(K)){aK.width=310;aK.height=177}}}}if(aL){Q.gallery.splice(aJ,1);if(aJ<Q.current){--Q.current}else{if(aJ==Q.current){Q.current=aJ>0?aJ-1:aJ}}--aJ}}}function aq(K){if(!Q.options.enableKeys){return}(K?F:M)(document,"keydown",an)}function an(aG){if(aG.metaKey||aG.shiftKey||aG.altKey||aG.ctrlKey){return}var S=v(aG),K;switch(S){case 81:case 88:case 27:K=Q.close;break;case 37:K=Q.previous;break;case 39:K=Q.next;break;case 32:K=typeof ap=="number"?Q.pause:Q.play;break}if(K){n(aG);K()}}function c(aK){aq(false);var aJ=Q.getCurrent();var aG=(aJ.player=="inline"?"html":aJ.player);if(typeof Q[aG]!="function"){throw"unknown player "+aG}if(aK){Q.player.remove();Q.revertOptions();Q.applyOptions(aJ.options||{})}Q.player=new Q[aG](aJ,Q.playerId);if(Q.gallery.length>1){var aH=Q.gallery[Q.current+1]||Q.gallery[0];if(aH.player=="img"){var S=new Image();S.src=aH.content}var aI=Q.gallery[Q.current-1]||Q.gallery[Q.gallery.length-1];if(aI.player=="img"){var K=new Image();K.src=aI.content}}Q.skin.onLoad(aK,W)}function W(){if(!A){return}if(typeof Q.player.ready!="undefined"){var K=setInterval(function(){if(A){if(Q.player.ready){clearInterval(K);K=null;Q.skin.onReady(e)}}else{clearInterval(K);K=null}},10)}else{Q.skin.onReady(e)}}function e(){if(!A){return}Q.player.append(Q.skin.body,Q.dimensions);Q.skin.onShow(I)}function I(){if(!A){return}if(Q.player.onLoad){Q.player.onLoad()}Q.options.onFinish(Q.getCurrent());if(!Q.isPaused()){Q.play()}aq(true)}if(!Array.prototype.indexOf){Array.prototype.indexOf=function(S,aG){var K=this.length>>>0;aG=aG||0;if(aG<0){aG+=K}for(;aG<K;++aG){if(aG in this&&this[aG]===S){return aG}}return -1}}function aw(){return(new Date).getTime()}function aC(K,aG){for(var S in aG){K[S]=aG[S]}return K}function aF(aH,aI){var S=0,K=aH.length;for(var aG=aH[0];S<K&&aI.call(aG,S,aG)!==false;aG=aH[++S]){}}function s(S,K){return S.replace(/\{(\w+?)\}/g,function(aG,aH){return K[aH]})}function aj(){}function ad(K){return document.getElementById(K)}function C(K){K.parentNode.removeChild(K)}var h=true,x=true;function d(){var K=document.body,S=document.createElement("div");h=typeof S.style.opacity==="string";S.style.position="fixed";S.style.margin=0;S.style.top="20px";K.appendChild(S,K.firstChild);x=S.offsetTop==20;K.removeChild(S)}Q.getStyle=(function(){var K=/opacity=([^)]*)/,S=document.defaultView&&document.defaultView.getComputedStyle;return function(aJ,aI){var aH;if(!h&&aI=="opacity"&&aJ.currentStyle){aH=K.test(aJ.currentStyle.filter||"")?(parseFloat(RegExp.$1)/100)+"":"";return aH===""?"1":aH}if(S){var aG=S(aJ,null);if(aG){aH=aG[aI]}if(aI=="opacity"&&aH==""){aH="1"}}else{aH=aJ.currentStyle[aI]}return aH}})();Q.appendHTML=function(aG,S){if(aG.insertAdjacentHTML){aG.insertAdjacentHTML("BeforeEnd",S)}else{if(aG.lastChild){var K=aG.ownerDocument.createRange();K.setStartAfter(aG.lastChild);var aH=K.createContextualFragment(S);aG.appendChild(aH)}else{aG.innerHTML=S}}};Q.getWindowSize=function(K){if(document.compatMode==="CSS1Compat"){return document.documentElement["client"+K]}return document.body["client"+K]};Q.setOpacity=function(aG,K){var S=aG.style;if(h){S.opacity=(K==1?"":K)}else{S.zoom=1;if(K==1){if(typeof S.filter=="string"&&(/alpha/i).test(S.filter)){S.filter=S.filter.replace(/\s*[\w\.]*alpha\([^\)]*\);?/gi,"")}}else{S.filter=(S.filter||"").replace(/\s*[\w\.]*alpha\([^\)]*\)/gi,"")+" alpha(opacity="+(K*100)+")"}}};Q.clearOpacity=function(K){Q.setOpacity(K,1)};function o(K){return K.target}function V(K){return[K.pageX,K.pageY]}function n(K){K.preventDefault()}function v(K){return K.keyCode}function F(aG,S,K){jQuery(aG).bind(S,K)}function M(aG,S,K){jQuery(aG).unbind(S,K)}jQuery.fn.shadowbox=function(K){return this.each(function(){var aG=jQuery(this);var aH=jQuery.extend({},K||{},jQuery.metadata?aG.metadata():jQuery.meta?aG.data():{});var S=this.className||"";aH.width=parseInt((S.match(/w:(\d+)/)||[])[1])||aH.width;aH.height=parseInt((S.match(/h:(\d+)/)||[])[1])||aH.height;Shadowbox.setup(aG,aH)})};var y=false,al;if(document.addEventListener){al=function(){document.removeEventListener("DOMContentLoaded",al,false);Q.load()}}else{if(document.attachEvent){al=function(){if(document.readyState==="complete"){document.detachEvent("onreadystatechange",al);Q.load()}}}}function g(){if(y){return}try{document.documentElement.doScroll("left")}catch(K){setTimeout(g,1);return}Q.load()}function P(){if(document.readyState==="complete"){return Q.load()}if(document.addEventListener){document.addEventListener("DOMContentLoaded",al,false);au.addEventListener("load",Q.load,false)}else{if(document.attachEvent){document.attachEvent("onreadystatechange",al);au.attachEvent("onload",Q.load);var K=false;try{K=au.frameElement===null}catch(S){}if(document.documentElement.doScroll&&K){g()}}}}Q.load=function(){if(y){return}if(!document.body){return setTimeout(Q.load,13)}y=true;d();Q.onReady();if(!Q.options.skipSetup){Q.setup()}Q.skin.init()};Q.plugins={};if(navigator.plugins&&navigator.plugins.length){var w=[];aF(navigator.plugins,function(K,S){w.push(S.name)});w=w.join(",");var ai=w.indexOf("Flip4Mac")>-1;Q.plugins={fla:w.indexOf("Shockwave Flash")>-1,qt:w.indexOf("QuickTime")>-1,wmp:!ai&&w.indexOf("Windows Media")>-1,f4m:ai}}else{var p=function(K){var S;try{S=new ActiveXObject(K)}catch(aG){}return !!S};Q.plugins={fla:p("ShockwaveFlash.ShockwaveFlash"),qt:p("QuickTime.QuickTime"),wmp:p("wmplayer.ocx"),f4m:false}}var X=/^(light|shadow)box/i,am="shadowboxCacheKey",b=1;Q.cache={};Q.select=function(S){var aG=[];if(!S){var K;aF(document.getElementsByTagName("a"),function(aJ,aK){K=aK.getAttribute("rel");if(K&&X.test(K)){aG.push(aK)}})}else{var aI=S.length;if(aI){if(typeof S=="string"){if(Q.find){aG=Q.find(S)}}else{if(aI==2&&typeof S[0]=="string"&&S[1].nodeType){if(Q.find){aG=Q.find(S[0],S[1])}}else{for(var aH=0;aH<aI;++aH){aG[aH]=S[aH]}}}}else{aG.push(S)}}return aG};Q.setup=function(K,S){aF(Q.select(K),function(aG,aH){Q.addCache(aH,S)})};Q.teardown=function(K){aF(Q.select(K),function(S,aG){Q.removeCache(aG)})};Q.addCache=function(aG,K){var S=aG[am];if(S==k){S=b++;aG[am]=S;F(aG,"click",u)}Q.cache[S]=Q.makeObject(aG,K)};Q.removeCache=function(K){M(K,"click",u);delete Q.cache[K[am]];K[am]=null};Q.getCache=function(S){var K=S[am];return(K in Q.cache&&Q.cache[K])};Q.clearCache=function(){for(var K in Q.cache){Q.removeCache(Q.cache[K].link)}Q.cache={}};function u(K){Q.open(this);if(Q.gallery.length){n(K)}}
/*
 * Sizzle CSS Selector Engine - v1.0
 *  Copyright 2009, The Dojo Foundation
 *  Released under the MIT, BSD, and GPL Licenses.
 *  More information: http://sizzlejs.com/
 *
 * Modified for inclusion in Shadowbox.js
 */
Q.find=(function(){var aP=/((?:\((?:\([^()]+\)|[^()]+)+\)|\[(?:\[[^[\]]*\]|['"][^'"]*['"]|[^[\]'"]+)+\]|\\.|[^ >+~,(\[\\]+)+|[>+~])(\s*,\s*)?((?:.|\r|\n)*)/g,aQ=0,aS=Object.prototype.toString,aK=false,aJ=true;[0,0].sort(function(){aJ=false;return 0});var aG=function(a1,aW,a4,a5){a4=a4||[];var a7=aW=aW||document;if(aW.nodeType!==1&&aW.nodeType!==9){return[]}if(!a1||typeof a1!=="string"){return a4}var a2=[],aY,a9,bc,aX,a0=true,aZ=aH(aW),a6=a1;while((aP.exec(""),aY=aP.exec(a6))!==null){a6=aY[3];a2.push(aY[1]);if(aY[2]){aX=aY[3];break}}if(a2.length>1&&aL.exec(a1)){if(a2.length===2&&aM.relative[a2[0]]){a9=aT(a2[0]+a2[1],aW)}else{a9=aM.relative[a2[0]]?[aW]:aG(a2.shift(),aW);while(a2.length){a1=a2.shift();if(aM.relative[a1]){a1+=a2.shift()}a9=aT(a1,a9)}}}else{if(!a5&&a2.length>1&&aW.nodeType===9&&!aZ&&aM.match.ID.test(a2[0])&&!aM.match.ID.test(a2[a2.length-1])){var a8=aG.find(a2.shift(),aW,aZ);aW=a8.expr?aG.filter(a8.expr,a8.set)[0]:a8.set[0]}if(aW){var a8=a5?{expr:a2.pop(),set:aO(a5)}:aG.find(a2.pop(),a2.length===1&&(a2[0]==="~"||a2[0]==="+")&&aW.parentNode?aW.parentNode:aW,aZ);a9=a8.expr?aG.filter(a8.expr,a8.set):a8.set;if(a2.length>0){bc=aO(a9)}else{a0=false}while(a2.length){var bb=a2.pop(),ba=bb;if(!aM.relative[bb]){bb=""}else{ba=a2.pop()}if(ba==null){ba=aW}aM.relative[bb](bc,ba,aZ)}}else{bc=a2=[]}}if(!bc){bc=a9}if(!bc){throw"Syntax error, unrecognized expression: "+(bb||a1)}if(aS.call(bc)==="[object Array]"){if(!a0){a4.push.apply(a4,bc)}else{if(aW&&aW.nodeType===1){for(var a3=0;bc[a3]!=null;a3++){if(bc[a3]&&(bc[a3]===true||bc[a3].nodeType===1&&aN(aW,bc[a3]))){a4.push(a9[a3])}}}else{for(var a3=0;bc[a3]!=null;a3++){if(bc[a3]&&bc[a3].nodeType===1){a4.push(a9[a3])}}}}}else{aO(bc,a4)}if(aX){aG(aX,a7,a4,a5);aG.uniqueSort(a4)}return a4};aG.uniqueSort=function(aX){if(aR){aK=aJ;aX.sort(aR);if(aK){for(var aW=1;aW<aX.length;aW++){if(aX[aW]===aX[aW-1]){aX.splice(aW--,1)}}}}return aX};aG.matches=function(aW,aX){return aG(aW,null,null,aX)};aG.find=function(a3,aW,a4){var a2,a0;if(!a3){return[]}for(var aZ=0,aY=aM.order.length;aZ<aY;aZ++){var a1=aM.order[aZ],a0;if((a0=aM.leftMatch[a1].exec(a3))){var aX=a0[1];a0.splice(1,1);if(aX.substr(aX.length-1)!=="\\"){a0[1]=(a0[1]||"").replace(/\\/g,"");a2=aM.find[a1](a0,aW,a4);if(a2!=null){a3=a3.replace(aM.match[a1],"");break}}}}if(!a2){a2=aW.getElementsByTagName("*")}return{set:a2,expr:a3}};aG.filter=function(a6,a5,a9,aZ){var aY=a6,bb=[],a3=a5,a1,aW,a2=a5&&a5[0]&&aH(a5[0]);while(a6&&a5.length){for(var a4 in aM.filter){if((a1=aM.match[a4].exec(a6))!=null){var aX=aM.filter[a4],ba,a8;aW=false;if(a3===bb){bb=[]}if(aM.preFilter[a4]){a1=aM.preFilter[a4](a1,a3,a9,bb,aZ,a2);if(!a1){aW=ba=true}else{if(a1===true){continue}}}if(a1){for(var a0=0;(a8=a3[a0])!=null;a0++){if(a8){ba=aX(a8,a1,a0,a3);var a7=aZ^!!ba;if(a9&&ba!=null){if(a7){aW=true}else{a3[a0]=false}}else{if(a7){bb.push(a8);aW=true}}}}}if(ba!==k){if(!a9){a3=bb}a6=a6.replace(aM.match[a4],"");if(!aW){return[]}break}}}if(a6===aY){if(aW==null){throw"Syntax error, unrecognized expression: "+a6}else{break}}aY=a6}return a3};var aM=aG.selectors={order:["ID","NAME","TAG"],match:{ID:/#((?:[\w\u00c0-\uFFFF-]|\\.)+)/,CLASS:/\.((?:[\w\u00c0-\uFFFF-]|\\.)+)/,NAME:/\[name=['"]*((?:[\w\u00c0-\uFFFF-]|\\.)+)['"]*\]/,ATTR:/\[\s*((?:[\w\u00c0-\uFFFF-]|\\.)+)\s*(?:(\S?=)\s*(['"]*)(.*?)\3|)\s*\]/,TAG:/^((?:[\w\u00c0-\uFFFF\*-]|\\.)+)/,CHILD:/:(only|nth|last|first)-child(?:\((even|odd|[\dn+-]*)\))?/,POS:/:(nth|eq|gt|lt|first|last|even|odd)(?:\((\d*)\))?(?=[^-]|$)/,PSEUDO:/:((?:[\w\u00c0-\uFFFF-]|\\.)+)(?:\((['"]*)((?:\([^\)]+\)|[^\2\(\)]*)+)\2\))?/},leftMatch:{},attrMap:{"class":"className","for":"htmlFor"},attrHandle:{href:function(aW){return aW.getAttribute("href")}},relative:{"+":function(a2,aX){var aZ=typeof aX==="string",a1=aZ&&!/\W/.test(aX),a3=aZ&&!a1;if(a1){aX=aX.toLowerCase()}for(var aY=0,aW=a2.length,a0;aY<aW;aY++){if((a0=a2[aY])){while((a0=a0.previousSibling)&&a0.nodeType!==1){}a2[aY]=a3||a0&&a0.nodeName.toLowerCase()===aX?a0||false:a0===aX}}if(a3){aG.filter(aX,a2,true)}},">":function(a2,aX){var a0=typeof aX==="string";if(a0&&!/\W/.test(aX)){aX=aX.toLowerCase();for(var aY=0,aW=a2.length;aY<aW;aY++){var a1=a2[aY];if(a1){var aZ=a1.parentNode;a2[aY]=aZ.nodeName.toLowerCase()===aX?aZ:false}}}else{for(var aY=0,aW=a2.length;aY<aW;aY++){var a1=a2[aY];if(a1){a2[aY]=a0?a1.parentNode:a1.parentNode===aX}}if(a0){aG.filter(aX,a2,true)}}},"":function(aZ,aX,a1){var aY=aQ++,aW=aU;if(typeof aX==="string"&&!/\W/.test(aX)){var a0=aX=aX.toLowerCase();aW=K}aW("parentNode",aX,aY,aZ,a0,a1)},"~":function(aZ,aX,a1){var aY=aQ++,aW=aU;if(typeof aX==="string"&&!/\W/.test(aX)){var a0=aX=aX.toLowerCase();aW=K}aW("previousSibling",aX,aY,aZ,a0,a1)}},find:{ID:function(aX,aY,aZ){if(typeof aY.getElementById!=="undefined"&&!aZ){var aW=aY.getElementById(aX[1]);return aW?[aW]:[]}},NAME:function(aY,a1){if(typeof a1.getElementsByName!=="undefined"){var aX=[],a0=a1.getElementsByName(aY[1]);for(var aZ=0,aW=a0.length;aZ<aW;aZ++){if(a0[aZ].getAttribute("name")===aY[1]){aX.push(a0[aZ])}}return aX.length===0?null:aX}},TAG:function(aW,aX){return aX.getElementsByTagName(aW[1])}},preFilter:{CLASS:function(aZ,aX,aY,aW,a2,a3){aZ=" "+aZ[1].replace(/\\/g,"")+" ";if(a3){return aZ}for(var a0=0,a1;(a1=aX[a0])!=null;a0++){if(a1){if(a2^(a1.className&&(" "+a1.className+" ").replace(/[\t\n]/g," ").indexOf(aZ)>=0)){if(!aY){aW.push(a1)}}else{if(aY){aX[a0]=false}}}}return false},ID:function(aW){return aW[1].replace(/\\/g,"")},TAG:function(aX,aW){return aX[1].toLowerCase()},CHILD:function(aW){if(aW[1]==="nth"){var aX=/(-?)(\d*)n((?:\+|-)?\d*)/.exec(aW[2]==="even"&&"2n"||aW[2]==="odd"&&"2n+1"||!/\D/.test(aW[2])&&"0n+"+aW[2]||aW[2]);aW[2]=(aX[1]+(aX[2]||1))-0;aW[3]=aX[3]-0}aW[0]=aQ++;return aW},ATTR:function(a0,aX,aY,aW,a1,a2){var aZ=a0[1].replace(/\\/g,"");if(!a2&&aM.attrMap[aZ]){a0[1]=aM.attrMap[aZ]}if(a0[2]==="~="){a0[4]=" "+a0[4]+" "}return a0},PSEUDO:function(a0,aX,aY,aW,a1){if(a0[1]==="not"){if((aP.exec(a0[3])||"").length>1||/^\w/.test(a0[3])){a0[3]=aG(a0[3],null,null,aX)}else{var aZ=aG.filter(a0[3],aX,aY,true^a1);if(!aY){aW.push.apply(aW,aZ)}return false}}else{if(aM.match.POS.test(a0[0])||aM.match.CHILD.test(a0[0])){return true}}return a0},POS:function(aW){aW.unshift(true);return aW}},filters:{enabled:function(aW){return aW.disabled===false&&aW.type!=="hidden"},disabled:function(aW){return aW.disabled===true},checked:function(aW){return aW.checked===true},selected:function(aW){aW.parentNode.selectedIndex;return aW.selected===true},parent:function(aW){return !!aW.firstChild},empty:function(aW){return !aW.firstChild},has:function(aY,aX,aW){return !!aG(aW[3],aY).length},header:function(aW){return/h\d/i.test(aW.nodeName)},text:function(aW){return"text"===aW.type},radio:function(aW){return"radio"===aW.type},checkbox:function(aW){return"checkbox"===aW.type},file:function(aW){return"file"===aW.type},password:function(aW){return"password"===aW.type},submit:function(aW){return"submit"===aW.type},image:function(aW){return"image"===aW.type},reset:function(aW){return"reset"===aW.type},button:function(aW){return"button"===aW.type||aW.nodeName.toLowerCase()==="button"},input:function(aW){return/input|select|textarea|button/i.test(aW.nodeName)}},setFilters:{first:function(aX,aW){return aW===0},last:function(aY,aX,aW,aZ){return aX===aZ.length-1},even:function(aX,aW){return aW%2===0},odd:function(aX,aW){return aW%2===1},lt:function(aY,aX,aW){return aX<aW[3]-0},gt:function(aY,aX,aW){return aX>aW[3]-0},nth:function(aY,aX,aW){return aW[3]-0===aX},eq:function(aY,aX,aW){return aW[3]-0===aX}},filter:{PSEUDO:function(a2,aY,aZ,a3){var aX=aY[1],a0=aM.filters[aX];if(a0){return a0(a2,aZ,aY,a3)}else{if(aX==="contains"){return(a2.textContent||a2.innerText||S([a2])||"").indexOf(aY[3])>=0}else{if(aX==="not"){var a1=aY[3];for(var aZ=0,aW=a1.length;aZ<aW;aZ++){if(a1[aZ]===a2){return false}}return true}else{throw"Syntax error, unrecognized expression: "+aX}}}},CHILD:function(aW,aZ){var a2=aZ[1],aX=aW;switch(a2){case"only":case"first":while((aX=aX.previousSibling)){if(aX.nodeType===1){return false}}if(a2==="first"){return true}aX=aW;case"last":while((aX=aX.nextSibling)){if(aX.nodeType===1){return false}}return true;case"nth":var aY=aZ[2],a5=aZ[3];if(aY===1&&a5===0){return true}var a1=aZ[0],a4=aW.parentNode;if(a4&&(a4.sizcache!==a1||!aW.nodeIndex)){var a0=0;for(aX=a4.firstChild;aX;aX=aX.nextSibling){if(aX.nodeType===1){aX.nodeIndex=++a0}}a4.sizcache=a1}var a3=aW.nodeIndex-a5;if(aY===0){return a3===0}else{return(a3%aY===0&&a3/aY>=0)}}},ID:function(aX,aW){return aX.nodeType===1&&aX.getAttribute("id")===aW},TAG:function(aX,aW){return(aW==="*"&&aX.nodeType===1)||aX.nodeName.toLowerCase()===aW},CLASS:function(aX,aW){return(" "+(aX.className||aX.getAttribute("class"))+" ").indexOf(aW)>-1},ATTR:function(a1,aZ){var aY=aZ[1],aW=aM.attrHandle[aY]?aM.attrHandle[aY](a1):a1[aY]!=null?a1[aY]:a1.getAttribute(aY),a2=aW+"",a0=aZ[2],aX=aZ[4];return aW==null?a0==="!=":a0==="="?a2===aX:a0==="*="?a2.indexOf(aX)>=0:a0==="~="?(" "+a2+" ").indexOf(aX)>=0:!aX?a2&&aW!==false:a0==="!="?a2!==aX:a0==="^="?a2.indexOf(aX)===0:a0==="$="?a2.substr(a2.length-aX.length)===aX:a0==="|="?a2===aX||a2.substr(0,aX.length+1)===aX+"-":false},POS:function(a0,aX,aY,a1){var aW=aX[2],aZ=aM.setFilters[aW];if(aZ){return aZ(a0,aY,aX,a1)}}}};var aL=aM.match.POS;for(var aI in aM.match){aM.match[aI]=new RegExp(aM.match[aI].source+/(?![^\[]*\])(?![^\(]*\))/.source);aM.leftMatch[aI]=new RegExp(/(^(?:.|\r|\n)*?)/.source+aM.match[aI].source)}var aO=function(aX,aW){aX=Array.prototype.slice.call(aX,0);if(aW){aW.push.apply(aW,aX);return aW}return aX};try{Array.prototype.slice.call(document.documentElement.childNodes,0)}catch(aV){aO=function(a0,aZ){var aX=aZ||[];if(aS.call(a0)==="[object Array]"){Array.prototype.push.apply(aX,a0)}else{if(typeof a0.length==="number"){for(var aY=0,aW=a0.length;aY<aW;aY++){aX.push(a0[aY])}}else{for(var aY=0;a0[aY];aY++){aX.push(a0[aY])}}}return aX}}var aR;if(document.documentElement.compareDocumentPosition){aR=function(aX,aW){if(!aX.compareDocumentPosition||!aW.compareDocumentPosition){if(aX==aW){aK=true}return aX.compareDocumentPosition?-1:1}var aY=aX.compareDocumentPosition(aW)&4?-1:aX===aW?0:1;if(aY===0){aK=true}return aY}}else{if("sourceIndex" in document.documentElement){aR=function(aX,aW){if(!aX.sourceIndex||!aW.sourceIndex){if(aX==aW){aK=true}return aX.sourceIndex?-1:1}var aY=aX.sourceIndex-aW.sourceIndex;if(aY===0){aK=true}return aY}}else{if(document.createRange){aR=function(aZ,aX){if(!aZ.ownerDocument||!aX.ownerDocument){if(aZ==aX){aK=true}return aZ.ownerDocument?-1:1}var aY=aZ.ownerDocument.createRange(),aW=aX.ownerDocument.createRange();aY.setStart(aZ,0);aY.setEnd(aZ,0);aW.setStart(aX,0);aW.setEnd(aX,0);var a0=aY.compareBoundaryPoints(Range.START_TO_END,aW);if(a0===0){aK=true}return a0}}}}function S(aW){var aX="",aZ;for(var aY=0;aW[aY];aY++){aZ=aW[aY];if(aZ.nodeType===3||aZ.nodeType===4){aX+=aZ.nodeValue}else{if(aZ.nodeType!==8){aX+=S(aZ.childNodes)}}}return aX}(function(){var aX=document.createElement("div"),aY="script"+(new Date).getTime();aX.innerHTML="<a name='"+aY+"'/>";var aW=document.documentElement;aW.insertBefore(aX,aW.firstChild);if(document.getElementById(aY)){aM.find.ID=function(a0,a1,a2){if(typeof a1.getElementById!=="undefined"&&!a2){var aZ=a1.getElementById(a0[1]);return aZ?aZ.id===a0[1]||typeof aZ.getAttributeNode!=="undefined"&&aZ.getAttributeNode("id").nodeValue===a0[1]?[aZ]:k:[]}};aM.filter.ID=function(a1,aZ){var a0=typeof a1.getAttributeNode!=="undefined"&&a1.getAttributeNode("id");return a1.nodeType===1&&a0&&a0.nodeValue===aZ}}aW.removeChild(aX);aW=aX=null})();(function(){var aW=document.createElement("div");aW.appendChild(document.createComment(""));if(aW.getElementsByTagName("*").length>0){aM.find.TAG=function(aX,a1){var a0=a1.getElementsByTagName(aX[1]);if(aX[1]==="*"){var aZ=[];for(var aY=0;a0[aY];aY++){if(a0[aY].nodeType===1){aZ.push(a0[aY])}}a0=aZ}return a0}}aW.innerHTML="<a href='#'></a>";if(aW.firstChild&&typeof aW.firstChild.getAttribute!=="undefined"&&aW.firstChild.getAttribute("href")!=="#"){aM.attrHandle.href=function(aX){return aX.getAttribute("href",2)}}aW=null})();if(document.querySelectorAll){(function(){var aW=aG,aY=document.createElement("div");aY.innerHTML="<p class='TEST'></p>";if(aY.querySelectorAll&&aY.querySelectorAll(".TEST").length===0){return}aG=function(a2,a1,aZ,a0){a1=a1||document;if(!a0&&a1.nodeType===9&&!aH(a1)){try{return aO(a1.querySelectorAll(a2),aZ)}catch(a3){}}return aW(a2,a1,aZ,a0)};for(var aX in aW){aG[aX]=aW[aX]}aY=null})()}(function(){var aW=document.createElement("div");aW.innerHTML="<div class='test e'></div><div class='test'></div>";if(!aW.getElementsByClassName||aW.getElementsByClassName("e").length===0){return}aW.lastChild.className="e";if(aW.getElementsByClassName("e").length===1){return}aM.order.splice(1,0,"CLASS");aM.find.CLASS=function(aX,aY,aZ){if(typeof aY.getElementsByClassName!=="undefined"&&!aZ){return aY.getElementsByClassName(aX[1])}};aW=null})();function K(aX,a2,a1,a5,a3,a4){for(var aZ=0,aY=a5.length;aZ<aY;aZ++){var aW=a5[aZ];if(aW){aW=aW[aX];var a0=false;while(aW){if(aW.sizcache===a1){a0=a5[aW.sizset];break}if(aW.nodeType===1&&!a4){aW.sizcache=a1;aW.sizset=aZ}if(aW.nodeName.toLowerCase()===a2){a0=aW;break}aW=aW[aX]}a5[aZ]=a0}}}function aU(aX,a2,a1,a5,a3,a4){for(var aZ=0,aY=a5.length;aZ<aY;aZ++){var aW=a5[aZ];if(aW){aW=aW[aX];var a0=false;while(aW){if(aW.sizcache===a1){a0=a5[aW.sizset];break}if(aW.nodeType===1){if(!a4){aW.sizcache=a1;aW.sizset=aZ}if(typeof a2!=="string"){if(aW===a2){a0=true;break}}else{if(aG.filter(a2,[aW]).length>0){a0=aW;break}}}aW=aW[aX]}a5[aZ]=a0}}}var aN=document.compareDocumentPosition?function(aX,aW){return aX.compareDocumentPosition(aW)&16}:function(aX,aW){return aX!==aW&&(aX.contains?aX.contains(aW):true)};var aH=function(aW){var aX=(aW?aW.ownerDocument||aW:0).documentElement;return aX?aX.nodeName!=="HTML":false};var aT=function(aW,a3){var aZ=[],a0="",a1,aY=a3.nodeType?[a3]:a3;while((a1=aM.match.PSEUDO.exec(aW))){a0+=a1[0];aW=aW.replace(aM.match.PSEUDO,"")}aW=aM.relative[aW]?aW+"*":aW;for(var a2=0,aX=aY.length;a2<aX;a2++){aG(aW,aY[a2],aZ)}return aG.filter(a0,aZ)};return aG})();Q.lang={code:"en",of:"of",loading:"loading",cancel:"Cancel",next:"Next",previous:"Previous",play:"Play",pause:"Pause",close:"Close",errors:{single:'You must install the <a href="{0}">{1}</a> browser plugin to view this content.',shared:'You must install both the <a href="{0}">{1}</a> and <a href="{2}">{3}</a> browser plugins to view this content.',either:'You must install either the <a href="{0}">{1}</a> or the <a href="{2}">{3}</a> browser plugin to view this content.'}};var D,at="sb-drag-proxy",E,j,ag;function ax(){E={x:0,y:0,startX:null,startY:null}}function aA(){var K=Q.dimensions;aC(j.style,{height:K.innerHeight+"px",width:K.innerWidth+"px"})}function O(){ax();var K=["position:absolute","cursor:"+(Q.isGecko?"-moz-grab":"move"),"background-color:"+(Q.isIE?"#fff;filter:alpha(opacity=0)":"transparent")].join(";");Q.appendHTML(Q.skin.body,'<div id="'+at+'" style="'+K+'"></div>');j=ad(at);aA();F(j,"mousedown",L)}function B(){if(j){M(j,"mousedown",L);C(j);j=null}ag=null}function L(S){n(S);var K=V(S);E.startX=K[0];E.startY=K[1];ag=ad(Q.player.id);F(document,"mousemove",H);F(document,"mouseup",i);if(Q.isGecko){j.style.cursor="-moz-grabbing"}}function H(aI){var K=Q.player,aJ=Q.dimensions,aH=V(aI);var aG=aH[0]-E.startX;E.startX+=aG;E.x=Math.max(Math.min(0,E.x+aG),aJ.innerWidth-K.width);var S=aH[1]-E.startY;E.startY+=S;E.y=Math.max(Math.min(0,E.y+S),aJ.innerHeight-K.height);aC(ag.style,{left:E.x+"px",top:E.y+"px"})}function i(){M(document,"mousemove",H);M(document,"mouseup",i);if(Q.isGecko){j.style.cursor="-moz-grab"}}Q.img=function(S,aG){this.obj=S;this.id=aG;this.ready=false;var K=this;D=new Image();D.onload=function(){K.height=S.height?parseInt(S.height,10):D.height;K.width=S.width?parseInt(S.width,10):D.width;K.ready=true;D.onload=null;D=null};D.src=S.content};Q.img.ext=["bmp","gif","jpg","jpeg","png"];Q.img.prototype={append:function(S,aI){var aG=document.createElement("img");aG.id=this.id;aG.src=this.obj.content;aG.style.position="absolute";var K,aH;if(aI.oversized&&Q.options.handleOversize=="resize"){K=aI.innerHeight;aH=aI.innerWidth}else{K=this.height;aH=this.width}aG.setAttribute("height",K);aG.setAttribute("width",aH);S.appendChild(aG)},remove:function(){var K=ad(this.id);if(K){C(K)}B();if(D){D.onload=null;D=null}},onLoad:function(){var K=Q.dimensions;if(K.oversized&&Q.options.handleOversize=="drag"){O()}},onWindowResize:function(){var aH=Q.dimensions;switch(Q.options.handleOversize){case"resize":var K=ad(this.id);K.height=aH.innerHeight;K.width=aH.innerWidth;break;case"drag":if(ag){var aG=parseInt(Q.getStyle(ag,"top")),S=parseInt(Q.getStyle(ag,"left"));if(aG+this.height<aH.innerHeight){ag.style.top=aH.innerHeight-this.height+"px"}if(S+this.width<aH.innerWidth){ag.style.left=aH.innerWidth-this.width+"px"}aA()}break}}};Q.iframe=function(S,aG){this.obj=S;this.id=aG;var K=ad("sb-overlay");this.height=S.height?parseInt(S.height,10):K.offsetHeight;this.width=S.width?parseInt(S.width,10):K.offsetWidth};Q.iframe.prototype={append:function(K,aG){var S='<iframe id="'+this.id+'" name="'+this.id+'" height="100%" width="100%" frameborder="0" marginwidth="0" marginheight="0" style="visibility:hidden" onload="this.style.visibility=\'visible\'" scrolling="auto"';if(Q.isIE){S+=' allowtransparency="true"';if(Q.isIE6){S+=" src=\"javascript:false;document.write('');\""}}S+="></iframe>";K.innerHTML=S},remove:function(){var K=ad(this.id);if(K){C(K);if(Q.isGecko){delete au.frames[this.id]}}},onLoad:function(){var K=Q.isIE?ad(this.id).contentWindow:au.frames[this.id];K.location.href=this.obj.content}};Q.html=function(K,S){this.obj=K;this.id=S;this.height=K.height?parseInt(K.height,10):300;this.width=K.width?parseInt(K.width,10):500};Q.html.prototype={append:function(K,S){var aG=document.createElement("div");aG.id=this.id;aG.className="html";aG.innerHTML=this.obj.content;K.appendChild(aG)},remove:function(){var K=ad(this.id);if(K){C(K)}}};var ao=false,Y=[],q=["sb-nav-close","sb-nav-next","sb-nav-play","sb-nav-pause","sb-nav-previous"],aa,ae,Z,m=true;function N(aG,aQ,aN,aL,aR){var K=(aQ=="opacity"),aM=K?Q.setOpacity:function(aS,aT){aS.style[aQ]=""+aT+"px"};if(aL==0||(!K&&!Q.options.animate)||(K&&!Q.options.animateFade)){aM(aG,aN);if(aR){aR()}return}var aO=parseFloat(Q.getStyle(aG,aQ))||0;var aP=aN-aO;if(aP==0){if(aR){aR()}return}aL*=1000;var aH=aw(),aK=Q.ease,aJ=aH+aL,aI;var S=setInterval(function(){aI=aw();if(aI>=aJ){clearInterval(S);S=null;aM(aG,aN);if(aR){aR()}}else{aM(aG,aO+aK((aI-aH)/aL)*aP)}},10)}function aB(){aa.style.height=Q.getWindowSize("Height")+"px";aa.style.width=Q.getWindowSize("Width")+"px"}function aE(){aa.style.top=document.documentElement.scrollTop+"px";aa.style.left=document.documentElement.scrollLeft+"px"}function ay(K){if(K){aF(Y,function(S,aG){aG[0].style.visibility=aG[1]||""})}else{Y=[];aF(Q.options.troubleElements,function(aG,S){aF(document.getElementsByTagName(S),function(aH,aI){Y.push([aI,aI.style.visibility]);aI.style.visibility="hidden"})})}}function r(aG,K){var S=ad("sb-nav-"+aG);if(S){S.style.display=K?"":"none"}}function ah(K,aJ){var aI=ad("sb-loading"),aG=Q.getCurrent().player,aH=(aG=="img"||aG=="html");if(K){Q.setOpacity(aI,0);aI.style.display="block";var S=function(){Q.clearOpacity(aI);if(aJ){aJ()}};if(aH){N(aI,"opacity",1,Q.options.fadeDuration,S)}else{S()}}else{var S=function(){aI.style.display="none";Q.clearOpacity(aI);if(aJ){aJ()}};if(aH){N(aI,"opacity",0,Q.options.fadeDuration,S)}else{S()}}}function t(aO){var aJ=Q.getCurrent();ad("sb-title-inner").innerHTML=aJ.title||"";var aP,aL,S,aQ,aM;if(Q.options.displayNav){aP=true;var aN=Q.gallery.length;if(aN>1){if(Q.options.continuous){aL=aM=true}else{aL=(aN-1)>Q.current;aM=Q.current>0}}if(Q.options.slideshowDelay>0&&Q.hasNext()){aQ=!Q.isPaused();S=!aQ}}else{aP=aL=S=aQ=aM=false}r("close",aP);r("next",aL);r("play",S);r("pause",aQ);r("previous",aM);var K="";if(Q.options.displayCounter&&Q.gallery.length>1){var aN=Q.gallery.length;if(Q.options.counterType=="skip"){var aI=0,aH=aN,aG=parseInt(Q.options.counterLimit)||0;if(aG<aN&&aG>2){var aK=Math.floor(aG/2);aI=Q.current-aK;if(aI<0){aI+=aN}aH=Q.current+(aG-aK);if(aH>aN){aH-=aN}}while(aI!=aH){if(aI==aN){aI=0}K+='<a onclick="Shadowbox.change('+aI+');"';if(aI==Q.current){K+=' class="sb-counter-current"'}K+=">"+(++aI)+"</a>"}}else{K=[Q.current+1,Q.lang.of,aN].join(" ")}}ad("sb-counter").innerHTML=K;aO()}function U(aH){var K=ad("sb-title-inner"),aG=ad("sb-info-inner"),S=0.35;K.style.visibility=aG.style.visibility="";if(K.innerHTML!=""){N(K,"marginTop",0,S)}N(aG,"marginTop",0,S,aH)}function av(aG,aM){var aK=ad("sb-title"),K=ad("sb-info"),aH=aK.offsetHeight,aI=K.offsetHeight,aJ=ad("sb-title-inner"),aL=ad("sb-info-inner"),S=(aG?0.35:0);N(aJ,"marginTop",aH,S);N(aL,"marginTop",aI*-1,S,function(){aJ.style.visibility=aL.style.visibility="hidden";aM()})}function ac(K,aH,S,aJ){var aI=ad("sb-wrapper-inner"),aG=(S?Q.options.resizeDuration:0);N(Z,"top",aH,aG);N(aI,"height",K,aG,aJ)}function ar(K,aH,S,aI){var aG=(S?Q.options.resizeDuration:0);N(Z,"left",aH,aG);N(Z,"width",K,aG,aI)}function ak(aM,aG){var aI=ad("sb-body-inner"),aM=parseInt(aM),aG=parseInt(aG),S=Z.offsetHeight-aI.offsetHeight,K=Z.offsetWidth-aI.offsetWidth,aK=ae.offsetHeight,aL=ae.offsetWidth,aJ=parseInt(Q.options.viewportPadding)||20,aH=(Q.player&&Q.options.handleOversize!="drag");return Q.setDimensions(aM,aG,aK,aL,S,K,aJ,aH)}var T={};T.markup='<div id="sb-container"><div id="sb-overlay"></div><div id="sb-wrapper"><div id="sb-title"><div id="sb-title-inner"></div></div><div id="sb-wrapper-inner"><div id="sb-body"><div id="sb-body-inner"></div><div id="sb-loading"><div id="sb-loading-inner"><span>{loading}</span></div></div></div></div><div id="sb-info"><div id="sb-info-inner"><div id="sb-counter"></div><div id="sb-nav"><a id="sb-nav-close" title="{close}" onclick="Shadowbox.close()"></a><a id="sb-nav-next" title="{next}" onclick="Shadowbox.next()"></a><a id="sb-nav-play" title="{play}" onclick="Shadowbox.play()"></a><a id="sb-nav-pause" title="{pause}" onclick="Shadowbox.pause()"></a><a id="sb-nav-previous" title="{previous}" onclick="Shadowbox.previous()"></a></div></div></div></div></div>';T.options={animSequence:"sync",counterLimit:10,counterType:"default",displayCounter:true,displayNav:true,fadeDuration:0.35,initialHeight:160,initialWidth:320,modal:false,overlayColor:"#000",overlayOpacity:0.5,resizeDuration:0.35,showOverlay:true,troubleElements:["select","object","embed","canvas"]};T.init=function(){Q.appendHTML(document.body,s(T.markup,Q.lang));T.body=ad("sb-body-inner");aa=ad("sb-container");ae=ad("sb-overlay");Z=ad("sb-wrapper");if(!x){aa.style.position="absolute"}if(!h){var aG,K,S=/url\("(.*\.png)"\)/;aF(q,function(aI,aJ){aG=ad(aJ);if(aG){K=Q.getStyle(aG,"backgroundImage").match(S);if(K){aG.style.backgroundImage="none";aG.style.filter="progid:DXImageTransform.Microsoft.AlphaImageLoader(enabled=true,src="+K[1]+",sizingMethod=scale);"}}})}var aH;F(au,"resize",function(){if(aH){clearTimeout(aH);aH=null}if(A){aH=setTimeout(T.onWindowResize,10)}})};T.onOpen=function(K,aG){m=false;aa.style.display="block";aB();var S=ak(Q.options.initialHeight,Q.options.initialWidth);ac(S.innerHeight,S.top);ar(S.width,S.left);if(Q.options.showOverlay){ae.style.backgroundColor=Q.options.overlayColor;Q.setOpacity(ae,0);if(!Q.options.modal){F(ae,"click",Q.close)}ao=true}if(!x){aE();F(au,"scroll",aE)}ay();aa.style.visibility="visible";if(ao){N(ae,"opacity",Q.options.overlayOpacity,Q.options.fadeDuration,aG)}else{aG()}};T.onLoad=function(S,K){ah(true);while(T.body.firstChild){C(T.body.firstChild)}av(S,function(){if(!A){return}if(!S){Z.style.visibility="visible"}t(K)})};T.onReady=function(aH){if(!A){return}var S=Q.player,aG=ak(S.height,S.width);var K=function(){U(aH)};switch(Q.options.animSequence){case"hw":ac(aG.innerHeight,aG.top,true,function(){ar(aG.width,aG.left,true,K)});break;case"wh":ar(aG.width,aG.left,true,function(){ac(aG.innerHeight,aG.top,true,K)});break;default:ar(aG.width,aG.left,true);ac(aG.innerHeight,aG.top,true,K)}};T.onShow=function(K){ah(false,K);m=true};T.onClose=function(){if(!x){M(au,"scroll",aE)}M(ae,"click",Q.close);Z.style.visibility="hidden";var K=function(){aa.style.visibility="hidden";aa.style.display="none";ay(true)};if(ao){N(ae,"opacity",0,Q.options.fadeDuration,K)}else{K()}};T.onPlay=function(){r("play",false);r("pause",true)};T.onPause=function(){r("pause",false);r("play",true)};T.onWindowResize=function(){if(!m){return}aB();var K=Q.player,S=ak(K.height,K.width);ar(S.width,S.left);ac(S.innerHeight,S.top);if(K.onWindowResize){K.onWindowResize()}};Q.skin=T;au.Shadowbox=Q})(window);
/* SWFObject v2.1 <http://code.google.com/p/swfobject/>
 Copyright (c) 2007-2008 Geoff Stearns, Michael Williams, and Bobby van der Sluis
 This software is released under the MIT License <http://www.opensource.org/licenses/mit-license.php>
 */
try {
    var p = swfobject.p;
} catch (e) {
    /*! SWFObject v2.1 <http://code.google.com/p/swfobject/>
     Copyright (c) 2007-2008 Geoff Stearns, Michael Williams, and Bobby van der Sluis
     This software is released under the MIT License <http://www.opensource.org/licenses/mit-license.php>
     */
    var swfobject = function() {

        var UNDEF = "undefined",
                OBJECT = "object",
                SHOCKWAVE_FLASH = "Shockwave Flash",
                SHOCKWAVE_FLASH_AX = "ShockwaveFlash.ShockwaveFlash",
                FLASH_MIME_TYPE = "application/x-shockwave-flash",
                EXPRESS_INSTALL_ID = "SWFObjectExprInst",

                win = window,
                doc = document,
                nav = navigator,

                domLoadFnArr = [],
                regObjArr = [],
                objIdArr = [],
                listenersArr = [],
                script,
                timer = null,
                storedAltContent = null,
                storedAltContentId = null,
                isDomLoaded = false,
                isExpressInstallActive = false;

        /* Centralized function for browser feature detection
         - Proprietary feature detection (conditional compiling) is used to detect Internet Explorer's features
         - User agent string detection is only used when no alternative is possible
         - Is executed directly for optimal performance
         */
        var ua = function() {
            var w3cdom = typeof doc.getElementById != UNDEF && typeof doc.getElementsByTagName != UNDEF && typeof doc.createElement != UNDEF,
                    playerVersion = [0,0,0],
                    d = null;
            if (typeof nav.plugins != UNDEF && typeof nav.plugins[SHOCKWAVE_FLASH] == OBJECT) {
                d = nav.plugins[SHOCKWAVE_FLASH].description;
                if (d && !(typeof nav.mimeTypes != UNDEF && nav.mimeTypes[FLASH_MIME_TYPE] && !nav.mimeTypes[FLASH_MIME_TYPE].enabledPlugin)) { // navigator.mimeTypes["application/x-shockwave-flash"].enabledPlugin indicates whether plug-ins are enabled or disabled in Safari 3+
                    d = d.replace(/^.*\s+(\S+\s+\S+$)/, "$1");
                    playerVersion[0] = parseInt(d.replace(/^(.*)\..*$/, "$1"), 10);
                    playerVersion[1] = parseInt(d.replace(/^.*\.(.*)\s.*$/, "$1"), 10);
                    playerVersion[2] = /r/.test(d) ? parseInt(d.replace(/^.*r(.*)$/, "$1"), 10) : 0;
                }
            }
            else if (typeof win.ActiveXObject != UNDEF) {
                var a = null, fp6Crash = false;
                try {
                    a = new ActiveXObject(SHOCKWAVE_FLASH_AX + ".7");
                }
                catch(e) {
                    try {
                        a = new ActiveXObject(SHOCKWAVE_FLASH_AX + ".6");
                        playerVersion = [6,0,21];
                        a.allowScriptAccess = "sameDomain";  // Introduced in fp6.0.47
                    }
                    catch(e) {
                        if (playerVersion[0] == 6) {
                            fp6Crash = true;
                        }
                    }
                    if (!fp6Crash) {
                        try {
                            a = new ActiveXObject(SHOCKWAVE_FLASH_AX);
                        }
                        catch(e) {}
                        }
                }
                if (!fp6Crash && a) { // a will return null when ActiveX is disabled
                    try {
                        d = a.GetVariable("$version");  // Will crash fp6.0.21/23/29
                        if (d) {
                            d = d.split(" ")[1].split(",");
                            playerVersion = [parseInt(d[0], 10), parseInt(d[1], 10), parseInt(d[2], 10)];
                        }
                    }
                    catch(e) {}
                }
            }
            var u = nav.userAgent.toLowerCase(),
                    p = nav.platform.toLowerCase(),
                    webkit = /webkit/.test(u) ? parseFloat(u.replace(/^.*webkit\/(\d+(\.\d+)?).*$/, "$1")) : false, // returns either the webkit version or false if not webkit
                    ie = false,
                    windows = p ? /win/.test(p) : /win/.test(u),
                    mac = p ? /mac/.test(p) : /mac/.test(u);
            /*@cc_on
             ie = true;
             @if (@_win32)
             windows = true;
             @elif (@_mac)
             mac = true;
             @end
             @*/
            return { w3cdom:w3cdom, pv:playerVersion, webkit:webkit, ie:ie, win:windows, mac:mac };
        }();

        /* Cross-browser onDomLoad
         - Based on Dean Edwards' solution: http://dean.edwards.name/weblog/2006/06/again/
         - Will fire an event as soon as the DOM of a page is loaded (supported by Gecko based browsers - like Firefox -, IE, Opera9+, Safari)
         */
        var onDomLoad = function() {
            if (!ua.w3cdom) {
                return;
            }
            addDomLoadEvent(main);
            //CQ: START
            /*if (ua.ie && ua.win) {
                try {    // Avoid a possible Operation Aborted error
                    doc.write("<scr" + "ipt id=__ie_ondomload defer=true src=//:></scr" + "ipt>"); // String is split into pieces to avoid Norton AV to add code that can cause errors
                    script = getElementById("__ie_ondomload");
                    if (script) {
                        addListener(script, "onreadystatechange", checkReadyState);
                    }
                }
                catch(e) {}
            }*/
            //CQ: END
            if (ua.webkit && typeof doc.readyState != UNDEF) {
                        timer = setInterval(function() { if (/loaded|complete/.test(doc.readyState)) { callDomLoadFunctions(); }}, 10);
                    }
            if (typeof doc.addEventListener != UNDEF) {
                doc.addEventListener("DOMContentLoaded", callDomLoadFunctions, null);
            }
            addLoadEvent(callDomLoadFunctions);
        }();

        function checkReadyState() {
            if (script.readyState == "complete") {
                script.parentNode.removeChild(script);
                callDomLoadFunctions();
            }
        }

        function callDomLoadFunctions() {
            if (isDomLoaded) {
                return;
            }
            //CQ: START
            /*if (ua.ie && ua.win) { // Test if we can really add elements to the DOM; we don't want to fire it too early
                var s = createElement("span");
                try { // Avoid a possible Operation Aborted error
                    var t = doc.getElementsByTagName("body")[0].appendChild(s);
                    t.parentNode.removeChild(t);
                }
                catch (e) {
                    return;
                }
            }*/
            //CQ: END
            isDomLoaded = true;
            if (timer) {
                clearInterval(timer);
                timer = null;
            }
            var dl = domLoadFnArr.length;
            for (var i = 0; i < dl; i++) {
                domLoadFnArr[i]();
            }
        }

        function addDomLoadEvent(fn) {
            if (isDomLoaded) {
                fn();
            }
            else {
                domLoadFnArr[domLoadFnArr.length] = fn; // Array.push() is only available in IE5.5+
            }
        }

        /* Cross-browser onload
         - Based on James Edwards' solution: http://brothercake.com/site/resources/scripts/onload/
         - Will fire an event as soon as a web page including all of its assets are loaded
         */
        function addLoadEvent(fn) {
            if (typeof win.addEventListener != UNDEF) {
                win.addEventListener("load", fn, false);
            }
            else if (typeof doc.addEventListener != UNDEF) {
                doc.addEventListener("load", fn, false);
            }
            else if (typeof win.attachEvent != UNDEF) {
                addListener(win, "onload", fn);
            }
            else if (typeof win.onload == "function") {
                var fnOld = win.onload;
                win.onload = function() {
                    fnOld();
                    fn();
                };
            }
            else {
                win.onload = fn;
            }
        }

        /* Main function
         - Will preferably execute onDomLoad, otherwise onload (as a fallback)
         */
        function main() { // Static publishing only
            var rl = regObjArr.length;
            for (var i = 0; i < rl; i++) { // For each registered object element
                var id = regObjArr[i].id;
                if (ua.pv[0] > 0) {
                    var obj = getElementById(id);
                    if (obj) {
                        regObjArr[i].width = obj.getAttribute("width") ? obj.getAttribute("width") : "0";
                        regObjArr[i].height = obj.getAttribute("height") ? obj.getAttribute("height") : "0";
                        if (hasPlayerVersion(regObjArr[i].swfVersion)) { // Flash plug-in version >= Flash content version: Houston, we have a match!
                            if (ua.webkit && ua.webkit < 312) { // Older webkit engines ignore the object element's nested param elements
                                fixParams(obj);
                            }
                            setVisibility(id, true);
                        }
                        else if (regObjArr[i].expressInstall && !isExpressInstallActive && hasPlayerVersion("6.0.65") && (ua.win || ua.mac)) { // Show the Adobe Express Install dialog if set by the web page author and if supported (fp6.0.65+ on Win/Mac OS only)
                            showExpressInstall(regObjArr[i]);
                        }
                        else { // Flash plug-in and Flash content version mismatch: display alternative content instead of Flash content
                            displayAltContent(obj);
                        }
                    }
                }
                else {  // If no fp is installed, we let the object element do its job (show alternative content)
                    setVisibility(id, true);
                }
            }
        }

        /* Fix nested param elements, which are ignored by older webkit engines
         - This includes Safari up to and including version 1.2.2 on Mac OS 10.3
         - Fall back to the proprietary embed element
         */
        function fixParams(obj) {
            var nestedObj = obj.getElementsByTagName(OBJECT)[0];
            if (nestedObj) {
                var e = createElement("embed"), a = nestedObj.attributes;
                if (a) {
                    var al = a.length;
                    for (var i = 0; i < al; i++) {
                        if (a[i].nodeName == "DATA") {
                            e.setAttribute("src", a[i].nodeValue);
                        }
                        else {
                            e.setAttribute(a[i].nodeName, a[i].nodeValue);
                        }
                    }
                }
                var c = nestedObj.childNodes;
                if (c) {
                    var cl = c.length;
                    for (var j = 0; j < cl; j++) {
                        if (c[j].nodeType == 1 && c[j].nodeName == "PARAM") {
                            e.setAttribute(c[j].getAttribute("name"), c[j].getAttribute("value"));
                        }
                    }
                }
                obj.parentNode.replaceChild(e, obj);
            }
        }

        /* Show the Adobe Express Install dialog
         - Reference: http://www.adobe.com/cfusion/knowledgebase/index.cfm?id=6a253b75
         */
        function showExpressInstall(regObj) {
            isExpressInstallActive = true;
            var obj = getElementById(regObj.id);
            if (obj) {
                if (regObj.altContentId) {
                    var ac = getElementById(regObj.altContentId);
                    if (ac) {
                        storedAltContent = ac;
                        storedAltContentId = regObj.altContentId;
                    }
                }
                else {
                    storedAltContent = abstractAltContent(obj);
                }
                if (!(/%$/.test(regObj.width)) && parseInt(regObj.width, 10) < 310) {
                    regObj.width = "310";
                }
                if (!(/%$/.test(regObj.height)) && parseInt(regObj.height, 10) < 137) {
                    regObj.height = "137";
                }
                doc.title = doc.title.slice(0, 47) + " - Flash Player Installation";
                var pt = ua.ie && ua.win ? "ActiveX" : "PlugIn",
                        dt = doc.title,
                        fv = "MMredirectURL=" + win.location + "&MMplayerType=" + pt + "&MMdoctitle=" + dt,
                        replaceId = regObj.id;
                // For IE when a SWF is loading (AND: not available in cache) wait for the onload event to fire to remove the original object element
                // In IE you cannot properly cancel a loading SWF file without breaking browser load references, also obj.onreadystatechange doesn't work
                if (ua.ie && ua.win && obj.readyState != 4) {
                    var newObj = createElement("div");
                    replaceId += "SWFObjectNew";
                    newObj.setAttribute("id", replaceId);
                    obj.parentNode.insertBefore(newObj, obj); // Insert placeholder div that will be replaced by the object element that loads expressinstall.swf
                    obj.style.display = "none";
                    var fn = function() {
                        obj.parentNode.removeChild(obj);
                    };
                    addListener(win, "onload", fn);
                }
                createSWF({ data:regObj.expressInstall, id:EXPRESS_INSTALL_ID, width:regObj.width, height:regObj.height }, { flashvars:fv }, replaceId);
            }
        }

        /* Functions to abstract and display alternative content
         */
        function displayAltContent(obj) {
            if (ua.ie && ua.win && obj.readyState != 4) {
                // For IE when a SWF is loading (AND: not available in cache) wait for the onload event to fire to remove the original object element
                // In IE you cannot properly cancel a loading SWF file without breaking browser load references, also obj.onreadystatechange doesn't work
                var el = createElement("div");
                obj.parentNode.insertBefore(el, obj); // Insert placeholder div that will be replaced by the alternative content
                el.parentNode.replaceChild(abstractAltContent(obj), el);
                obj.style.display = "none";
                var fn = function() {
                    obj.parentNode.removeChild(obj);
                };
                addListener(win, "onload", fn);
            }
            else {
                obj.parentNode.replaceChild(abstractAltContent(obj), obj);
            }
        }


        function abstractAltContent(obj) {
            var ac = createElement("div");
            if (ua.win && ua.ie) {
                ac.innerHTML = obj.innerHTML;
            }
            else {
                var nestedObj = obj.getElementsByTagName(OBJECT)[0];
                if (nestedObj) {
                    var c = nestedObj.childNodes;
                    if (c) {
                        var cl = c.length;
                        for (var i = 0; i < cl; i++) {
                            if (!(c[i].nodeType == 1 && c[i].nodeName == "PARAM") && !(c[i].nodeType == 8)) {
                                ac.appendChild(c[i].cloneNode(true));
                            }
                        }
                    }
                }
            }
            return ac;
        }

        /* Cross-browser dynamic SWF creation
         */
        function createSWF(attObj, parObj, id) {
            var r, el = getElementById(id);
            if (el) {
                if (typeof attObj.id == UNDEF) { // if no 'id' is defined for the object element, it will inherit the 'id' from the alternative content
                    attObj.id = id;
                }
                if (ua.ie && ua.win) { // IE, the object element and W3C DOM methods do not combine: fall back to outerHTML
                    var att = "";
                    for (var i in attObj) {
                        if (attObj[i] != Object.prototype[i]) { // Filter out prototype additions from other potential libraries, like Object.prototype.toJSONString = function() {}
                            if (i.toLowerCase() == "data") {
                                parObj.movie = attObj[i];
                            }
                            else if (i.toLowerCase() == "styleclass") { // 'class' is an ECMA4 reserved keyword
                                att += ' class="' + attObj[i] + '"';
                            }
                            else if (i.toLowerCase() != "classid") {
                                att += ' ' + i + '="' + attObj[i] + '"';
                            }
                        }
                    }
                    var par = "";
                    for (var j in parObj) {
                        if (parObj[j] != Object.prototype[j]) { // Filter out prototype additions from other potential libraries
                            par += '<param name="' + j + '" value="' + parObj[j] + '" />';
                        }
                    }
                    el.outerHTML = '<object classid="clsid:D27CDB6E-AE6D-11cf-96B8-444553540000"' + att + '>' + par + '</object>';
                    objIdArr[objIdArr.length] = attObj.id; // Stored to fix object 'leaks' on unload (dynamic publishing only)
                    r = getElementById(attObj.id);
                }
                else if (ua.webkit && ua.webkit < 312) { // Older webkit engines ignore the object element's nested param elements: fall back to the proprietary embed element
                    var e = createElement("embed");
                    e.setAttribute("type", FLASH_MIME_TYPE);
                    for (var k in attObj) {
                        if (attObj[k] != Object.prototype[k]) { // Filter out prototype additions from other potential libraries
                            if (k.toLowerCase() == "data") {
                                e.setAttribute("src", attObj[k]);
                            }
                            else if (k.toLowerCase() == "styleclass") { // 'class' is an ECMA4 reserved keyword
                                e.setAttribute("class", attObj[k]);
                            }
                            else if (k.toLowerCase() != "classid") { // Filter out IE specific attribute
                                e.setAttribute(k, attObj[k]);
                            }
                        }
                    }
                    for (var l in parObj) {
                        if (parObj[l] != Object.prototype[l]) { // Filter out prototype additions from other potential libraries
                            if (l.toLowerCase() != "movie") { // Filter out IE specific param element
                                e.setAttribute(l, parObj[l]);
                            }
                        }
                    }
                    el.parentNode.replaceChild(e, el);
                    r = e;
                }
                else { // Well-behaving browsers
                    var o = createElement(OBJECT);
                    o.setAttribute("type", FLASH_MIME_TYPE);
                    for (var m in attObj) {
                        if (attObj[m] != Object.prototype[m]) { // Filter out prototype additions from other potential libraries
                            if (m.toLowerCase() == "styleclass") { // 'class' is an ECMA4 reserved keyword
                                o.setAttribute("class", attObj[m]);
                            }
                            else if (m.toLowerCase() != "classid") { // Filter out IE specific attribute
                                o.setAttribute(m, attObj[m]);
                            }
                        }
                    }
                    for (var n in parObj) {
                        if (parObj[n] != Object.prototype[n] && n.toLowerCase() != "movie") { // Filter out prototype additions from other potential libraries and IE specific param element
                            createObjParam(o, n, parObj[n]);
                        }
                    }
                    el.parentNode.replaceChild(o, el);
                    r = o;
                }
            }
            r.style.outline="none";
            return r;
        }

        function createObjParam(el, pName, pValue) {
            var p = createElement("param");
            p.setAttribute("name", pName);
            p.setAttribute("value", pValue);
            el.appendChild(p);
        }

        /* Cross-browser SWF removal
         - Especially needed to safely and completely remove a SWF in Internet Explorer
         */
        function removeSWF(id) {
            var obj = getElementById(id);
            if (obj && (obj.nodeName == "OBJECT" || obj.nodeName == "EMBED")) {
                if (ua.ie && ua.win) {
                    if (obj.readyState == 4) {
                        removeObjectInIE(id);
                    }
                    else {
                        win.attachEvent("onload", function() {
                            removeObjectInIE(id);
                        });
                    }
                }
                else {
                    obj.parentNode.removeChild(obj);
                }
            }
        }

        function removeObjectInIE(id) {
            var obj = getElementById(id);
            if (obj) {
                for (var i in obj) {
                    if (typeof obj[i] == "function") {
                        obj[i] = null;
                    }
                }
                obj.parentNode.removeChild(obj);
            }
        }

        /* Functions to optimize JavaScript compression
         */
        function getElementById(id) {
            var el = null;
            try {
                el = doc.getElementById(id);
            }
            catch (e) {
            }
            return el;
        }

        function createElement(el) {
            return doc.createElement(el);
        }

        /* Updated attachEvent function for Internet Explorer
         - Stores attachEvent information in an Array, so on unload the detachEvent functions can be called to avoid memory leaks
         */
        function addListener(target, eventType, fn) {
            target.attachEvent(eventType, fn);
            listenersArr[listenersArr.length] = [target, eventType, fn];
        }

        /* Flash Player and SWF content version matching
         */
        function hasPlayerVersion(rv) {
            var pv = ua.pv, v = rv.split(".");
            v[0] = parseInt(v[0], 10);
            v[1] = parseInt(v[1], 10) || 0; // supports short notation, e.g. "9" instead of "9.0.0"
            v[2] = parseInt(v[2], 10) || 0;
            return (pv[0] > v[0] || (pv[0] == v[0] && pv[1] > v[1]) || (pv[0] == v[0] && pv[1] == v[1] && pv[2] >= v[2])) ? true : false;
        }

        /* Cross-browser dynamic CSS creation
         - Based on Bobby van der Sluis' solution: http://www.bobbyvandersluis.com/articles/dynamicCSS.php
         */
        function createCSS(sel, decl) {
            if (ua.ie && ua.mac) {
                return;
            }
            var h = doc.getElementsByTagName("head")[0], s = createElement("style");
            s.setAttribute("type", "text/css");
            s.setAttribute("media", "screen");
            if (!(ua.ie && ua.win) && typeof doc.createTextNode != UNDEF) {
                s.appendChild(doc.createTextNode(sel + " {" + decl + "}"));
            }
            h.appendChild(s);
            if (ua.ie && ua.win && typeof doc.styleSheets != UNDEF && doc.styleSheets.length > 0) {
                var ls = doc.styleSheets[doc.styleSheets.length - 1];
                if (typeof ls.addRule == OBJECT) {
                    ls.addRule(sel, decl);
                }
            }
        }

        function setVisibility(id, isVisible) {
            var v = isVisible ? "visible" : "hidden";
            if (isDomLoaded && getElementById(id)) {
                getElementById(id).style.visibility = v;
            }
            else {
                createCSS("#" + id, "visibility:" + v);
            }
        }


        /* Filter to avoid XSS attacks
         */
        function urlEncodeIfNecessary(s) {
            var regex = /[\\\"<>\.;]/;
            var hasBadChars = regex.exec(s) != null;
            return hasBadChars ? encodeURIComponent(s) : s;
        }

        /* Release memory to avoid memory leaks caused by closures, fix hanging audio/video threads and force open sockets/NetConnections to disconnect (Internet Explorer only)
         */
        var cleanup = function() {
            if (ua.ie && ua.win) {
                var fn = function() {
                    // remove listeners to avoid memory leaks
                    var ll = listenersArr.length;
                    for (var i = 0; i < ll; i++) {
                        listenersArr[i][0].detachEvent(listenersArr[i][1], listenersArr[i][2]);
                    }
                    // cleanup dynamically embedded objects to fix audio/video threads and force open sockets and NetConnections to disconnect
                    var il = objIdArr.length;
                    for (var j = 0; j < il; j++) {
                        removeSWF(objIdArr[j]);
                    }
                    // cleanup library's main closures to avoid memory leaks
                    for (var k in ua) {
                        ua[k] = null;
                    }
                    ua = null;
                    for (var l in swfobject) {
                        swfobject[l] = null;
                    }
                    swfobject = null;
                    // detach ourself
                    window.detachEvent("onunload", fn);
                };
                window.attachEvent("onunload", fn);
            }
        }();


        return {
            /* Public API
             - Reference: http://code.google.com/p/swfobject/wiki/SWFObject_2_0_documentation
             */
            registerObject: function(objectIdStr, swfVersionStr, xiSwfUrlStr) {
                if (!ua.w3cdom || !objectIdStr || !swfVersionStr) {
                    return;
                }
                var regObj = {};
                regObj.id = objectIdStr;
                regObj.swfVersion = swfVersionStr;
                regObj.expressInstall = xiSwfUrlStr ? xiSwfUrlStr : false;
                regObjArr[regObjArr.length] = regObj;
                setVisibility(objectIdStr, false);
            },

            getObjectById: function(objectIdStr) {
                var r = null;
                if (ua.w3cdom) {
                    var o = getElementById(objectIdStr);
                    if (o) {
                        var n = o.getElementsByTagName(OBJECT)[0];
                        if (!n || (n && typeof o.SetVariable != UNDEF)) {
                            r = o;
                        }
                        else if (typeof n.SetVariable != UNDEF) {
                            r = n;
                        }
                    }
                }
                return r;
            },

            embedSWF: function(swfUrlStr, replaceElemIdStr, widthStr, heightStr, swfVersionStr, xiSwfUrlStr, flashvarsObj, parObj, attObj) {
                //CQ: START
                if (document.readyState === undefined || document.readyState == "complete") {
                    callDomLoadFunctions();
                }
                //CQ: END
                if (!ua.w3cdom || !swfUrlStr || !replaceElemIdStr || !widthStr || !heightStr || !swfVersionStr) {
                    return;
                }
                widthStr += ""; // Auto-convert to string
                heightStr += "";
                if (hasPlayerVersion(swfVersionStr)) {
                    setVisibility(replaceElemIdStr, false);
                    var att = {};
                    if (attObj && typeof attObj === OBJECT) {
                        for (var i in attObj) {
                            if (attObj[i] != Object.prototype[i]) { // Filter out prototype additions from other potential libraries
                                att[i] = attObj[i];
                            }
                        }
                    }
                    att.data = swfUrlStr;
                    att.width = widthStr;
                    att.height = heightStr;
                    var par = {};
                    if (parObj && typeof parObj === OBJECT) {
                        for (var j in parObj) {
                            if (parObj[j] != Object.prototype[j]) { // Filter out prototype additions from other potential libraries
                                par[j] = parObj[j];
                            }
                        }
                    }
                    if (flashvarsObj && typeof flashvarsObj === OBJECT) {
                        for (var k in flashvarsObj) {
                            if (flashvarsObj[k] != Object.prototype[k]) { // Filter out prototype additions from other potential libraries
                                if (typeof par.flashvars != UNDEF) {
                                    par.flashvars += "&" + k + "=" + flashvarsObj[k];
                                }
                                else {
                                    par.flashvars = k + "=" + flashvarsObj[k];
                                }
                            }
                        }
                    }
                    addDomLoadEvent(function() {
                        createSWF(att, par, replaceElemIdStr);
                        if (att.id == replaceElemIdStr) {
                            setVisibility(replaceElemIdStr, true);
                        }
                    });
                }
                else if (xiSwfUrlStr && !isExpressInstallActive && hasPlayerVersion("6.0.65") && (ua.win || ua.mac)) {
                    isExpressInstallActive = true; // deferred execution
                    setVisibility(replaceElemIdStr, false);
                    addDomLoadEvent(function() {
                        var regObj = {};
                        regObj.id = regObj.altContentId = replaceElemIdStr;
                        regObj.width = widthStr;
                        regObj.height = heightStr;
                        regObj.expressInstall = xiSwfUrlStr;
                        showExpressInstall(regObj);
                    });
                }
            },

            getFlashPlayerVersion: function() {
                return { major:ua.pv[0], minor:ua.pv[1], release:ua.pv[2] };
            },

            hasFlashPlayerVersion: hasPlayerVersion,

            createSWF: function(attObj, parObj, replaceElemIdStr) {
                if (ua.w3cdom) {
                    return createSWF(attObj, parObj, replaceElemIdStr);
                }
                else {
                    return undefined;
                }
            },

            removeSWF: function(objElemIdStr) {
                if (ua.w3cdom) {
                    removeSWF(objElemIdStr);
                }
            },

            createCSS: function(sel, decl) {
                if (ua.w3cdom) {
                    createCSS(sel, decl);
                }
            },

            addDomLoadEvent: addDomLoadEvent,

            addLoadEvent: addLoadEvent,

            getQueryParamValue: function(param) {
                var q = doc.location.search || doc.location.hash;
                if (param == null) {
                    return urlEncodeIfNecessary(q);
                }
                if (q) {
                    var pairs = q.substring(1).split("&");
                    for (var i = 0; i < pairs.length; i++) {
                        if (pairs[i].substring(0, pairs[i].indexOf("=")) == param) {
                            return urlEncodeIfNecessary(pairs[i].substring((pairs[i].indexOf("=") + 1)));
                        }
                    }
                }
                return "";
            },

            // For internal usage only
            expressInstallCallback: function() {
                if (isExpressInstallActive && storedAltContent) {
                    var obj = getElementById(EXPRESS_INSTALL_ID);
                    if (obj) {
                        obj.parentNode.replaceChild(storedAltContent, obj);
                        if (storedAltContentId) {
                            setVisibility(storedAltContentId, true);
                            if (ua.ie && ua.win) {
                                storedAltContent.style.display = "block";
                            }
                        }
                        storedAltContent = null;
                        storedAltContentId = null;
                        isExpressInstallActive = false;
                    }
                }
            }
        };
    }();
}
$(document).ready(function() { 
        var hrefs = $("a");
        var curHost=  "http://"+ window.location.host;
        var govDomainList = new Array('http://www.ask.census.gov','https://www.census.gov','http://census.gov', 'https://www.census.gov', 'https://census.gov','http://www.commerce.gov','http://commerce.gov','https://census.csod.com/ats/careersite/search.aspx?site=1&c=census','https://census.csod.com/ats/careersite/LoginLite.aspx?c=census&site=1',curHost);
            

          $('#confirm1').hide();
       // This Loop used to initialize external link handlers for every <a> on the page.
        // this refers to the current <a> element
          var filename = window.location.pathname.split('/').pop();
          if(filename.indexOf("siteheader")== -1) {
        $('a').each(function(){
        var lnk=$(this).attr('href');
        if(typeof lnk === "undefined" || !lnk.match("^http")||lnk.indexOf(curHost)!=-1){
        }
        else{
        var index = externalink($(this).attr('href'), govDomainList);       
        if(!index) {
            $(this).attr('target', '_blank'); 
            $(this).on('click', function(event){
                interstit($(this).attr('href'), event);
           });
          }
         }
        });
          }
   });
   
// Popup Disclaimer for external links
function interstit( page, event ) {
    var ret;
    $msg=$.trim($('#popuptext').text()) ;
    ret=confirm($msg);
    if ( ret ) {
        // window.open(page)
     } else {
        event.preventDefault();
     }
}

function externalink(domain, domList) {

  var domainRexp = new RegExp('.*\\.gov');
  var foundPresent = domList.indexOf(domain);//includes(domain);
  var found = false;

  for(var i = 0; i < domList.length; i++)   {
         if(domainRexp.test(domain) ) {
          found = true;
          break;
        }
		if(foundPresent) {
          found = true;
          break;
        }       
  }
    return found;
}
$(document).ready(function () {   
     
    //$(".lefttnav > li").children("ul").css("display", "none"); //hide all dropdowns
    //var top = $(".lefttnav > li").position().top + $(".lefttnav > li").height(); //find the top position for the dropdown menu by adding the top position of the navigation links to their height (giving the bottom position for the navigation links)       
    var counter = 0;
    //when the user hovers of the navigation link..
    var navItems=null    
navItems=$(".grid_navInnerLandingLinks >ul").children();
    if(navItems.length==0){
        navItems=$(".leftnav >ul").children();
    }
   
    if(navItems.length>0){
    //$(navItems).children("span").css("margin-left","5px");
    $(navItems).each(function (index) {
        if($(this).children("ul").length>0){          
        if(!$(this).children("ul").hasClass("expanded")){
        $(this).hover(delayNavDisplay,hideNavSub);
        }else{                              
        var children= $(this).children("ul").children();
        if(children){              
            $(children).each(function (index) {            
                           if($(this).children("ul").hasClass("expanded")){                           
                           $(this).children("ul").children("li").css('margin-left', '0px');                            
                           }
                           else{
                $(this).hover(delayNavDisplay2,hideNavSub);
                }                
            });
        }
        }
    }
    });
    }

    var $mobileNav = $('#data-uscb-input-wrapper-mobile-nav');

    $mobileNav.click(function(e) {
        var $dropdown = $mobileNav.find('ul');
        if ( $dropdown[0] ) {
        	if ( $dropdown[0].style.display === 'none' ) {
            	$dropdown[0].style.display = 'block';
        	} else {
            	$dropdown[0].style.display = 'none';
        	}
        }
    });

    
    $(window).click(function(e){			
        if ( $(e.target).closest( '#data-uscb-input-wrapper-mobile-nav' )[0] !== undefined ) {
            //do nothing...
        } else {
            var $dropdown = $mobileNav.find('ul');
            if ( $dropdown[0] ) {
                if ( $dropdown[0].style.display === 'block' ) {
                    $dropdown[0].style.display = 'none';
                }
            }
        }
    });
});

var delayNavDisplay = function () {
    //clearInterval(delay);
     var left = $(this).position().left; //get the postion of the main link relative to the <body>
    var right = $(this).position().right; // get the right margin of the parent tab
    var offset = $(this).offset().left; //get the position of the main link relative to the document
    var width = $(this).width(); //get the width of the dropdown    
    
    var listItem = this;
    //$(this).css('background-color', '#0d6a99'); 
    $(this).css('background-color', '#405773');
    $(this).children("span").children("a").css('color', '#FFFFFF');
    $(this).children("span").children("strong").css('color', '#FFFFFF');
    $(this).children("span").children("a").css('text-decoration', 'underline');
    
    $(this).children("ul").css("display", "block");
    $(this).children("ul").css("position", "absolute");   
    $(this).children("ul").css("box-shadow", "0px 0px 0px #818493, 3px 3px 3px #818493, 0px 0px 0px #818493");    
    $(this).children("ul").children("li").children("a").css('color', '#041C5D');
    
    $(this).children("ul").css({
        left: width,
        top: $(this).position().top
     });
};

var delayNavDisplay2 = function () {
    
    var left = $(this).position().left; //get the postion of the main link relative to the <body>
    var right = $(this).position().right; // get the right margin of the parent tab
    var offset = $(this).offset().left; //get the position of the main link relative to the document
    var width = $(this).width(); //get the width of the dropdown
    
    var listItem = this;
    $(this).css('background-color', '#66cccc');
    $(this).children("span").children("a").css('text-decoration', 'underline')
    $(this).css('color', 'black');
    $(this).children("ul").css("display", "block");
    $(this).children("ul").css("position", "absolute");    
    $(this).children("ul").css("box-shadow", "0px 0px 0px #818493, 3px 3px 3px #818493, 0px 0px 0px #818493");
    
    $(this).children("ul").css({
        left: width-2,
        top: $(this).position().top
     });
};

var hideNavSub = (function () { //if the user moves the cursor outside of the dropdown menu...
    //clearInterval(delay);
    $(this).css('background-color', '');
    $(this).children("span").children("a").css('color', '#112E51');//rgb(51,51,51)
    $(this).children("span").children("strong").css('color', '#112E51');//rgb(51,51,51)
    $(this).css('color', '#112E51');//rgb(51,51,51)
    $($($(this).children().get(0)).children().get(0)).css("text-decoration","none");
    $(this).children("ul").hide(); //hide the menu
    
});
function buildListItem(fData, sChar, colSize) {
	var colWidth = 100 / colSize;
	var listResult = "<table width='100%'>";
		listResult += "<tr>";
	    	listResult += "<th colspan='" + colSize + "' class='category-label'>";
	    		listResult += sChar.toUpperCase();
	    	listResult += "</th>";
	    listResult += "</tr>";
	    var columnCtr = 0;
	    for (var i = 0; i < fData.length; i++) {
	    	if (columnCtr == 0) {
                listResult += "<tr>";
                columnCtr = 1;
            }

            if (fData[i].country_selector_type === "aemattachment") {
				listResult += "<td class='uscb-padding-T-20 uscb-padding-B-20' width='" + colWidth + "%'>";
                    listResult += "<a href=" + fData[i].url + " filetrack=" + fData[i].fileTrackKey + " target='_blank' tabindex=1" + 
                    " onclick='buttonClick(this, \"Geographic Country List Button\", \"Body\");' class='uscb-capitalize uscb-color-accent uscb-overflow-ellipsis'>";
                    listResult += "<img src=" + fData[i].icon_path + " class='uscb-country-list-icon' />";
                    listResult += fData[i].geographic_area_name.toLowerCase();
                    listResult += "&nbsp;&nbsp;<span class='fileSize'>[" + fData[i].file_size + "]</span></a>";                                
                listResult += "</td>";
			} else if (fData[i].country_selector_type === "nonaemattachment") {				
				listResult += "<td class='uscb-padding-T-20 uscb-padding-B-20' width='" + colWidth + "%'>";
                    listResult += "<a href=" + fData[i].url + " target='_blank' tabindex=1" + 
                    " onclick='buttonClick(this, \"Geographic Country List Button\", \"Body\");' class='uscb-capitalize uscb-color-accent uscb-overflow-ellipsis'>";
                    listResult += "<img src=" + fData[i].icon_path + " class='uscb-country-list-icon' />";
                    listResult += fData[i].geographic_area_name.toLowerCase();
                    listResult += "&nbsp;&nbsp;<span class='fileSize'>[" + fData[i].file_size + "]</span></a>";                                
                listResult += "</td>";
			} else if (fData[i].country_selector_type === "aemwebpage" || fData[i].country_selector_type === "nonaemwebpage") {
				listResult += "<td class='uscb-padding-T-20 uscb-padding-B-20' width='" + colWidth + "%'>";
                    listResult += "<a href=" + fData[i].url + " target='" + (fData[i].open_new_page ? "_blank" : "") + "' tabindex=1" + 
                    " onclick='buttonClick(this, \"Geographic Country List Button\", \"Body\");' class='uscb-capitalize uscb-color-accent uscb-overflow-ellipsis'>";                                
                    listResult += fData[i].geographic_area_name.toLowerCase() + "</a>";                                
                listResult += "</td>";
			}

			if (columnCtr == colSize) {                
                listResult += "</tr>";
                columnCtr = 0;
            }

            columnCtr++;

	    }//end for...loop
	    listResult += "<tr><td colspan='3'><a href='#' class='data-uscb-back-top-link uscb-color-accent'>Back to Top</a><hr/></td></tr>";
	listResult += "</table>";	
	return listResult;
}

$(document).ready(function() {

	$('.data-uscb-alpha-link-lg').click(function(e) {		
		var selectedChar = $(this).attr('id');
		var filteredData = [];
		if (selectedChar.toUpperCase() != 'ALL') {												
			for (var i = 0; i < data.length; i++) {				  
			  if (data[i].geographic_area_name.toLowerCase().startsWith(selectedChar.toLowerCase())) {				  	
			  	filteredData.push(data[i]);					  	
			  }
			}//end for...loop				
			$('#data-uscb-geographic-area-detail-list').html(buildListItem(filteredData, selectedChar, 3));
			e.preventDefault();
		    var target = '#data-uscb-geographic-area-detail-list';
		    $('html, body').animate({
		        scrollTop: $(target).offset().top
		    }, 2000);				
		} else {			
			for (var i = 0; i < data.length; i++) {
				filteredData.push(data[i]);
			}
			$('#data-uscb-geographic-area-detail-list').html(buildListItem(filteredData, selectedChar, 3));
			e.preventDefault();
		    var target = '#data-uscb-geographic-area-detail-list';
		    $('html, body').animate({
		        scrollTop: $(target).offset().top
		    }, 2000);
		}
	});
	
	$('.data-uscb-alpha-link-sm').click(function(e) {
		var selectedChar = $(this).attr('id');	
		var filteredData = [];
		if (selectedChar.toUpperCase() != 'ALL') {							
			for (var i = 0; i < data.length; i++) {				  
			  if (data[i].geographic_area_name.toLowerCase().startsWith(selectedChar.toLowerCase())) {				  	
			  	filteredData.push(data[i]);					  	
			  }
			}//end for...loop				
			$('#data-uscb-geographic-area-detail-list').html(buildListItem(filteredData, selectedChar, 1));
			e.preventDefault();
		    var target = '#data-uscb-geographic-area-detail-list';
		    $('html, body').animate({
		        scrollTop: $(target).offset().top
		    }, 2000);				
		} else {
			for (var i = 0; i < data.length; i++) {
				filteredData.push(data[i]);
			}
			$('#data-uscb-geographic-area-detail-list').html(buildListItem(filteredData, selectedChar, 1));
			e.preventDefault();
		    var target = '#data-uscb-geographic-area-detail-list';
		    $('html, body').animate({
		        scrollTop: $(target).offset().top
		    }, 2000);
		}
	});	
	
	$(document).on("click", "a.data-uscb-back-top-link", function(e) {		
		$('html, body').animate({
            'scrollTop' : $('.data-uscb-country-selector-dropdown-click').offset().top
        },2000);
	});

});//end document.ready function
$(document).ready(function() {



});
//end of doc ready


function dropDownToggle(event, id) {
    let selectedElement;
    let ulElement = $('#'+id);
    console.log("id is " + id);
    let ulDisplayedElement = $('.uscb-lang-dropdown-button');

    let target =  $('#'+id);
    if (target.is("li")) {
        selectedElement = target[0];
        $(target[0]).siblings().removeClass("uscb-lang-dropdown-selected");
        $(selectedElement).addClass("uscb-lang-dropdown-selected");
        ulDisplayedElement.html(selectedElement.innerHTML);
    } else {
        ulElement.toggleClass('uscb-hide-dropdown');
    }
}
var $ = jQuery.noConflict();

function toggleStatus(activeObj, disabledObj){
	activeObj.removeClass('isDisabled');
	activeObj.addClass('isActive');
	activeObj.attr('tabindex', 0);

	disabledObj.removeClass('isActive');
	disabledObj.addClass('isDisabled');
	disabledObj.attr('tabindex', -1);
}

function showMoreCardsHandler(event) {
	var $showMoreCards = $(event.currentTarget);
	var $showLessCards = $showMoreCards.closest('.uscb-sub-heading-2').find('.data-uscb-show-less-cards');

	$showMoreCards.closest('.data-uscb-list-articles-container').find('.card-over-limit').show();
	$showMoreCards.attr('aria-expanded', true);
	$showLessCards.attr('aria-expanded', true);

	toggleStatus($showLessCards, $showMoreCards);
}

function showLessCardsHandler(event) {
	var $showLessCards = $(event.currentTarget);
	var $showMoreCards = $showLessCards.closest('.uscb-sub-heading-2').find('.data-uscb-show-more-cards');

	$showLessCards.closest('.data-uscb-list-articles-container').find('.card-over-limit').hide();
	$showLessCards.attr('aria-expanded', false);
	$showMoreCards.attr('aria-expanded', false);

	toggleStatus($showMoreCards, $showLessCards);
}

function showMoreCardsKeyDownHandler(event) {
	if (event.keyCode === 13 || event.keyCode === 32) {
		event.preventDefault();
		showMoreCardsHandler(event);
	}
}

function showLessCardsKeyDownHandler(event) {
	if (event.keyCode === 13 || event.keyCode === 32) {
		event.preventDefault();
		showLessCardsHandler(event);
	}
}

$(document).ready(function(){
	//hide those cards with class .card-over-limit
	$('.card-over-limit').hide();

	var $showLessCards = $('.data-uscb-show-less-cards');
	$showLessCards.addClass('isDisabled');
	$showLessCards.attr('tabindex', -1);

	var $body = $("body");

	$body.on("click", ".data-uscb-show-more-cards", showMoreCardsHandler);
	$body.on("click", ".data-uscb-show-less-cards", showLessCardsHandler);
	$body.on("keydown", ".data-uscb-show-more-cards", showMoreCardsKeyDownHandler);
	$body.on("keydown", ".data-uscb-show-less-cards", showLessCardsKeyDownHandler);
});
function customFilterGeographicArea(array, terms) {
	arrayOfTerms = terms.split(" ");
	var term = $.map(arrayOfTerms, function(tm) {
		return $.ui.autocomplete.escapeRegex(tm);
	}).join('|');
	var matcher = new RegExp("\\b" + term, "i");

	return $.grep(array, function(value) {
		return matcher.test(value.geographic_area_name);
	});
};

function onClearGeoSelectionKeyDown (event, wrapperDivId) {
	if (event.keyCode === 32) {
		event.preventDefault();
		clearSelectionGeographicArea(wrapperDivId);
	}
}

function clearSelectionGeographicArea(wrapperDivId) {
	var $countrySelectorInput = $(wrapperDivId).find('input');
	$countrySelectorInput.val('');
	$(wrapperDivId).find('.uscb-input-box-icon').remove();
	$(wrapperDivId).find('.uscb-country-item-close').remove();
	$(wrapperDivId).find('.uscb-geographic-area-level').remove();
	$(wrapperDivId).next().html('');
}

var CensusCountrySelector = (function($, _, digitalData) {
	var countrySelectors = [];
	var initCountrySelector = function(id, dataSorted, isOpen) {
		var countrySelectorObj = _.find(countrySelectors, {
			"id" : "" + id + ""
		});
		
		if (!countrySelectorObj) {
			countrySelectorObj = {
				id : id,
				data : dataSorted
			}
			countrySelectors.push(countrySelectorObj);
		}		
		
		var countrySelectorWrapperDivId = '#data-uscb-input-wrapper-' + id;		
		var $countrySelector = $(countrySelectorWrapperDivId);		
		var $countrySelectorInput = $countrySelector.find('input');
		
		if ($countrySelectorInput.length > 0) {
			$countrySelectorInput
					.autocomplete({
						source : dataSorted,
						multiple : true,
						minLength : 0,
						appendTo : countrySelectorWrapperDivId,
						mustMatch : false,
						source : function(request, response) {
							response(customFilterGeographicArea(dataSorted,
									request.term));
						},
						open : function(event, ui) {
							isOpen = true;
							$(event.target).parent().attr("aria-expanded", "true");
							var inputBoxWidth = $countrySelector.width();                
			                $('.ui-autocomplete').css('width', inputBoxWidth + 'px');
						},
						close : function(event, ui) {
							$(event.target).parent().attr("aria-expanded", "false");
						},
						select : function(event, ui) {
							isOpen = false;
							clearSelectionGeographicArea(countrySelectorWrapperDivId);							
							if (ui.item.show_item == 'uscb-no-show-attachment') {
								$('.data-uscb-download-btn-wrapper').html('');
								if (ui.item.open_new_page) {
									window.open(ui.item.url, '_blank');
									$countrySelectorInput
											.val(ui.item.geographic_area_name)
											.after(
													"<img role='button' aria-label='clear geography selection' tabindex='0' class='uscb-country-item-close' src='/etc.clientlibs/census/clientlibs/census-pattern-library/resources/images/close-3-teal.svg' onkeydown=\"onClearGeoSelectionKeyDown(event, \'" + countrySelectorWrapperDivId + "\')\" onClick='clearSelectionGeographicArea(\'" + countrySelectorWrapperDivId + "\'); />")
											.before(
													"<img class='uscb-input-box-icon "
															+ ui.item.icon_status
															+ "' src='"
															+ ui.item.icon_path
															+ "' />").blur();

									event.preventDefault();
								} else {
									location.href = ui.item.url;
								}
							} else if (ui.item.show_item === 'uscb-show-attachment') {
								$countrySelectorInput.val('');								
								$('.uscb-input-wrapper-' + id).find('.uscb-country-item-close').remove();							
								$countrySelectorInput
										.val(
												ui.item.geographic_area_name
														+ " ["
														+ ui.item.file_size
														+ "]")
										.after("<span class='uscb-geographic-area-level'>" + ui.item.geographic_area_level + "</span>&nbsp;<img role='button' aria-label='clear geography selection' tabindex='0' class='uscb-country-item-close' src='/etc.clientlibs/census/clientlibs/census-pattern-library/resources/images/close-3-teal.svg' onkeydown=\"onClearGeoSelectionKeyDown(event, \'" + countrySelectorWrapperDivId + "\')\" onClick=\"clearSelectionGeographicArea('" + countrySelectorWrapperDivId + "');\"/>")
										.before("<img class='uscb-input-box-icon "
														+ ui.item.icon_status
														+ "' src='"
														+ ui.item.icon_path
														+ "' />");

								$('.data-uscb-download-btn-wrapper').html("<a class='uscb-primary-button uscb-download-button' target='_blank' href='"
														+ ui.item.url
														+ "' filetrack='"
														+ ui.item.fileTrackKey
														+ "'>Download this file</a>");								
								$countrySelectorInput.autocomplete("search", "");
								event.preventDefault();
							} else {
								console.log('Other outside option...');
							}
							return false;
						},
						focus : function(event, ui) {
							$countrySelectorInput.val( ui.item.geographic_area_name );
							return false;
						},
						create : function() {
							$(this).data('ui-autocomplete')._renderItem = function(
									ul, item) {
								return $('<li class="geographyMenuItem">')
										.append(
												'<a href="'
														+ item.url
														+ '" class="uscb-stateitem '
														+ item.left_padding
														+ '  uscb-capitalize" title="'
														+ item.tool_tip
														+ '" filetrack="'
														+ item.fileTrackKey
														+ '" onclick="countrySelectorAnalytics(this);">'
	                                                    + '<span class="uscb-layout-align-start-center">'
														+ '<img class="'
														+ item.icon_status
														+ ' uscb-country-selector-icon" src="'
														+ item.icon_path
														+ '"/>'
														+ item.geographic_area_name
																.toLowerCase()
														+ '</span>'
														+ '<span>'
														+ '<span class="'
														+ (item.country_selector_type != "aemwebpage" ? "uscb-geo-level"
														: "uscb-aem-web-page-geo-level")
														+ '">'
														+ item.geographic_area_level
														+ '</span>'
														+ ' <span class="uscb-file-size '
														+ item.show_attachment_size
														+ '">&nbsp;['
														+ item.file_size
														+ ']</span>'
														+ '</span>'
														+ '</a>')
										.appendTo(ul);
							};
						}
					});
		}// end if		

		$countrySelector.click(function(e) {			
			if (!e.target.className.startsWith('uscb-country-item-close')) {
				if (isOpen) {
					$countrySelectorInput.autocomplete("close");
					isOpen = false;					
				} else {
					$countrySelectorInput.autocomplete("search", "");
					isOpen = true;
				}
			}
		});
		
		$(window).click(function(e) {			
			if ( e.target.className.startsWith('data-uscb-country-selector-dropdown-click') ) {
				//do nothing...
			} else {
				if (isOpen) {					
					$countrySelectorInput.autocomplete("close");
					isOpen = false;	
					e.stopPropagation();
				}
			}
		});
		
	}// end init function

	return {
		initCountrySelector : initCountrySelector
	}

})(jQuery, window._, digitalData);

function customFilterStateList(array, terms) {
	arrayOfTerms = terms.split(" ");
	var term = $.map(arrayOfTerms, function(tm) {
		return $.ui.autocomplete.escapeRegex(tm);
	}).join('|');
	var matcher = new RegExp("\\b" + term, "i");

	return $.grep(array, function(value) {
		return matcher.test(value.value);
	});
};

function onClearStateSelectionKeyDown (event, wrapperDivId) {
	if (event.keyCode === 32) {
		event.preventDefault();
		clearSelectionStateList(wrapperDivId);
	}
}

function clearSelectionStateList(wrapperDivId){
	var $stateListInput = $(wrapperDivId).find('input');
	$stateListInput.val('');
	$(wrapperDivId).find('.uscb-input-box-icon').remove();
	$(wrapperDivId).find('.uscb-state-item-close').remove();
	$(wrapperDivId).next().html('');
}

var CensusStateList = (function($, _, digitalData) {
	var stateLists = [];
	var initStateList = function(id, data, isOpen) {
	    
	    var dataSorted = sortJSON(data, 'label', 'ASC');
		
		var stateListObj = _.find(stateLists, {
			"id" : "" + id + ""
		});
		
		if (!stateListObj) {
			stateListObj = {
				id : id,
				data : dataSorted
			}
			stateLists.push(stateListObj);
		}	
		
		var stateListWrapperDivId = '#data-uscb-input-wrapper-' + id;		
		var $stateList = $(stateListWrapperDivId);		
		var $stateListInput = $stateList.find('input');
		
		if ($stateListInput.length > 0) {
			//To force clear the pre-selected items when browser back button is clicked.
			clearSelectionStateList(stateListWrapperDivId);
			$stateListInput
					.autocomplete({
						source : dataSorted,
						multiple : true,
						minLength : 0,
						appendTo : stateListWrapperDivId,
						mustMatch : false,
						source : function(request, response) {
							response(customFilterStateList(dataSorted,
									request.term));
						},
						open : function(event, ui) {
							isOpen = true;
							$(event.target).parent().attr("aria-expanded", "true");
							var inputBoxWidth = $stateList.width();                
			                $('.ui-autocomplete').css('width', inputBoxWidth + 'px');
						},
						close : function(event, ui) {
							$(event.target).parent().attr("aria-expanded", "false");
						},
						select : function(event, ui) {
							isOpen = false;							
							clearSelectionStateList(stateListWrapperDivId);							
							if (ui.item.show_item == 'uscb-no-show-attachment') {
								$('.data-uscb-download-btn-wrapper').html('');
								if (ui.item.open_new_page) {
									window.open(ui.item.url, '_blank');
									$stateListInput
											.val(ui.item.value)
											.after(
													"<img role='button' aria-label='clear state selection' tabindex='0' class='uscb-state-item-close' src='/etc.clientlibs/census/clientlibs/census-pattern-library/resources/images/close-3-teal.svg' onkeydown=\"onClearStateSelectionKeyDown(event, \'" + stateListWrapperDivId + "\')\" onClick='clearSelectionStateList(\"" + stateListWrapperDivId + "\");'/>")
											.before(
													"<img class='uscb-input-box-icon "
															+ ui.item.icon_status
															+ "' src='"
															+ ui.item.icon_path
															+ "' />").blur();

									event.preventDefault();
								} else {
									location.href = ui.item.url;
								}
							} else if (ui.item.show_item === 'uscb-show-attachment') {
								$stateListInput.val('');								
								$stateList.find('.uscb-state-item-close').remove();							
								$stateListInput
										.val(
												ui.item.value
														+ " ["
														+ ui.item.file_size
														+ "]")
										.after(
												"<img role='button' aria-label='clear state selection' tabindex='0' class='uscb-state-item-close' src='/etc.clientlibs/census/clientlibs/census-pattern-library/resources/images/close-3-teal.svg' onkeydown=\"onClearStateSelectionKeyDown(event, \'" + stateListWrapperDivId + "\')\" onClick='clearSelectionStateList(\"" + stateListWrapperDivId + "\");'/>")
										.before(
												"<img class='uscb-input-box-icon "
														+ ui.item.icon_status
														+ "' src='"
														+ ui.item.icon_path
														+ "' />");

								$('#data-uscb-download-btn-wrapper-' + id).html("<a class='uscb-primary-button uscb-download-button' target='_blank' href='"
														+ ui.item.url
														+ "' filetrack='"
														+ ui.item.fileTrackKey
														+ "'>Download this file</a>");								
								$stateListInput.autocomplete("search", "");
								event.preventDefault();
							} else {
								// Should never happen.
								console.log('Other outside option...');
							}
							return false;
						},
						focus : function(event, ui) {
							$stateListInput.val( ui.item.value );
							return false;
						},
						create : function() {							
							$(this).data('ui-autocomplete')._renderItem = function(
									ul, item) {
								return $('<li class="stateMenuItem">')
										.append(
                                                '<a href="'
                                                    + item.url
                                                    + '" class="uscb-stateitem ' 
                                                    + item.left_padding
                                                    + '  capitalized" title="'
                                                    + item.tool_tip
                                                    + '" filetrack="' 
                                                    + item.fileTrackKey 
                                                    + '" onclick="CensusStateList.stateListSelectorAnalytics(this);">'
                                                    + '<span class="uscb-layout-align-start-center">'
                                                    + '<img class="'
                                                    + item.icon_status 
                                                    + ' uscb-state-list-icon" src="' 
                                                    + item.icon_path                                                                         
                                                    + '" />'
                                                    + item.label 
                                                    + '</span>'
                                                    + ' <span class="uscb-file-size '+ item.show_attachment_size +'">['
                                                    + item.file_size                                                                           
                                                    + ']</span></a>')
										.appendTo(ul);
							};
						}
					});
		}// end if		

		$stateList.click(function(e) {			
			if (!e.target.className.startsWith('uscb-state-item-close')) {
				if (isOpen) {					
					$stateListInput.autocomplete("close");
					isOpen = false;					
				} else {
					$stateListInput.autocomplete("search", "");					
					isOpen = true;
					e.stopPropagation();
				}
			}
		});
		
		$(window).click(function(e){			
			if ( e.target.className.startsWith('data-uscb-dropdown-click') ) {
				//do nothing...
			} else {
				if (isOpen) {					
					$stateListInput.autocomplete("close");
					isOpen = false;					
				}
			}
		});
		
	}// end init function


	function stateListSelectorAnalytics(elem) { 
		///DTM code///
		var res = $(elem).attr('href').split("/");		
		if( typeof($(elem).attr('filetrack')) != 'undefined' ){			
		    digitalData.event.eventInfo.download.fileUrl = $(elem).attr('filetrack');
		    digitalData.event.eventInfo.download.fileName = res[res.length - 1];
		    digitalData.event.eventName = 'Download';
		}else{
			digitalData.event.eventInfo.download.fileUrl = null;
		    digitalData.event.eventInfo.download.fileName = null;
		    digitalData.event.eventName = null;
		}
	 }
	
	function sortJSON(rawData, key, sortOrder) {
	    return rawData.sort(function(a, b) {
	        var x = a[key]; var y = b[key];
	        if (sortOrder === 'ASC' ) { return ((x < y) ? -1 : ((x > y) ? 1 : 0)); }
	        if (sortOrder === 'DESC') { return ((x > y) ? -1 : ((x < y) ? 1 : 0)); }
	    });
	}

	return {
		initStateList: initStateList,
		stateListSelectorAnalytics: stateListSelectorAnalytics
	}

})(jQuery, window._, digitalData);

var CensusPageTabs = (function($, _) {
	
	var pageTabs = [];
    
    var initPageTabs = function(id, items) {
    	
    	var pageTabObj = _.find(pageTabs, {"id": "" + id + ""});
    	if (!pageTabObj) {
    		pageTabObj = {
    			id: id
    		}
    		pageTabs.push(pageTabObj);
    	}
    	
    	var $pageTab = $("#" + id);    	
    	
    	$pageTab.find(".uscb-tab").each(function (index, element) {
            $(element).click(function () {
                $(this).siblings().removeClass("uscb-tab-active")
                $(this).toggleClass("uscb-tab-active");
            });      
        });


		var $tabMenuButton = $pageTab.find(".tabMenu__button");

		var toggleMenuDropdown = function() {
			$('.uscb-tab-dropdown-content').toggleClass("uscb-tab-show");

			var newAriaExpandedState = $tabMenuButton.attr('aria-expanded') === 'true' ? 'false' : 'true';

			$tabMenuButton.attr('aria-expanded', newAriaExpandedState)
		};

		$tabMenuButton.click(function(event){
			toggleMenuDropdown()
		});

		$tabMenuButton.keydown(function(event) {
			if (event.keyCode === 27 && $('.uscb-tab-dropdown-content').hasClass("uscb-tab-show")) {
				toggleMenuDropdown()
			}

			if (event.keyCode === 13 || event.keyCode === 32) {
				toggleMenuDropdown()
			}
		});
    };
    
    var togglePageTabDropdown = function(event) {	    
	    var selectedElement;
	    var ulElement = $('.data-uscb-form-dropdown-selection');
	    var ulDisplayedElement = $('.uscb-form-dropdown-button');
	    var target = $(event);
	    
	    if (target.is("li") ) {
	        selectedElement = target[0];
	        ulElement.toggleClass('uscb-hide-dropdown');		        
	        $(location).attr("href", $(selectedElement).attr("id"));
	    } else {
	        ulElement.toggleClass('uscb-hide-dropdown');
	        $('.uscb-form-dropdown').prepend("<div class='uscb-dropdown-backdrop'></div>");
	        $('.uscb-dropdown-backdrop').click(function(event) {
	            ulElement.toggleClass('uscb-hide-dropdown');
	            $(this).remove();
	            event.stopPropagation();
	        });
	    }    	
    }
    
    return {
        initPageTabs: initPageTabs,
        togglePageTabDropdown: togglePageTabDropdown
    }

})(jQuery, window._);


        
var CensusPagination = (function($, _, digitalData) {   
    var paginations = [];
    var initPagination = function(id) {
        var paginationObj = _.find(paginations, {
            'id': '' + id + ''
        });
      var  displayAs = $('#txtdisplayas_' + id ).val();
      var  listSource = $('#txtlistsource_' + id).val();
      var  currentPage = $('#txtcurrentpage_' + id).val();
      var  totalPages = $('#txttotalpages_' + id).val();
      var  numPagesToDisplay = $('#txtnumpagetodisplay_' + id).val();
      var  startPageNum = $('#txtstartpagenum_' + id).val();
      var  openNonAEMAttachmentListNewTab = $('#txtopeninnewtabnonaemattachment_' + id).val();
      
        if (!paginationObj) {
            paginationObj = {            		
                id: id,
                displayAs : displayAs,
                listSource: listSource,
                currentPage : currentPage,
                totalPages : totalPages,
                numPagesToDisplay : numPagesToDisplay,
                startPageNum : startPageNum,
                openNonAEMAttachmentListNewTab: openNonAEMAttachmentListNewTab
            }
            paginations.push(paginationObj);
        }
        showHidePrevNextButtons(id);       
    };

    function updateCurrentPage(page, element, componentName) {
     	var paginationObj = _.find(paginations, {
             "id": "" + componentName + ""
         });
     	
        $(element).siblings('li.curpage').children('a').removeClass('uscb-pagination-active');
        $(element).siblings('li.curpage').removeClass('curpage');

        $(element).addClass('curpage');
        $(element).children('a').addClass('uscb-pagination-active');

        paginationObj.currentPage = page;
    }
    
    function showHidePrevNextButtons(className) {
    	 var paginationObj = _.find(paginations, {
             'id': '' + className + ''
         });
        $('.prevButton'+className).toggle(paginationObj.currentPage != 1);
        $('.nextButton'+className).toggle(paginationObj.currentPage != paginationObj.totalPages);
    }

    var gotoPrevPage = function(event, ListId) {
    	var componentName = $('#' + ListId.id).attr('name');
    	var paginationObj = _.find(paginations, {
            'id': '' + componentName + ''
        });
    	
        event.preventDefault();
       
        var prev = $('#li_' + componentName + '_' + paginationObj.currentPage).prev();
        
        if (paginationObj.currentPage > 1) {
            gotoPage(event, prev, ListId);
        } else {
        	$('.prevButton').addClass('uscb-hide');
            event.preventDefault();
            return false;
        }
    };

    var gotoNextPage = function(event, ListId) {
    	var componentName = $('#' + ListId.id).attr('name');
    	var paginationObj = _.find(paginations, {
    		'id': '' + componentName + ''
    	});
    	
        var next = $('#li_' +componentName + '_' + paginationObj.currentPage).next();        
        
        if (paginationObj.currentPage < paginationObj.totalPages) {       	
            gotoPage(event, next, ListId);
        } else {
        	$('.nextButton').addClass('uscb-hide');
            event.preventDefault();
            return false;
        }
    };
    
    var goToNextPageKeyDown = function(event, ListId) {
		if (event.keyCode == 13) {
			CensusPagination.gotoNextPage(event, ListId);
		}
	};
    
    var gotoFirstPage = function(event, ListId) {
        var componentName = $('#' + ListId.id).attr('name');
        var first = $('#li_' + componentName + '_1').addClass('first');
        gotoPage(event, first, ListId, 'first');
    };

    var gotoLastPage = function(event, ListId) {
        var componentName = $('#' + ListId.id).attr('name');
        var paginationObj = _.find(paginations, {
            'id': '' + componentName + ''
        });
        var last = $('#li_' + componentName + '_' + paginationObj.totalPages);
        gotoPage(event, last, ListId, 'last');
    };
    
    var gotoLastPageKeyDown = function(event, ListId) {
		if (event.keyCode == 13) {
			CensusPagination.gotoLastPage(event, ListId);
		}
	};

    var gotoPage = function (event, element, ListId, pageIndicator) {
        var componentName = $('#' + ListId.id).attr('name');
        var page = null;
        var paginationObj = _.find(paginations, {
            'id': '' + componentName + ''
        });
        
        if ('first' === pageIndicator) {
            page = 1;
        } else if ('last' === pageIndicator) {
            page = paginationObj.totalPages;
        }
        
        if (null != element && null == page) {
            page = $(element).data('target');
        }
        
        var slingModelURL = $('#txtslingmodelurl_' + componentName).val();
        var pageURL = $('#txtpageurl_' + componentName).val();
        var chosenYear = 0;
        var pageURLFragments = pageURL.split('.');
        
        /* 
         * Tabbed census list pageURL e.g: /content/census/en/uat/aem6-4-testing/test-cms-1149-dynamic-list-component.2015.html/page/
         */
        if (pageURLFragments.length > 1) {
        	if (pageURLFragments[1].substring(0,3).toLowerCase() === 'all') {
    			chosenYear = 'All';
    		} else {        			        			
    			chosenYear = parseInt(pageURLFragments[1].substring(0,4));        			
    		}
        }

        if (!$(element).hasClass('curpage')) {
            var selectedTagIds = "";    
            var all_checkboxes = $('.chk-filter-tag');
            
            for (var i = 0; i < all_checkboxes.length; i++) {               
                if ($(all_checkboxes[i]).is(':checked')) {              
                    if ("" == selectedTagIds) {
                        selectedTagIds += $(all_checkboxes[i]).attr('data-target');
                    } else {
                        selectedTagIds += ',' + $(all_checkboxes[i]).attr('data-target');
                    }
                }
            }//end for...loop
            
            var urlVal =  (typeof(chosenYear) == 'number' && chosenYear > 0) || chosenYear == 'All' ? slingModelURL + '?offSet=' + page + '&yearParam=' + chosenYear : slingModelURL + '?offSet=' + page;
            
            $.ajax( {
                url: urlVal,
                data: selectedTagIds.length > 0 ? selectedTagIds : null,
                success: function(result) {                                     
                    updateCurrentPage(page, element, componentName);
                    showHidePrevNextButtons(componentName);
                    
                    if (paginationObj.displayAs === 'images') {
                        $('#listImagesContainer_' + componentName).html(buildGalleryItems(result.mlist.listItems, paginationObj.currentPage, componentName));
                    } else if (paginationObj.displayAs === 'tiles') {
                        $('#listTilesContainer_' + componentName ).html(buildTileItems(result.mlist.listItems, paginationObj.currentPage, componentName));
                    } else if (paginationObj.displayAs === 'experience') {
                    	$('#currentPageSpan_' + componentName ).html(paginationObj.currentPage);
						var source = $('' + result + '');
                        $('#exp_fragments_' + componentName).html(source.find('div#exp_fragments_' + componentName).html());
                    } else if (paginationObj.displayAs === 'attachment') {
                        if (paginationObj.listSource === 'attachment') {
                            $('#listArticlesContainer_' + componentName).html(buildNonAEMAttachmentItems(result.mlist.listItems, paginationObj.currentPage, componentName, paginationObj.openNonAEMAttachmentListNewTab));
                        } else {
                            //its a dam
                            $('#listArticlesContainer_' + componentName).html(buildDAMAttachmentItems(result.mlist.listItems, paginationObj.currentPage, componentName));
                        }
                    } else if (paginationObj.displayAs === 'calendar') {
                        $('#listArticlesContainer_' + componentName).html(buildEventsItems(result.mlist.listItems, paginationObj.currentPage, componentName));
                    } else {
                        if (paginationObj.listSource === 'attachment') {
                            $('#listArticlesContainer_' + componentName).html(buildNonAEMAttachmentItems(result.mlist.listItems, paginationObj.currentPage, componentName, paginationObj.openNonAEMAttachmentListNewTab));
                        } else if (paginationObj.listSource === 'dam') {
                            $('#listArticlesContainer_' + componentName).html(buildDAMAttachmentItems(result.mlist.listItems, paginationObj.currentPage, componentName));
                        } else if (paginationObj.listSource === 'events') {
                            $('#listArticlesContainer_' + componentName).html(buildEventsItems(result.mlist.listItems, paginationObj.currentPage, componentName));
                        } else if (paginationObj.listSource === 'mixed') {
                            $('#listArticlesContainer_' + componentName).html(buildMixedItems(result.mlist.listItems, paginationObj.currentPage, componentName));
                        } else {
                            $('#listArticlesContainer_' + componentName).html(buildListItems(result.mlist.listItems, paginationObj.currentPage, componentName));
                        }
                    }
                    resetPaginationControl(page, element, componentName);
                    scrollToSection(ListId.id);
                },
                error: function(err) {
                    console.log(err);
                }
            } );
        }

        event.preventDefault();
        return false;
    };

    function scrollToSection(sectionId) {
        document.getElementById(sectionId).scrollIntoView();
    }

    function resetPaginationControl(page, element, componentName) {
    	var paginationObj = _.find(paginations, {
            "id": "" + componentName + ""
        });
        var newStartPageNum = 1;
        if (page - 5 > 0) {
            newStartPageNum = page - 5;
        } else if (page - 4 > 0) {
            newStartPageNum = page - 4;
        } else if (page - 3 > 0) {
            newStartPageNum = page - 3;
        } else if (page - 2 > 0) {
            newStartPageNum = page - 2;
        } else if (page - 1 > 0) {
            newStartPageNum = page - 1;
        }

        if (newStartPageNum != paginationObj.startPageNum) {
            var newPageTarget = newStartPageNum;
            $.each($('.pagenumber_'+ componentName), function(i, el) {
                $(el).find('a').text(newPageTarget).attr('href', '=pageURL' + newPageTarget);
                if (newPageTarget != paginationObj.currentPage) {
                    $(el).attr('id', 'li_' +componentName + "_"+ newPageTarget).removeClass('curpage');
                    $(el).attr('id', 'li_' +componentName + "_"+ newPageTarget).children('a').removeClass('uscb-pagination-active');
                } else {
                    $(el).attr('id', 'li_' +componentName + "_"+ newPageTarget).addClass('curpage');
                    $(el).attr('id', 'li_' +componentName + "_"+ newPageTarget).children('a').addClass('uscb-pagination-active');
                }
                $(el).data('target', newPageTarget);
                if (newPageTarget > paginationObj.totalPages) {
                    $(el).css('display', 'none');
                } else {
                    $(el).css('display', 'inline');
                }
                // set aria label for screen readers
                var ariaLabel = $(el).find('a');
                if (ariaLabel.length) {
                	ariaLabel.attr('aria-label', "page " + newPageTarget);
                }
                newPageTarget++
            });
            paginationObj.startPageNum = newStartPageNum;
        }
    }
    
    function buildMixedItems(listItems, currentPage, componentName) {
    	$('#currentPageSpan_' + componentName).html(currentPage);
    	var mixedItemHtml = '';
    	for (var i = 0; i < listItems.length; i++) {
    		var listItem = listItems[i];
    		
    		switch (listItem.mixedListType) {
    			case 'calendar':
    				mixedItemHtml += buildCalendarItem(listItem);
    				break;
    			case 'dam':
    				mixedItemHtml += buildDAMItem(listItem);
    				break;
    			case 'attachment':
    				mixedItemHtml += buildAttachmentItem(listItem, "false");
    				break;
    			case 'manual':
    				mixedItemHtml += buildCQ5Item(listItem, i);
    				break;
    			default:
    				mixedItemHtml += buildCQ5Item(listItem, i);
    		}
    	}
    	return mixedItemHtml;
    }
    
    function buildCalendarItem(listItem) {
    	var calendarItemHtml = '';
    	calendarItemHtml += '<div>';
    	calendarItemHtml += '<img style="margin-bottom: 0px;" src="' + listItem.myImageSrc + '" height="12" width="12">';
    	calendarItemHtml += '&nbsp;';
    	calendarItemHtml += '<a onclick="popevent(); linkClick(this, \'Census List Component\'); return false;" href="#" tabindex="0">';
    	calendarItemHtml += listItem.myTitle;
    	calendarItemHtml += '</a>';
    	calendarItemHtml += '<br>';
    	calendarItemHtml += listItem.myDate;
    	calendarItemHtml += '<br>';
    	calendarItemHtml += '</div>';
		
		return calendarItemHtml;
    }
    
    function buildEventsItems(listItems, currentPage, componentName) {
    	$('#currentPageSpan_' + componentName).html(currentPage);
		var eventsItemHtml = '';
		for (var i = 0; i < listItems.length; i++) {
			var listItem = listItems[i];
			
			eventsItemHtml += buildCalendarItem(listItem);
		}
		return eventsItemHtml;
    }
    
    function buildDAMItem(listItem) {
    	var DAMItemHtml = '';
    	
    	DAMItemHtml += '<div>';
    	DAMItemHtml += '<div class="uscb-margin-TB-5">';
    	DAMItemHtml += '<span class="uscb-margin-TB-0 uscb-layout-align-start-start">';
    	DAMItemHtml += '<a onclick="linkClick(this, \'Census List Component\');" class="uscb-display-block uscb-padding-R-8 uscb-padding-TB-2 uscb-layout-align-start-center" title="' + listItem.myTitle + '" name="' + listItem.myTitle + '" href="' + listItem.myHyperlink + '" tabindex="0">';
    	DAMItemHtml += '<img src="' + listItem.myImageSrc + '" alt="' + listItem.myImageAlt + '" style="width: 20px; height: 20px">';
    	DAMItemHtml += '</a>';
		
    	DAMItemHtml += '&nbsp;';
		
    	DAMItemHtml += '<span class="uscb-padding-T-2">';
    	DAMItemHtml += '<a onclick="linkClick(this, \'Census List Component\');" class="uscb-text-link" title="' + listItem.myTitle + '" name="' + listItem.myTitle + '" href="' + listItem.myHyperlink + '" tabindex="0">';
    	DAMItemHtml += listItem.myTitle;
    	DAMItemHtml += '</a>';
		
    	DAMItemHtml += '&nbsp;';
		
    	DAMItemHtml += '<span class="uscb-sub-heading-2 uscb-color-primary">';
		
		if (listItem.mySize.length > 0) {
			if (listItem.myImageSrc != '' && listItem.myImageSrc != '/etc.clientlibs/census/clientlibs/census-pattern-library/resources/images/external-link.svg') {
				DAMItemHtml += '[' + listItem.mySize + ']';
			} else if (listItem.myImageSrc == '' && listItem.myImageSrc == '/etc.clientlibs/census/clientlibs/census-pattern-library/resources/images/external-link.svg') {
				DAMItemHtml += listItem.mySize.length;
			}
		}
		DAMItemHtml += '</span>';
		DAMItemHtml += '</span>';
		DAMItemHtml += '</span>';
		
		DAMItemHtml += '<div class="uscb-sub-heading-2 uscb-color-secondary-1 uscb-line-height-20-18 uscb-margin-T-5 uscb-margin-B-20">';
		DAMItemHtml += listItem.myAttachmentDescription;
		DAMItemHtml += '</div>';
		DAMItemHtml += '</div>';
		DAMItemHtml += '</div>';
		
		return DAMItemHtml;
    }
    
    function buildDAMAttachmentItems(listItems, currentPage, componentName) {
    	$('#currentPageSpan_' + componentName).html(currentPage);
		var damAEMAttachmentItemHtml = '';
		for (var i = 0; i < listItems.length; i++) {
			var listItem = listItems[i];
			damAEMAttachmentItemHtml += buildDAMItem(listItem);
		}
		
		return damAEMAttachmentItemHtml;
    }
    
    function buildAttachmentItem(listItem, newTab) {
    	var attachmentItemHtml = '';
    	
    	var title = listItem.myTitle;
		if (listItem.myExternalLink == true) {
			title = 'This external link , does not imply endorsement of any particular product, company, or content.';
		}
		
		var target = '';
		if ('true' === newTab) {
			target = '_blank';
		}
		
		attachmentItemHtml += '<div>';
		attachmentItemHtml += '<div class="uscb-margin-TB-5">';
		attachmentItemHtml += '<span class="uscb-margin-TB-0 uscb-layout-align-start-start">';
		attachmentItemHtml += '<a onclick="linkClick(this, \'Census List Component\');" filetrack="' + listItem.filetrack +'" class="uscb-display-block uscb-padding-R-8 uscb-padding-TB-2 uscb-layout-align-start-center" title="' + title + '" name="' + listItem.myTitle + '" href="' + listItem.myHyperlink + '" tabindex="0" target="' + target + '">';
		attachmentItemHtml += '<img src="' + listItem.myImageSrc + '" alt="' + listItem.myImageAlt + '" style="width: 20px; height: 20px">';
		attachmentItemHtml += '</a>';
		attachmentItemHtml += '&nbsp;';
		
		attachmentItemHtml += '<span class="uscb-padding-T-2">';
		attachmentItemHtml += '<a onclick="linkClick(this, \'Census List Component\');" class="uscb-text-link" filetrack="' + listItem.filetrack + '" title="' + listItem.myTitle + '" name="' + listItem.myTitle + '" href="' + listItem.myHyperlink + '" tabindex="0" target="' + target + '">';
		attachmentItemHtml += listItem.myTitle;
		attachmentItemHtml += '</a>';
		
		attachmentItemHtml += '&nbsp;';
		
		attachmentItemHtml += '<span class="uscb-sub-heading-2 uscb-color-primary">';
		
		if (parseInt(listItem.mySize) > 0) {
			if (listItem.myImageSrc != '' && listItem.myImageSrc != '/etc.clientlibs/census/clientlibs/census-pattern-library/resources/images/external-link.svg') {
				attachmentItemHtml += '[' + listItem.mySize + ']';
			} else if (listItem.myImageSrc == '' && listItem.myImageSrc == '/etc.clientlibs/census/clientlibs/census-pattern-library/resources/images/external-link.svg') {
				attachmentItemHtml += listItem.mySize.length;
			}
		}
		attachmentItemHtml += '</span>';
		attachmentItemHtml += '</span>';
		attachmentItemHtml += '</span>';
		
		attachmentItemHtml += '<div class="uscb-sub-heading-2 uscb-color-secondary-1 uscb-line-height-20-18 uscb-margin-T-5 uscb-margin-B-20">';
		attachmentItemHtml += listItem.myAttachmentDescription;
		attachmentItemHtml += '</div>';
		attachmentItemHtml += '</div>';
		attachmentItemHtml += '</div>';
		
		return attachmentItemHtml;
    }
    
    function buildNonAEMAttachmentItems(listItems, currentPage, componentName, newTab) {
    	$('#currentPageSpan_' + componentName).html(currentPage);
		var nonAEMAttachmentItemHtml = '';
		for (var i = 0; i < listItems.length; i++) {
			var listItem = listItems[i];
			nonAEMAttachmentItemHtml += buildAttachmentItem(listItem, newTab);
		}
		return nonAEMAttachmentItemHtml;
    }
    
    function buildCQ5Item(listItem, i) {
    	var cq5ItemHtml = '';
    	
    	if (i == 0) {
    		cq5ItemHtml += '<hr class="uscb-light-teal-hr uscb-margin-T-0"/>';
		}
    	
    	var resolvedHyperlink = (typeof(listItem.externalizedHyperlink) != 'undefined' && listItem.externalizedHyperlink != null) ? listItem.externalizedHyperlink : listItem.myHyperlink;
    	
    	cq5ItemHtml += '<a href="' + resolvedHyperlink + '" class="uscb-list-item">';
    	cq5ItemHtml += '<div class="uscb-list-item-container">';
    	cq5ItemHtml += '<p class="uscb-sub-heading-2 uscb-color-secondary-1 uscb-line-height-20-18 uscb-uppercase uscb-margin-TB-02">';
		
    	if (listItem.contentType) {
			cq5ItemHtml += listItem.contentType.toUpperCase();
		}
		
    	if (listItem.myDisplayDate && listItem.contentType) {
			cq5ItemHtml += ' | ';
		}
		
    	if (listItem.myDisplayDate) {
			cq5ItemHtml += listItem.myDisplayDate.toUpperCase();
		}
		cq5ItemHtml += '</p>';
		cq5ItemHtml += '<p class="uscb-title-3 uscb-margin-TB-02">';
		cq5ItemHtml += listItem.myTitle;
		cq5ItemHtml += '</p>';
		cq5ItemHtml += '<p class="uscb-sub-heading-2 uscb-color-secondary-1 uscb-line-height-20-18 uscb-margin-TB-02">';
		cq5ItemHtml += listItem.myDescription;
		cq5ItemHtml += '</p>';
		cq5ItemHtml += '<hr class="uscb-light-teal-hr" />';
		cq5ItemHtml += '</div>';
		cq5ItemHtml += '</a>';
		
		return cq5ItemHtml;
    }
    
    function buildListItems(listItems, currentPage, componentName) {
		$('#currentPageSpan_' + componentName).html(currentPage);
		
		var listItemHtml = '';
		for (var i = 0; i < listItems.length; i++) {
			var listItem = listItems[i];
			listItemHtml += buildCQ5Item(listItem, i);
		}
		
		return listItemHtml;
	}

    function buildGalleryItems(galleryItems, currentPage, componentName) {
        $('#currentPageSpan' + (componentName ? ("_" + componentName) : "")).html(currentPage);
        var galleryItemHtml = '';
        
        for (var i = 0; i < galleryItems.length; i++) {
        	var resolvedHyperlink = (typeof(galleryItems[i].externalizedHyperlink) != 'undefined' && galleryItems[i].externalizedHyperlink != null) ? galleryItems[i].externalizedHyperlink : galleryItems[i].myHyperlink;
        	
            galleryItemHtml += '<a href="' + resolvedHyperlink + '" class="uscb-list-item uscb-visual-list" tableindex="">';
            galleryItemHtml += '<div class="uscb-nav-button-container uscb-visual-list-container">';
            galleryItemHtml += '<img src="' + galleryItems[i].myImageSrc + '" alt="' + encodeURIComponent(galleryItems[i].myDescription) + '">';
            galleryItemHtml += '<div class="uscb-visual-list-title">';
            galleryItemHtml += '<p class="uscb-layout-align-vert-flex-start uscb-title-3 uscb-text-nav-button-spacing uscb-margin-TB-5 uscb-padding-L-5 uscb-padding-T-gt-sm-5">' + galleryItems[i].myTitle + '</p>';
            galleryItemHtml += '</div>';
            galleryItemHtml += buildNavigationButton();
            galleryItemHtml += '</div>';
            galleryItemHtml += '</a>';
        }
        
        return galleryItemHtml;
    }


    function buildTileItems(tileItems, currentPage, componentName) {
        $('#currentPageSpan' + (componentName ? ("_" + componentName) : "")).html(currentPage);
		var tileItemHtml = '';
		
		for( var i=0; i < tileItems.length; i++ ) {
			var resolvedHyperlink = (typeof(tileItems[i].externalizedHyperlink) != 'undefined' && tileItems[i].externalizedHyperlink != null) ? tileItems[i].externalizedHyperlink : tileItems[i].myHyperlink;
			
			tileItemHtml +=	'<a href="' + resolvedHyperlink + '" class="uscb-list-item uscb-featured-list uscb-margin-R-gt-md-20 uscb-margin-B-20 uscb-margin-L-md-16">';
			tileItemHtml +=	'<div class="uscb-featured-list-container uscb-nav-button-container "' + tileItems[i].mytileColClass + '>';
			tileItemHtml +=	'<img src="' + tileItems[i].myImageSrc + '" alt="' + encodeURIComponent(tileItems[i].myDescription) + '">';

			var tagName = tileItems[i].mydisplayTagName ? tileItems[i].mydisplayTagName : tileItems[i].myprimaryTopicTagName;
			
			if (tagName) {
				tileItemHtml +=	'<div class="' + tileItems[i].myprimaryTopicTagColorCSS + ' uscb-featured-list-tag uscb-primary-blue-bg uscb-layout-column uscb-layout-align-center-center">';			
				tileItemHtml +=	'<p class="uscb-sub-heading-2-condensed uscb-bold uscb-color-white uscb-uppercase uscb-margin-LR-30 uscb-margin-TB-10">' + tagName + '</p>';
				tileItemHtml +=	'</div>';
			}
			
			tileItemHtml +=	'<div class="uscb-featured-list-title">';
			tileItemHtml +=	'<div class="uscb-margin-15">';
			tileItemHtml +=	'<p class="uscb-title-3 uscb-margin-T-0 uscb-margin-B-10">' + tileItems[i].myTitle + '</p>';
			tileItemHtml +=	'<p class="uscb-sub-heading-2 uscb-color-secondary-1 uscb-line-height-20-18 uscb-text-nav-button-spacing">' + tileItems[i].myDescription + '</p>';
			tileItemHtml +=	' </div>';
			tileItemHtml +=	' </div>';
			tileItemHtml += buildNavigationButton();			
			tileItemHtml +=	'</div>';
			tileItemHtml +=	'</a>';
		}
		
		return tileItemHtml;
    }

    function buildNavigationButton(color, ctaText, cssClass, ctaTextClass) {
        var navBtn = "";

        navBtn += '<div class="' + (cssClass ? cssClass : (ctaText ? 'uscb-text-img-button' : 'uscb-icon-button')) + '">';
        navBtn += (ctaText ? ('<span class="' + (ctaTextClass ? ctaTextClass : '') + '">' + ctaText + '</span>') : '');
        navBtn += '<svg class="ovaltealcircle' + (ctaText ? ' uscb-margin-L-10' : '') + '" viewBox="0 0 64 64" aria-label="read more / go to the page">';			
        navBtn += '<path fill="none" stroke="' + (color ? color : '#008392') + '" stroke-miterlimit="10" stroke-width="2" d="M28 21.002L41 32 28 43.002" stroke-linejoin="miter" stroke-linecap="butt"></path>';			
        navBtn += '<circle cx="32" cy="32" r="30" fill="none" stroke="' + (color ? color : '#008392') + '" stroke-miterlimit="10" stroke-width="2" stroke-linejoin="miter" stroke-linecap="butt"></circle>';
        navBtn += '</svg>';
        navBtn += '</div>';

        return navBtn;
    }


    return {
        initPagination: initPagination,
        gotoPage: gotoPage,
        gotoLastPage: gotoLastPage,
        gotoLastPageKeyDown: gotoLastPageKeyDown,
        gotoFirstPage: gotoFirstPage, 
        gotoPrevPage: gotoPrevPage,
        gotoNextPage: gotoNextPage,
        goToNextPageKeyDown: goToNextPageKeyDown,
        buildTileItems: buildTileItems
    }

})(jQuery, window._);
var CensusListFilter = (function($, _, digitalData) {   
    var censusListFilters = [];
    
    var initCensusListFilter = function(id) {
	    var censusListFilterObj = _.find(censusListFilters, {
	        "id": "" + id + ""
	    });
	    
	    var  displayAs = $('#txtdisplayas_' + id ).val();      
	      
	    if (!censusListFilterObj) {
	    	censusListFilterObj = {            		
	            id: id,
	            displayAs : displayAs
	        }
	    	censusListFilters.push(censusListFilterObj);
	    }
    }    
    
    var clearAllFilters = function(event, ListId) {
    	event.preventDefault();    	
    	var baseURL = window.location.href.split('?')[0];
    	window.history.pushState('', '', baseURL);
		location.reload(true);
    }
    
    var removeFilter = function(event, ListId, tagId) {
    	event.preventDefault();    	
    	var componentName = $("#" + ListId.id).attr("name");
    	var paramValue = $.urlParam('tagfilter_' + componentName);    	
    	if (typeof(paramValue) != 'undefined') {
	    	var tagFilters = paramValue.split(',');
	    	var newParam = '?tagfilter_' + componentName + '=';
	    	
	    	for (var i = 0; i < tagFilters.length; i++) {
	    		if (tagFilters[i].trim() != decodeURIComponent(tagId).trim()) {
	    			newParam += tagFilters[i] + ',';
	    		}
	    	}
	    	
	    	newParam = newParam.slice(0, -1);
	    	var baseURL = window.location.href.split('?')[0];
	    	var newURL = baseURL + newParam;
	    	window.history.pushState('', '', newURL);	    	
	    	location.reload(true);
    	}
    }
    
    var filterResult = function(event, ListId) {
    	var componentName = $("#" + ListId.id).attr("name");
    	var selectedTagIds = "tagfilter_" + componentName + "=";    	    	
    	var all_checkboxes = $('.filter_' + componentName);
		
		for (var i = 0; i < all_checkboxes.length; i++) {    			
			if ($(all_checkboxes[i]).is(':checked')) {				
				selectedTagIds += $(all_checkboxes[i]).attr('data-target') + ",";				
			}
		}
		
		selectedTagIds = selectedTagIds.slice(0, -1);
        var baseURL = window.location.href.split('?')[0];
        window.history.pushState('', '', baseURL + '?' + selectedTagIds);
		location.reload(true);
    }
    
    $.urlParam = function(name){
        var results = new RegExp('[\?&]' + name + '=([^&#]*)').exec(window.location.href);
        if (results == null){
           return null;
        }
        else {
           return decodeURI(results[1]) || 0;
        }
    }

    return {
    	initCensusListFilter: initCensusListFilter,
    	filterResult: filterResult,
    	clearAllFilters: clearAllFilters,
    	removeFilter: removeFilter
    }

})(jQuery, window._);
( function() {

	// Toggle for small viewports to show the filters
	var $filterToggle = $( '.uscb-list-filter-toggle' );

	$filterToggle.on( 'click.listFilter', function( e ) {
		e.preventDefault();
		var id = $( this ).attr( 'href' ).substring( 1 );
		var $target = $( '#' + id );
		$target.toggleClass( 'uscb-hide-md' ).toggleClass( 'uscb-hide-sm' );
		// Assuming initial state has one shown and the other hidden
		$( this ).find( '.uscb-list-filter-show').toggleClass( 'uscb-hide');
		$( this ).find( '.uscb-list-filter-hide').toggleClass( 'uscb-hide');
	} );

	// When an input is checked, expose an additional label
	var $filterInputsParent = $( '.uscb-list-filter-group-content' );
	var $filterInputs = $filterInputsParent.find( 'input[type="checkbox"]' );
	var $selectedLabelsParent = $( '.uscb-list-filter-selected' );

	$filterInputs.on( 'change.listFilter', function() {

		// ACCENTURE: This would be a good spot to hook into an ajax request & adjust the URL

		if( this.checked ) {
			makeLabel( $( this ) );
		} else {
			removeLabel( $( this ) );
		}
	} );

	$selectedLabelsParent.find( '[type="reset"]').on( 'click.listFilter', function( e ) {
		e.preventDefault(); // default sets it back to the way it was loaded (initial checkboxes checked)
		$filterInputs.prop( 'checked', false ); // adjusting so that it clears everything - regardless of initial state
		removeLabel( $filterInputs ); // remove all labels

		// ACCENTURE: In lieu of triggering change on every input (firing lots of ajax requests), recommend sending an ajax request here instead
	} );

	function makeLabel( $forInputs ) {
		$forInputs.each( function() {
			var inputId = $( this ).attr( 'id' );
			var labelDisplay = $filterInputsParent.find( 'label[for="' + inputId + '"]').text(); // Find existing label and grab the display text

			var $newLabel = $( '<label class="uscb-button uscb-primary-button uscb-button-small" />' );
			$newLabel.attr( 'for', inputId );
			$newLabel.text( labelDisplay );

			$selectedLabelsParent.prepend( $newLabel );
		} );

		$selectedLabelsParent.removeClass( 'uscb-hide' );
	}

	function removeLabel( $forInputs ) {
		$forInputs.each( function() {
			var inputId = $( this ).attr( 'id' );
			$selectedLabelsParent.find( 'label[for="' + inputId + '"]' ).remove();
		} );

		if( !$selectedLabelsParent.find( 'label' ).length ) {
			$selectedLabelsParent.addClass( 'uscb-hide' );
		}
	}


	// The dropdowns which expose each filter group's options
	var $filterDropdownToggles = $( '.uscb-list-filter-group-toggle' );
	$filterDropdownToggles.on( 'click.listFilter', dropdownListener );


	// Generic dropdown functions, associating an a's href with a div with a matching id
	function dropdownListener( e ) {
		e.preventDefault();
		openDropdown( $( this ) );
	}

	function openDropdown( $toggle ) {
		var id = $toggle.attr( 'href' ).substring( 1 );
		var $dropdown = $( '#' + id );
		
		$dropdown.removeClass( 'uscb-hide' );
		$toggle.parent().addClass( 'uscb-list-filter-open' );

		$toggle.off( 'click.listFilter' ); // stop listening for a click while its open (body listener will take care of it)
	
		// Let the current event stack finish and then add a listener to the body for on click outside the embed to close it
		setTimeout( function() {
			$( 'body' ).off( 'click.listFilter' ).on( 'click.listFilter', function( e ) {
				var $bodyTarget = $( e.target );
				if( !$bodyTarget.closest( $dropdown ).length && !$bodyTarget.is( $dropdown ) ) {
					closeDropdown( $toggle, $dropdown );
				}

				if( $bodyTarget.is( $filterDropdownToggles ) || $filterDropdownToggles.find( $bodyTarget ).length ) {
					e.preventDefault();
				}
			} );
		}, 0 );
	}

	function closeDropdown( $toggle, $dropdown ){
		$dropdown.addClass( 'uscb-hide' );
		$toggle.parent().removeClass( 'uscb-list-filter-open' );
		$( 'body' ).off( 'click.listFilter' );
		$filterDropdownToggles.off( 'click.listFilter ').on( 'click.listFilter', dropdownListener );
	}
} )();
(function() {
    'use strict';
    
    document.addEventListener( 'DOMContentLoaded', function() {
        var loaders = document.getElementsByClassName( 'uscb-loading-circular-indeterminate' );
        loaders = Array.prototype.slice.call( loaders );
        
        loaders.forEach( function(el) {
            el.innerHTML =
                '<svg version="1.1" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" x="0px" y="0px"' +
                ' width="100%" height="100%" viewBox="0 0 108 108" xml:space="preserve" >' +
                ' <path fill="#fff0" stroke-width="8" stroke-miterlimit="10" endcap="butt" d="M 54,54 m -50,0 a 50,50 0 1,0 100,0 a 50,50 0 1,0 -100,0"/>' +
                '</svg>';
              // Detect MS browsers and add the class suffix to display the correct animation
            if ( document.documentMode || !!navigator.userAgent.match(/Trident/g) || !!navigator.userAgent.match(/MSIE/g) || !!navigator.userAgent.match(/Edge/g) ) {
                el.className = 'uscb-loading-circular-indeterminate-IE';
            }
        });
    });
})();
