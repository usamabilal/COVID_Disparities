function languageSelectionClick(lang) {
	var languageClick = 'Lang:' + lang;
	digitalData.event.eventInfo.language.pageLanguage = digitalData.page.pageInfo.pageName + '^' + languageClick;
	digitalData.event.eventName = 'Page Language';
	digitalData.event.eventInfo.eventName = 'Language Dropdown';
	digitalData.event.eventInfo.language = lang;

	if ( typeof(Storage) !== "undefined" ) {
		localStorage.setItem("pageLanguage", digitalData.event.eventInfo.language.pageLanguage);
		localStorage.setItem("langEventName", "Page Language");
		localStorage.setItem("eventInfoEventName", "Language Dropdown");
		localStorage.setItem("eventInfoLanguage", lang);
	} else{
		setCookie("pageLanguage", digitalData.event.eventInfo.language.pageLanguage);
		setCookie("langEventName", "Page Language");
		setCookie("eventInfoEventName", "Language Dropdown");
		setCookie("eventInfoLanguage", lang);
	}
}

function handleToggleLangDropdown(e) {
    if (event.keyCode === 13) {
		toggleLanguageDropDown(e.target);
    }
}

function toggleLanguageDropDown(elem) {
    var selectedElement;
    var ulElement = $('.uscb-lang-dropdown-selection');
    var ulDisplayedElement = $('.uscb-lang-dropdown-button');
    var target = $(elem);

    if (target.is("li") ) {
        selectedElement = target[0];
        ulElement.toggleClass('uscb-hide-dropdown');

        languageSelectionClick($(selectedElement).text());

        $(location).attr("href", $(selectedElement).attr("id"));
    } else {
        ulElement.toggleClass('uscb-hide-dropdown');
        $('.uscb-lang-dropdown').prepend("<div class='uscb-dropdown-backdrop'></div>");
        $('.uscb-dropdown-backdrop').click(function(event) {
            ulElement.toggleClass('uscb-hide-dropdown');

            $(this).remove();
            event.stopPropagation();
        });
    }
}

function onKeyParent(event) {
    if (event.keyCode === 40) { //down arrow
   		var target = document.getElementById("languageSelector");
    	var element = target.querySelector(".lang-options");

    	if (element) {
      		element.focus();
        	event.stopPropagation();
    	}
    }
}

function onKeyChild(event, element) {
	if (event.keyCode === 13 || event.keyCode === 32) { //enter or space
	    var ulElement = $('.uscb-lang-dropdown-selection');
	    var ulDisplayedElement = $('.uscb-lang-dropdown-button');
	    var target = $(element);

	    var selectedElement = target[0];
	    ulElement.toggleClass('uscb-hide-dropdown');

	    languageSelectionClick($(selectedElement).text());

	    $(location).attr("href", $(selectedElement).attr("id"));
	}
}

function moveLanguageSelector(){
	if (Modernizr.mq('only screen and (min-width : 10px) and (max-width : 992px)')) {//was 576px
		$('#mobileLanguageSelectorContainer').append($('#languageSelector'));
	} else {
		$('#desktopLanguageSelectorContainer').append($('#languageSelector'));
	}
}

$(document).ready(function(){
	moveLanguageSelector();
	$(window).resize(function(){
		moveLanguageSelector();
	});
});//end document.ready function
