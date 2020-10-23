function toggleSharingDropDown(event, link) {
    var selectedElement;
    var target = $(event);
    var ulElement = $(target.siblings('ul')[0]);

    if (target.is("a") ) {
        selectedElement = target[0];
        ulElement.toggleClass('uscb-hide');

        var title = $(selectedElement).attr("title");
        if (title === "Instagram" || title === "Snapchat" || title === "YouTube") { //websites that have specific posting requirements
        	copyToClipboard(link);
        }

        // Grab the current logo and append it to the Pinterest URL as the media parameter
        if (title === 'Pinterest') {
            var currentLink = $(selectedElement).attr('href');
            var logoLink = location.origin + $('.uscb-nav-image').attr('src');
            $(selectedElement).attr('href', currentLink + '&media=' + logoLink);
        }

        $(location).attr("href", $(selectedElement).attr("id"));
    } else {
        if (ulElement[0] === undefined) {
            ulElement = target.closest('ul');
        }
        ulElement.toggleClass('uscb-hide');
    }
}

function nonBasicSharingItemClick(event, link) {
	var selectedElement;
    var target = $(event);
    
    if (target.is("a") ) {
        selectedElement = target[0];

        var title = $(selectedElement).attr("title");
        if (title === "Instagram" || title === "Snapchat" || title === "YouTube") { //websites that have specific posting requirements
        	copyToClipboard(link);
        }

        // Grab the current logo and append it to the Pinterest URL as the media parameter
        if (title === 'Pinterest') {
            var currentLink = $(selectedElement).attr('href');
            var logoLink = location.origin + $('.uscb-nav-image').attr('src');
            $(selectedElement).attr('href', currentLink + '&media=' + logoLink);
        }

        $(location).attr("href", $(selectedElement).attr("id"));
    } else {
        if (ulElement[0] === undefined) {
            ulElement = target.closest('ul');
        }
    }
}

function onKeyParent(event, share) {
    if (event.keyCode === 40) { //down arrow
   		var target = share.closest(".uscb-share-dropdown");
    	var element = target.closest(".share-options");

    	if (element) {
    		toggleTabIndex(document.getElementsByClassName("share-options"));
      		element.focus();
        	event.stopPropagation();
    	}
    }
}

function onKeyChild(event, element, link) {
	if (event.keyCode === 13 || event.keyCode === 32) { //enter or space
	    var ulElement = $('.uscb-share-dropdown-selection');
	    var target = $(element);

	    var selectedElement = target[0];
	    ulElement.toggleClass('uscb-hide-dropdown');

	    var title = $(selectedElement).attr("title");
        if (title === "Instagram" || title === "Snapchat" || title === "YouTube") { //websites that have specific posting requirements
        	copyToClipboard(link);
        }

	    $(location).attr("href", $(selectedElement).attr("id"));
	}
}

function toggleTabIndex(array) {
	for (var i = array.length - 1; i >= 0; i--) {
		if (array[i].tabIndex === -1) {
			array[i].tabIndex = 0;
		} else {
			array[i].tabindex = -1;
		}
	}
}

function copyToClipboard(link) {
	// Create new element
   var el = document.createElement('textarea');
   // Set value (string to be copied)
   el.value = link;
   // Set non-editable to avoid focus and move outside of view
   el.setAttribute('readonly', '');
   el.style = {position: 'absolute', left: '-9999px'};
   document.body.appendChild(el);
   // Select text inside element
   el.select();
   // Copy text to clipboard
   document.execCommand('copy');
   // Remove temporary element
   document.body.removeChild(el);
}
