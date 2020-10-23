/* 
 * This code is coupled with the Census search typeahead HTL.
 * 
 * It is framework-less and should remain that way for the foreseeable future to aid other, non-AEM census pages to utilize it. 
 * 
 * As such, it does not inject any dependencies.
 * 
 */
var CensusSearchTypeahead = (function() { // DO NOT INJECT FRAMEWORKS OR DEPENDENCIES THAT RELY ON FRAMEWORKS.
	
	var apiUri;
	var resultUrl;
	    
    var initSearchTypeahead = function(searchApiUri, searchResultUrl) {
    	apiUri = searchApiUri;
    	resultUrl = searchResultUrl;
    	
    	// Add a keyup event listener to our input element
        var searchInput = document.getElementById('data-uscb-header-input');
        
        var throttle = function(fn, threshhold, scope) {
            threshhold || (threshhold = 250);
            var last,
                deferTimer;
            return function () {
              var context = scope || this;
          
              var now = +new Date,
                  args = arguments;
              if (last && now < last + threshhold) {
                // hold on to it
                clearTimeout(deferTimer);
                deferTimer = setTimeout(function () {
                  last = now;
                  fn.apply(context, args);
                }, threshhold);
              } else {
                last = now;
                fn.apply(context, args);
              }
            };
        }

        var parseResponse = function(xmlDoc) {

            var getValue = function(xml, name) {
                var elt = xml.getElementsByTagName(name);
                if (elt && elt[0] && elt[0].childNodes && elt[0].childNodes[0] && elt[0].childNodes[0].nodeValue) {
                    return elt[0].childNodes[0].nodeValue;
                }

                return "";
            }

            var getNestedValue = function(xml, outerName, innerName) {
                var outerXml = xml.getElementsByTagName(outerName);
                if (outerXml && outerXml[0]) {
                    return getValue(outerXml[0], innerName);                        
                }

                return "";
            }

            var result = {};

            var answerXml = xmlDoc.getElementsByTagName("Answer");
            if (answerXml && answerXml[0]) {
                answerXml = answerXml[0];
                result.answers = {
                    ansValue: getValue(answerXml, "Value"),
                    ansGeography: getValue(answerXml, "Geography"),                                                                
                    prefix: getValue(answerXml, "Prefix"),
                    suffix: getValue(answerXml, "Suffix"),
                    ansLastUpdated: getValue(answerXml, "LastUpdated"),
                    ansDescription: getValue(answerXml, "Description"),
                    ansSystemDesc: getNestedValue(answerXml, "System", "Description"),
                    ansSystemURL: getNestedValue(answerXml, "System", "URL"),
                }
            }

            var resultXml = xmlDoc.getElementsByTagName("result");
            if (resultXml) {
                result.results = [];
                for (var i = 0; i < resultXml.length; i++) {
                    if (resultXml[i]) {
                        var resultXmlItem = resultXml[i];
                        var category = resultXmlItem.getAttribute("category");
                        var targetCategoryObj = undefined;
                        for (var j = 0; j < result.results.length; j++) {
                            if (result.results[j].category === category) {
                                targetCategoryObj = result.results[j];
                            }
                        }
                        if (targetCategoryObj === undefined) {
                            targetCategoryObj = {
                                category: category,
                                items: []
                            }
                            result.results.push(targetCategoryObj);
                        }
                        targetCategoryObj.items.push({
                            value: resultXmlItem.getAttribute("name")
                        })
                    }
                }

            }

            return (result.answers || result.results) ? result : undefined;
        }

        var callTypeaheadService = function(query) {

            // abort any pending requests
            window.hinterXHR.abort();

            window.hinterXHR.onreadystatechange = function() {
                if (this.readyState == 4 && this.status == 200) {

                    var response = window.hinterXHR.responseText;

                    var xmlDoc;
                    if (window.DOMParser)
                    {
                        var parser = new DOMParser();
                        xmlDoc = parser.parseFromString(response, "text/xml");
                    }
                    else // Internet Explorer
                    {
                        xmlDoc = new ActiveXObject("Microsoft.XMLDOM");
                        xmlDoc.async = false;
                        xmlDoc.loadXML(response);
                    }

                    var result = parseResponse(xmlDoc);

                    if (result) {
                        var typeaheadObj = document.getElementById("data-uscb-header-search-typeahead");
                        
                        typeaheadObj.innerHTML = "";
                        var showResults = false;
                        var typeaheadHtml = "";
                        
                        if (result.answers) {
                        	showResults = true;

                            var sysName = "";                
                            if (result.answers.ansLastUpdated && result.answers.ansLastUpdated != "") {
                                var d = new Date(result.answers.ansLastUpdated * 1000); // Given seconds originally
                                sysName = d.getFullYear() + " " + result.answers.ansSystemDesc;
                            } else {
                                sysName = result.answers.ansSystemDesc;
                            }

                            var query = document.getElementById("data-uscb-header-input").value;
                            
                            typeaheadHtml += 
                                '<div class="uscb-header-search-typeahead-answer">' 
                                    + '<a href="' + resultUrl + query + '">'
                                        + '<p class="uscb-header-search-typeahead-answer-geo">' + result.answers.ansGeography + " " + result.answers.ansDescription + '</p>'
                                        + '<p class="uscb-header-search-typeahead-answer-stat">' 
                                            + result.answers.prefix + result.answers.ansValue.replace(/(\d)(?=(\d\d\d)+(?!\d))/g, "$1,") + " " + result.answers.suffix 
                                        + '</p>'
                                    + '</a>'
                                    + '<a href="' + result.answers.ansSystemURL + '?cssp=Typeahead" class="uscb-header-search-typeahead-answer-source">Source: ' + sysName + '</a>'
                                + '</div>';

                        }

                        if (result.results && result.results.length > 0) {
                        	showResults = true;
                        	
                            typeaheadHtml += 
                                '<div class="uscb-header-search-typeahead-suggestions">'
                                    + '<ul>';

                                    for (var i = 0; i < result.results.length; i++) {
                                        var resultItem = result.results[i];
                                        typeaheadHtml += '<li>' + resultItem.category + '</li>'

                                        for (var j = 0; j < resultItem.items.length; j++) {
                                            var item = resultItem.items[j];

                                            typeaheadHtml += '<li><a href="' + resultUrl + item.value + '">' + item.value + '</a></li>'
                                        }
                                    }

                            typeaheadHtml += 
                                    '</ul>'
                                + '</div>';

                        }
                        
                        if (showResults) {
	                        typeaheadObj.innerHTML = typeaheadHtml;	                        
	                        typeaheadObj.style.display = "block";
                        }
                        
                    } else {
                        document.getElementById("data-uscb-header-search-typeahead").style.display = "none";
                    }
                    
                }
            };

            window.hinterXHR.open("GET", apiUri + query, true);
            window.hinterXHR.send()
        }

        // Autocomplete for form
        var hinter = function(event) {

            // retireve the input element
            var input = event.target;

            // minimum number of characters before we start to generate suggestions
            var min_characters = 3;

            if (input.value.length < min_characters ) { 
                document.getElementById("data-uscb-header-search-typeahead").style.display = "none";
                return;
            } else { 
               callTypeaheadService(input.value);
            }
        }

        searchInput.addEventListener("keyup", throttle(hinter));
        // so we can abort old requests when a new one is make
        window.hinterXHR = new XMLHttpRequest();
    	
    }
    
    var onSearchFocusBlur = function(active) {
        var searchIcon = document.getElementsByClassName("uscb-header-search-icon");
        if (searchIcon && searchIcon[0]) {
            searchIcon[0].style.display = active ? "none" : "block";
        }
        var searchButtons = document.getElementsByClassName("data-uscb-header-search-input-button");
        if (searchButtons) {
            for (var i = 0; i < searchButtons.length; i++) {
                searchButtons[i].style.display = active ? "block" : "none";
            }
        }
        
        if (typeof(document.getElementsByClassName("uscb-header-backdrop")) !== 'undefined' && 
        		document.getElementsByClassName("uscb-header-backdrop") != null && document.getElementsByClassName("uscb-header-backdrop").length > 0) {
        	document.getElementsByClassName("uscb-header-backdrop")[0].style.display = active ? "block" : "none";
        }
        
        if (!active && (typeof(document.getElementById("data-uscb-header-search-typeahead")) !== 'undefined' && 
        		document.getElementById("data-uscb-header-search-typeahead") != null && document.getElementById("data-uscb-header-search-typeahead").length > 0)) {
            // Don't activate unnecessarily, since search terms might not be present.
            document.getElementById("data-uscb-header-search-typeahead").style.display = "none";
        }

          // Close typeahead on blur if clicking outisde typeahead/search bar
        if (!active) {
            var typeahead = document.getElementById("data-uscb-header-search-typeahead");
            if ( typeahead ) {
                typeahead.style.display = "none";
            }
        }
    }
    
    var onSearchCloseClick = function() {
        document.getElementById("data-uscb-header-input").value = "";
        document.getElementById("data-uscb-header-search-typeahead").style.display = "none";
        
        onSearchFocusBlur(false);
    }

    var onSearchButtonClick = function() {
    	document.getElementById("data-uscb-header-search-form").submit();
    }
        
    return {
    	initSearchTypeahead: initSearchTypeahead,
    	onSearchFocusBlur: onSearchFocusBlur,
    	onSearchCloseClick: onSearchCloseClick,
    	onSearchButtonClick: onSearchButtonClick
    }

})(); // DO NOT INJECT FRAMEWORKS OR DEPENDENCIES THAT RELY ON FRAMEWORKS.
/*
 * This code is coupled with the Census main header HTL.
 *
 * It is framework-less and should remain that way for the foreseeable future to aid other, non-AEM census pages to utilize it.
 *
 * As such, it does not inject any dependencies, UNLESS the dependency is also dependency-less like CensusSearchTypeahead.
 *
 */
var CensusUniversalHeader = (function(CensusSearchTypeahead) {  // DO NOT INJECT FRAMEWORKS OR DEPENDENCIES THAT RELY ON FRAMEWORKS.

    var initHeader = function() {}

    var closeDropdowns = function(idPrefix, navItemIdPrefix, navItemLinkIdPrefix, navItemLength, skipIndex) {
        for (var i = 0; i < navItemLength; i++) {
            if (skipIndex !== undefined && i === skipIndex) {
                continue;
            }
            var other = document.getElementById(idPrefix + i);
            if (other) {
                other.style.display = "none";
            }

            var otherNavItemTarget = document.getElementById(navItemIdPrefix + i);
            otherNavItemTarget.classList.remove("uscb-nav-item-hover");

            var otherNavItemLinkTarget = document.getElementById(navItemLinkIdPrefix + i);
            otherNavItemLinkTarget.classList.remove("uscb-nav-item-link-hover");
        }

        setMenuTabOrders(0);
    }

    var onActivateMenu = function(active, idPrefix, navItemIdPrefix, navItemLinkIdPrefix, index, navItemLength) {
        if (active === true) {
        	CensusSearchTypeahead.onSearchFocusBlur(false);

            var target = document.getElementById(idPrefix + index);

            // Show targetted dropdown
            if (target && target.style.display === "none") {
                target.style.display = "flex";

                var navItemTarget = document.getElementById(navItemIdPrefix + index);
                navItemTarget.className += " uscb-nav-item-hover";

                var navItemLinkTarget = document.getElementById(navItemLinkIdPrefix + index);
                navItemLinkTarget.className += " uscb-nav-item-link-hover";
            }

            // Close all other dropdowns
            closeDropdowns(idPrefix, navItemIdPrefix, navItemLinkIdPrefix, navItemLength, index);
        } else if (active === false) {
            // Close ALL dropdowns
            closeDropdowns(idPrefix, navItemIdPrefix, navItemLinkIdPrefix, navItemLength);
        } else if (active === "toggle") {
            var target = document.getElementById(idPrefix + index);
            if (target) {
                if (target.style.display === "none") {
                    window.headerOpenIndex = index;
                    onActivateMenu(true, idPrefix, navItemIdPrefix, navItemLinkIdPrefix, index, navItemLength);
                } else if (target.style.display === "flex") {
                    closeDropdowns(idPrefix, navItemIdPrefix, navItemLinkIdPrefix, navItemLength);
                    if (index !== window.headerOpenIndex) {
                        window.headerOpenIndex = index;
                        onActivateMenu(true, idPrefix, navItemIdPrefix, navItemLinkIdPrefix, index, navItemLength);
                    }
                }
            }
        }
    }

    var onHoverLeave = function(idPrefix, navItemIdPrefix, navItemLinkIdPrefix, index, navItemLength) {
        closeDropdowns(idPrefix, navItemIdPrefix, navItemLinkIdPrefix, navItemLength);
    }

    var toggleMenu = function(idPrefix, navItemIdPrefix, navItemLinkIdPrefix, firstExpandedIndex, navItemLength) {
        var header = document.getElementById("data-uscb-main-header");
        var headerMenu = document.getElementById("data-uscb-header-menu");
        var headerMenuButton = document.getElementsByClassName("uscb-menu-icon-container")[0];
        var closeText = document.getElementById("data-uscb-header-close");
        if (headerMenu.classList.contains("uscb-hide-md")) {
            // Open
            header.classList.add("uscb-header-fixed");
            headerMenu.classList.remove("uscb-hide-md");
            headerMenu.classList.add("uscb-show-md");
            headerMenuButton.classList.add("uscb-menu-change");
            headerMenuButton.style.width = "90px";
            closeText.style.display = "block";

            onActivateMenu(true, idPrefix, navItemIdPrefix, navItemLinkIdPrefix, firstExpandedIndex, navItemLength);
        } else if (headerMenu.classList.contains("uscb-show-md")) {
            // Close
            header.classList.remove("uscb-header-fixed");
            headerMenu.classList.remove("uscb-show-md");
            headerMenu.classList.add("uscb-hide-md");
            headerMenuButton.classList.remove("uscb-menu-change");
            headerMenuButton.style.width = "auto";
            closeText.style.display = "none";
        }
    }

    var onKeyParent = function(event, idPrefix, index) {
    	if (event.keyCode === 9 && !event.shiftKey) {
            setMenuTabOrders(-1);
        }
    }

		var onKeyChildLast = function(event, parentMenuId) {
    	if (event.keyCode === 9 && !event.shiftKey) {
            setMenuTabOrders(0);
            if ( !isLastHeaderItem( parentMenuId ) ) {
                var target = document.getElementById(parentMenuId);
                if (target) {
                    target.focus();
                    event.stopPropagation();
                }
            }
    	}
    }

    var onKeyChildFirst = function(event, parentMenuId) {
    	if (event.keyCode === 9 && event.shiftKey) {
            setMenuTabOrders(0);
            var target = document.getElementById(parentMenuId);
            if (target) {
                target.focus();
                event.stopPropagation();
            }
        }
    }

    function isLastHeaderItem( parentMenuId ) {
        var parentMenuItem = document.querySelector( '.uscb-header-menu #' + parentMenuId );
        var menuItems = document.querySelectorAll( '.uscb-header-menu a[id*="data-uscb-header-nav-item-"]' );
        return parentMenuItem === menuItems[ menuItems.length - 1 ];
    }

    function setMenuTabOrders(order) {
        var menuItems = document.querySelectorAll( '.uscb-header-menu a[id*="data-uscb-header-nav-item-"]' );
        if ( menuItems ) {
              // Old for loop type to support IE
            for ( var i = 0; i < menuItems.length; i++ ) {
                menuItems[i].setAttribute('tabindex', order);
            }
        }
    }

    return {
			initHeader: initHeader,
    	onActivateMenu: onActivateMenu,
    	closeDropdowns: closeDropdowns,
    	onHoverLeave: onHoverLeave,
    	toggleMenu: toggleMenu,
    	onKeyParent: onKeyParent,
    	onKeyChildLast : onKeyChildLast,
      onKeyChildFirst : onKeyChildFirst
    }

})(CensusSearchTypeahead); // DO NOT INJECT FRAMEWORKS OR DEPENDENCIES THAT RELY ON FRAMEWORKS.

