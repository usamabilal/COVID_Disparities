/*Functions used for Analytics*/
var curHost=  "http://"+ window.location.host;
var govDomainList = new Array(
	'http://www.ask.census.gov',
	'https://www.census.gov',
	'http://census.gov', 
	'https://www.census.gov', 
	'https://census.gov',
	'http://www.commerce.gov',
	'http://commerce.gov',
	'https://census.csod.com/ats/careersite/search.aspx?site=1&c=census',
	'https://census.csod.com/ats/careersite/LoginLite.aspx?c=census&site=1',
	curHost
);

var fileExtList = new Array(
	'pdf', 'jpg', 'jpeg', 'mp3', 'mp4', 'gif', 'gz', 'doc', 'dot', 'gdoc', 'odt', 'png', 'tiff', 'nef', 'ai',
	'orf', 'indd', 'eps', 'cr2', 'psd', 'pcm', 'wav', 'aiff', 'aac', 'ogg', 'wma', 'flac', 'alac', 'm4a', 'm4v',
	'f4v', 'f4a', 'm4b', 'm4r', 'f4b', 'mov', '3gp', '3gp2', '3g2', '3gpp', '3gpp2', 'oga', 'ogv', 'ogx', 'wmv',
	'asf', 'webm', 'avi', 'quicktime', 'hdv', 'mxf', 'vob', 'lxf', 'mpeg-ts', 'mpeg-2 ps', 'mpeg-2 ts', 'gxf',
	'he-aac', 'ac3', 'eac3', 'vorbis', 'pcm', 'tiff', 'docx', 'xls', 'xlsx', 'ods', 'ppt', 'pptx', 'txt', 'docbook',
	'rtf', 'tei', 'troff', 'wpd', 'wp', 'wp7', 'jfif', 'jpeg 2000', 'exif', 'bmp', 'ppm', 'pgm', 'pbm', 'pnm', 'webp',
	'hdr', 'heif', 'bat', 'bpg', 'cgm', 'rs-274x', 'svg', 'ico', 'img', 'pcx', 'pam', 'pgf', 'sgi'
);

function isInternalLink(URL) {	
    var domainRexp = new RegExp('.*\\.gov');
  	var foundPresent = govDomainList.indexOf(URL);  	
  	var found = false;
  	
	for(var i = 0; i < govDomainList.length; i++) {
		if(domainRexp.test(URL) ) {
	        found = true;
	        break;      
		}
		  
		if(-1 !== parseInt(foundPresent)) {
	        found = true;
	        break;
	    }       
	}//end for...loop
	  
	return found;
}

function isDownloadLink(URL) {
	var found = false;
	var res = URL.split("/");
	var fileName = res[res.length - 1];	
	var splittedVal = fileName.split(".");
	var ext = splittedVal[1];
	
	if (typeof(ext) !== 'undefined') {
		var cleanExt = ext.replace(/[^a-zA-Z0-9]/g, '');	
	
		for (var i = 0; i < fileExtList.length; i++) {
			if (fileExtList[i] == cleanExt.toLowerCase()) {
				found = true;
				break;
			}
		}//end for...loop
	}
	
	return found;
}

function isExitLink(linkHref) {	
	var isExit = true;
	if (typeof(linkHref) !== 'undefined') {
		if (linkHref.indexOf("census") > -1 || 
			linkHref.indexOf("census.gov") > -1 || 
			linkHref.indexOf("2020census") > -1 ||
			linkHref.indexOf("2020Census") > -1 || 
			linkHref.indexOf("2020-census") > -1 || 
			linkHref.indexOf("2020-Census") > -1) {
			isExit = false;
		}
		
		return isExit;
	} else {
		return null;
	}
}

function bannerAlertLinkClick(elem, hrefVal) {
	digitalData.event.eventInfo.eventName = 'Component Click';
	digitalData.event.eventInfo.textInfo = $(elem).text().trim();
	digitalData.event.eventInfo.componentName = 'Banner Alert Component';
	
	if (isExitLink(hrefVal)) {
		digitalData.event.eventInfo.eventName = 'Exit Link';
		digitalData.event.eventInfo.exitURL = hrefVal; 
	} else {
		digitalData.event.eventInfo.exitURL = null;
	}
}

function linkClick(elem, componentName) {
	digitalData.event.eventInfo.eventName = 'Component Click';
	if ('Census List Component' === componentName) {
		digitalData.event.eventInfo.textInfo = $(elem).attr('title');
	} else {
		digitalData.event.eventInfo.textInfo = $(elem).text().trim();
	}
	digitalData.event.eventInfo.componentName = componentName;
	var linkHref = $(elem).attr('href');
	
	if (isExitLink(linkHref)) {
		digitalData.event.eventInfo.eventName = 'Exit Link';
		digitalData.event.eventInfo.exitURL = linkHref; 
	} else {
		digitalData.event.eventInfo.exitURL = null;
	}
	dataLayerDownloadHandler(elem);
}

function bannerAlertCloseButtonClick() {
	digitalData.event.eventInfo.eventName = 'Banner Alert Close Button Click';
}

function navigationBLinkClick(elem, componentName, location) {
	var topLinkLabel = $(elem).text().trim();
	digitalData.event.eventInfo.eventName = 'Navigation Click';
	digitalData.event.eventInfo.mainNav = topLinkLabel;
	digitalData.event.eventInfo.navLocation = location;
	digitalData.event.eventInfo.componentName = componentName;
}

function navigationLinkClick(elem, componentName, location, index) {	
	var topLinkLabel = $('#data-uscb-header-nav-item-' + index).find('.data-uscb-top-link').text().trim();
	var subLinkLabel = $(elem).text().trim();
	digitalData.event.eventInfo.eventName = 'Navigation Click';
	digitalData.event.eventInfo.mainNav = topLinkLabel;
	digitalData.event.eventInfo.subNav = subLinkLabel;
	digitalData.event.eventInfo.navLocation = location;
	digitalData.event.eventInfo.componentName = componentName;
}

function leftNavLinkClick(elem) {
	var topLinkLabel = $('.data-uscb-breadcrumb-last-element').text().trim();
	var subLinkLabel = $(elem).find('span').text().trim();
	digitalData.event.eventInfo.eventName = 'Navigation Click';
	digitalData.event.eventInfo.mainNav = topLinkLabel;
	digitalData.event.eventInfo.subNav = subLinkLabel;
	digitalData.event.eventInfo.navLocation = 'Left';
	digitalData.event.eventInfo.componentName = 'Left Navigation';
}

function dataLayerDownloadHandler(elem) {	
	if (typeof(elem) !== 'undefined' && typeof($(elem).attr('href')) !== 'undefined') {		
		var res = $(elem).attr('href').split("/");
		if (typeof($(elem).attr('filetrack')) != 'undefined') {
		    digitalData.event.eventInfo.download.fileUrl = $(elem).attr('filetrack');
		    digitalData.event.eventInfo.download.fileName = res[res.length - 1];
		    digitalData.event.eventName = 'Download';		    
		    digitalData.event.eventInfo.eventName = 'Download';
		    digitalData.event.eventInfo.downloadURL = $(elem).attr('filetrack');
		    digitalData.event.eventInfo.downloadFile = res[res.length - 1];		    
		} else {			
			if (isDownloadLink($(elem).attr('href'))) {
				var fileName = res[res.length - 1];
				var splittedVal = fileName.split(".");
				var ext = splittedVal[1];
				var cleanExt = ext.replace(/[^a-zA-Z0-9]/g, '');
				var downloadFile = splittedVal[0] + "." + cleanExt;
				digitalData.event.eventInfo.downloadURL = $(elem).attr('href');
				digitalData.event.eventInfo.downloadFile = downloadFile
			}
		}
	} else {
		digitalData.event.eventInfo.download.fileUrl = null;
	    digitalData.event.eventInfo.download.fileName = null;
	    digitalData.event.eventName = null;
	    digitalData.event.eventInfo.eventName = null;
	    digitalData.event.eventInfo.downloadURL = null;
	    digitalData.event.eventInfo.downloadFile = null;
	}
}

function buttonClick(elem, btnName, btnLocation) {	
	dataLayerDownloadHandler(elem);				
	var componentName = 'Button Component';
	var textValue = $(elem).text().trim();			 
	var linkType = isInternalLink($(elem).attr('href')) ? 'Internal' : 'External';			
	digitalData.event.eventName = 'Button Click';
	digitalData.event.eventInfo.eventName = 'Button Click';
	digitalData.event.eventInfo.componentName = componentName;
	digitalData.event.eventInfo.textValue = textValue;
	digitalData.event.eventInfo.linkType = linkType;
	digitalData.event.eventInfo.buttonComponent = btnName;
	digitalData.event.eventInfo.buttonLocation = btnLocation;
	digitalData.event.eventInfo.buttonLabel = textValue;
	digitalData.event.eventInfo.buttonTarget = linkType;
	
	var latestCount = 0;
	if ( typeof(Storage) !== "undefined" ) {		
		latestCount = localStorage.getItem("buttonClickCount");
		latestCount++;
		localStorage.setItem("buttonClickCount", latestCount);
		digitalData.event.eventInfo.buttonClickCount = latestCount;
	} else {
		latestCount = getCookie("buttonClickCount");
		latestCount++;
		setCookie("buttonClickCount", latestCount);
		digitalData.event.eventInfo.buttonClickCount = latestCount;
	}
	dataLayerDownloadHandler(elem);
 }

function searchResultLinkClick(elem, componentName) {
	digitalData.event.eventInfo.eventName = 'Search Result Click';
	digitalData.event.eventInfo.searchResultName = $(elem).attr('href');
	digitalData.event.eventInfo.search.clickValue = 'ORG:' + $(elem).attr('href');
}

function shareThisClick(elem) {
	var title = $(elem).attr('title');
	
	if (title.toLowerCase() === "download") {
		var res = null;
		if (typeof($(elem).attr('id')) !== 'undefined') {
			res = $(elem).attr('id').split("/");
		} else {
			res = $(elem).attr('href').split("/");
		}
		
		if (isDownloadLink(res[res.length - 1])) {
			var fileName = res[res.length - 1];
			var splittedVal = fileName.split(".");
			var ext = splittedVal[1];
			var cleanExt = ext.replace(/[^a-zA-Z0-9]/g, '');
			var downloadFile = splittedVal[0] + "." + cleanExt;
			digitalData.event.eventInfo.download.fileUrl = res;
		    digitalData.event.eventInfo.download.fileName = downloadFile;
		    digitalData.event.eventName = 'Download';		    
		    digitalData.event.eventInfo.eventName = 'Download';
		    digitalData.event.eventInfo.downloadURL = res;
		    digitalData.event.eventInfo.downloadFile = downloadFile
		}
	} else {	
		digitalData.event.eventInfo.eventName = 'Social Share';
		digitalData.event.eventInfo.socialType = title;
	}
}

function blogSearchFormSubmit(elem) {	
	var inputValue = $(elem).find('#livesearch').val();
	if ('Search this blog' !== inputValue) {
		if (typeof(Storage) !== "undefined") {
			localStorage.setItem("blogSearchPhrase", inputValue);
			localStorage.setItem("blogEventName", "Blog Search");			
			var breadCrumbLastElement = $('.data-uscb-breadcrumb-last-element').text();			
			if (typeof(breadCrumbLastElement) !== "undefined") {
				localStorage.setItem("blogName", breadCrumbLastElement);
			}			
		} else{
			setCookie("blogSearchPhrase", inputValue);
			setCookie("blogEventName", "Blog Search");
			var breadCrumbLastElement = $('.data-uscb-breadcrumb-last-element').text();			
			if (typeof(breadCrumbLastElement) !== "undefined") {
				setCookie("blogName", breadCrumbLastElement);
			}
		}
	}
}