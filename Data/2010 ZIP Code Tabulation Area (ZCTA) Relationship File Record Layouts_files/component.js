function adjustResponsiveIFrame(id, url) {
	var riObj = $("#" + id);
	if (riObj) {
		var containerWidth = riObj.parent().width();		
		riObj.width(containerWidth);

		riObj.attr("src", url);		
	}		
}
