
function arrangeFixedGridLayout(id) {
	var fixedGridObj = $("#" + id);
	if (fixedGridObj) {
		
		var targetObj = fixedGridObj.find(".uscb-fixed-grid-R");
		var mobileTarget = fixedGridObj.find(".data-uscb-fixed-grid-target-mobile");
		var desktopTarget = fixedGridObj.find(".data-uscb-fixed-grid-target-desktop");
		
		if (Modernizr.mq('only screen and (max-width : 768px)')) {
			<!--/* Mobile */-->
			targetObj.insertAfter(mobileTarget);
			
		} else {
			<!--/* Desktop */-->
			targetObj.insertAfter(desktopTarget);
		}			
	}
}