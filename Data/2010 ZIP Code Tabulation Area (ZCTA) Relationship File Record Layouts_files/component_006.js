var CensusCarousel = (function($, _) {
	
	var carousels = [];
    
    var initCarousel = function(id, items) {
    	var carouselObj = _.find(carousels, {"id": "" + id + ""});
    	if (!carouselObj) {
    		carouselObj = {
    			id: id,
    			carouselItems: items,
    			currentObjectIndex: 0,
    			currentObject: items[0]
    		}
    		carousels.push(carouselObj);
    	}
    	
    	var $carousel = $("#" + id);

    	$($carousel.find(".uscb-dot-spec")[carouselObj.currentObjectIndex]).addClass("uscb-active-carousel");
        $($carousel.find(".uscb-carousel-spec-mini")[carouselObj.currentObjectIndex]).addClass("uscb-active-carousel-special");
        
        updateMediaContainer($carousel, carouselObj);
        
        $carousel.find(".uscb-next-spec").click(function () {
        	var carousel = findParentCarousel($(this));
        	
            if (carousel.carouselObj.currentObjectIndex === carousel.carouselObj.carouselItems.length - 1) {
            	carousel.carouselObj.currentObjectIndex = 0;
            } else {
            	carousel.carouselObj.currentObjectIndex++;
            }
            
            updateCarousel(carousel.$carousel, carousel.carouselObj);
        });     
        
        $carousel.find(".uscb-prev-spec").click(function () {
        	var carousel = findParentCarousel($(this));
        	
            if (carousel.carouselObj.currentObjectIndex === 0) {
            	carousel.carouselObj.currentObjectIndex = carousel.carouselObj.carouselItems.length - 1;
            } else {
            	carousel.carouselObj.currentObjectIndex--;               
            }
            
            updateCarousel(carousel.$carousel, carousel.carouselObj); 
        });   
        
        $carousel.find("div[id^='data-uscb-mini-" + id + "-']").click(function () {
        	var carousel = findParentCarousel($(this));
        	
        	var miniId = $(this).attr("id");
        	var indexId = miniId.replace("data-uscb-mini-" + id + "-", "");
        	
        	var index = parseInt(indexId);    	
        	carousel.carouselObj.currentObjectIndex = index;
        	
        	updateCarousel(carousel.$carousel, carousel.carouselObj);
        });
        
        function findParentCarousel($child) {
        	var $carousel = $child.closest(".uscb-carousel");
        	var id = $carousel.attr("id");        	
        	var carouselObj = _.find(carousels, {"id": "" + id + ""});
        	
        	return {
        		$carousel: $carousel,
        		carouselObj: carouselObj,
        		id: id
    		};
        }
        
        function updateCarousel($carousel, carouselObj) {
        	carouselObj.currentObject = carouselObj.carouselItems[carouselObj.currentObjectIndex];
            
            updateMediaContainer($carousel, carouselObj);
            //Make sure the currentObj.title and teaser are empty b/c the next items may not have value...
            $carousel.find(".uscb-spec-header").text("");
            $carousel.find(".uscb-spec-desc").text("");
            //////
            $carousel.find(".uscb-spec-header").text(carouselObj.currentObject.title);
            $carousel.find(".uscb-spec-desc").html(carouselObj.currentObject.teaser);
            
            // remove active class
            $carousel.find(".uscb-carousel-spec-mini").each(function (index, element) {
                $(element).removeClass("uscb-active-carousel-special");
            })

            $($carousel.find(".uscb-carousel-spec-mini")[carouselObj.currentObjectIndex]).addClass("uscb-active-carousel-special");

            // dots used on special container
            // remove active class
            $carousel.find(".uscb-dot-spec").each(function (index, element) {
                $(element).removeClass("uscb-active-carousel");
            })
            $($carousel.find(".uscb-dot-spec")[carouselObj.currentObjectIndex]).addClass("uscb-active-carousel");
        	
        }

        function updateMediaContainer($carousel, carouselObj) {
	        $mediaContainer = $carousel.find(".data-uscb-carousel-media-container");
	        if (carouselObj.currentObject.mediaType.toLowerCase() === 'image') {
	        	$mediaContainer.empty();
	        		        
	        	var mediaContainerMarkup = "";
	        	if (carouselObj.currentObject.ctaUrl) {
	        		mediaContainerMarkup += '<a href="' + carouselObj.currentObject.ctaUrl + '" class="data-uscb-carousel-cta-link uscb-text-decoration-none">';	
	        	} 	        	
	        	
	        	mediaContainerMarkup += '<div class="uscb-carousel-spec-img">';
				mediaContainerMarkup += '<img src="' + carouselObj.currentObject.image + '" id="uscb-carousel-img-spec" alt="">';
				mediaContainerMarkup += '</div>';
	        	
	        	if (carouselObj.currentObject.ctaUrl) {
	        		mediaContainerMarkup += '</a>';	        	
	        	}
	        	
	        	$mediaContainer.append(mediaContainerMarkup);

	        	$carousel.find(".data-uscb-carousel-cta-link").attr("href", carouselObj.currentObject.ctaUrl);
                
                if (carouselObj.currentObject.ctaText) {
                	$carousel.find(".data-uscb-spec-button").show();
                	$carousel.find(".data-uscb-spec-button").text(carouselObj.currentObject.ctaText);
                } else {
                	if (carouselObj.currentObject.ctaUrl) {
                		$carousel.find(".data-uscb-spec-button").show();
                		$carousel.find(".data-uscb-spec-button").text("View More");            		
                	} else {
                		$carousel.find(".data-uscb-spec-button").hide();
                	}
                }
	        	
	        } else if (carouselObj.currentObject.mediaType.toLowerCase() === 'video') {
	        	$mediaContainer.empty();
	        	
	        	var mediaContainerMarkup = 
	    			"<iframe src='//www.youtube.com/embed/" + carouselObj.currentObject.video + "?wmode=opaque&rel=0' height='100%' width='100%' frameborder='0' allowfullscreen>Loading Video...</iframe>";
	        	$mediaContainer.append(mediaContainerMarkup);

	        	$carousel.find(".data-uscb-carousel-cta-link").attr("href", "");
	        	$carousel.find(".data-uscb-spec-button").hide();
	        }
        }
    }
    
    return {
        initCarousel: initCarousel
    }

})(jQuery, window._);


        