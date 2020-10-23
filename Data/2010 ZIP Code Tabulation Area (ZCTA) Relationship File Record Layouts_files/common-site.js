var cfSchemaConfig = (function() {
    
    function init( modelPath ) {
        if ( modelPath.endsWith( 'faq' ) ) {
            generateFAQSchema();
        }
    }

    function generateFAQSchema() {
        var $head = $( 'head' );
        var $faqSchema = $head.find( '#faqSchema' );

        var currSchema = {};
        if ( $faqSchema.get(0) ) {
            currSchema = $faqSchema.text().toJSON();
            $faqSchema.remove();
        } else {
            currSchema = {
                '@context': 'https://schema.org',
                '@type': 'FAQPage',
                'mainEntity': []
            };
        }

        var currFAQs = getFAQsFromComponent();
        currFAQs.forEach( function( currFAQ ) {
            currSchema.mainEntity.push( buildFAQEntry( currFAQ.question, currFAQ.answer ) );
        });

        var $faqScript = $( document.createElement( 'script' ) );
        $faqScript.attr( 'type', 'application/ld+json' );
        $faqScript.attr( 'id', 'faqSchema' );
        $faqScript.text( JSON.stringify( currSchema ) );

        $head.append( $faqScript );
    }

      // Returns QA data from current component
    function getFAQsFromComponent() {
        var $questions = $( '.cmp-contentfragment__title' );
        var $answers = $( '.cmp-contentfragment__element-value' );

        var faqArray = [];
        $questions.each( function( idx ) {
            faqArray.push( {
                question: $questions.get( idx ).innerText.trim(),
                answer: $answers.get( idx ).innerText.trim()
            });
        });

        return faqArray;
    }

      // Returns QA data in form formatted to schema
    function buildFAQEntry( question, answer ) {
        return {
            '@type': 'Question',
            'name': question,
            'acceptedAnswer': {
                '@type': 'Answer',
                'text': answer
            }
        }
    }

    return {
        init: init
    };
})();
var CensusDropdown = (function() {
    'use strict';

    var lazyCallbacks = {};

      // Adds event listener to dropdown button. Activates any callbacks upon click.
    function addListener( idRoot ) {
        var trigger = document.getElementById( "dd" + idRoot + "-tr" );
        if ( trigger ) {
            trigger.addEventListener("click", function() {
                var container = $( "#dd" + idRoot + "-co" );
                if ( container.length > 0 ) {
                    container.toggleClass( 'uscb-hide' );
                    $( trigger ).toggleClass( 'uscb-dropdown-open' );
                    
                      // Only activate callbacks on dropdown open
                    if ( !container.hasClass( 'uscb-hide' ) ) {
                        activateCallbacks( idRoot );
                    }
                }
            });
        }
    }

      // Registers new callback to list
        // Because IE doesn't support the spread operator, all callbacks must be made to support all arguments as a single array
    function registerCallback( compId, callback, argList ) {
        var dropdownId = isChildOfDropdown( compId );
        if ( dropdownId ) {
            var callbackObj = {
                fn: callback,
                args: argList
            }
            if ( !lazyCallbacks[ dropdownId ] || lazyCallbacks[ dropdownId ].length === 0 ) {
                lazyCallbacks[ dropdownId ] = [ callbackObj ];
            } else {
                lazyCallbacks[ dropdownId ].push( callbackObj );
            }
        }
    }

      // Checks if given ID is child of a dropdown component. Returns that dropdown's ID if true, otherwise undefined
    function isChildOfDropdown( compId ) {
        var component = $('#' + compId );
        if ( component && component.length > 0 ) {
            var parentDropdown = component.closest( '.uscb-dropdown' );
            if ( parentDropdown && parentDropdown.length > 0 ) {
                return parentDropdown[0].id;
            }
        }
    }

      // Finds all callbacks keyed to the given ID and activates them
    function activateCallbacks( idRoot ) {
        var callbackList = lazyCallbacks[ idRoot ];

        if ( callbackList && callbackList.length > 0 ) {
            callbackList.forEach( function( callbackObj ) {
                if ( callbackObj.args ) {
                    callbackObj.fn( callbackObj.args );
                } else {
                    callbackObj.fn();
                }
            });
        }
    }

    return {
        addListener: addListener,
        registerCallback: registerCallback,
        isChildOfDropdown: isChildOfDropdown
    }; 
})();
var CensusLinkList = (function() {
    'use strict';
    
    var targets = {};

      // Subscribes to dropdown's callback service if list is child of dropdown. Otherwise immediately calls for dynamic data.
    function subscribe( idRoot, nodePath, immediateActivation ) {
        var dropdownId = CensusDropdown.isChildOfDropdown( idRoot );
        if ( immediateActivation || !dropdownId ) {
            activate( [ idRoot, nodePath ] );
        } else {
            CensusDropdown.registerCallback( idRoot, activate, [ idRoot, nodePath ] );
        }
    }

      // Adds to list map of component IDs and targets to be added to generated links
    function registerTarget( idRoot, target ) {
        if ( idRoot && target ) {
            targets[ idRoot ] = target;
        }
    }

      // Calls AEM backend for dynamic list data. Servlet will ignore call if list is set to manual.
    function activate( argList ) {
        var parentElement = $( '#' + argList[0] );
        if ( parentElement && parentElement.length > 0 ) {
            var listItems = parentElement.find( '.uscb-multi-col-list-item' );
            if ( listItems && listItems.length === 0 ) {
                $.ajax({
                    url: argList[1] + ".lazy.html",
                    dataType: 'html'
                }).done( function(data) {
                    buildDynamicList( argList[0], data );
                });
            }
        }
    }

      // Parse raw list and build HTML for each item then place in page
    function buildDynamicList( idRoot, list ) {
        var parsedList = JSON.parse( list );

        var listHtml = '';
        parsedList.forEach( function( listItem ) {
            listHtml += buildListItem( listItem.url, targets[ idRoot ], listItem.altText, listItem.label );
        });

        var parentElement = $( '#' + idRoot );
        if ( parentElement && parentElement.length > 0 ) {
            var listContainer = parentElement.find( '.uscb-link-list' );
            if ( listContainer && listContainer.length > 0 ) {
                listContainer.html( listHtml );
            }
        }
    }

      // Constructs a list item
    function buildListItem( url, target, altText, label ) {
        return "<li class='uscb-multi-col-list-item'>" + 
                   "<a href='" + url + "' target='" + target + "' title='" + altText + "'>" + label + "</a>" +
               "</li>";
    }

    return {
        subscribe: subscribe,
        registerTarget: registerTarget
    }
})();
(function() {
    document.addEventListener( "DOMContentLoaded", function() {
        var headerSkipNav = document.getElementById( "uscb-nav-skip-header" );
        var localSkipNav = document.getElementById( "uscb-nav-skip-local" );

        if ( localSkipNav ) {
              // Update header skip nav link
            headerSkipNav.href = "#uscb-nav-skip-local";

              // Look up a maximum of 4 levels for a next sibling and apply the content ID to it
            var localNav = localSkipNav.parentElement;
            var nextSibling = localNav.nextElementSibling;
            if ( !nextSibling || nextSibling.className.indexOf( 'sponsorlogo' ) !== -1 ) {
                localNav = localNav.parentElement;
                nextSibling = localNav.nextElementSibling;

                if ( !nextSibling || nextSibling.className.indexOf( 'sponsorlogo' ) !== -1 ) {
                    localNav = localNav.parentElement;
                    nextSibling = localNav.nextElementSibling;

                    if ( !nextSibling || nextSibling.className.indexOf( 'sponsorlogo' ) !== -1 ) {
                        localNav = localNav.parentElement;
                        nextSibling = localNav.nextElementSibling;
                    }
                }
            }

            if ( nextSibling ) {
                nextSibling.id = "content";
            }

        } else {
              // Add content ID to next component from header depending on page structure
            var headerContainer;
            if ( document.body.id === 'innerPage' ) {
                headerContainer = document.body.getElementsByClassName('uscb-main-container');
                headerContainer = headerContainer ? headerContainer[0] : undefined;
            } else {
                headerContainer = document.getElementsByClassName('universalheader');
                headerContainer = headerContainer ? headerContainer[0] : undefined;
            }

            if ( headerContainer ) {
                var nextSibling = headerContainer.nextElementSibling;
                while ( nextSibling && window.getComputedStyle(nextSibling).display === 'none' ) {
                    nextSibling = nextSibling.nextElementSibling;
                }
                if ( nextSibling ) {
                    if ( nextSibling.id ) {
                        var skipLink = document.getElementsByClassName('uscb-header-nav-skip');
                        if ( skipLink && skipLink[0] ) {
                            skipLink[0].setAttribute('href', "#" + nextSibling.id);
                        }
                    } else {
                        nextSibling.id = 'content';
                    }
                }
            }
        }
    });
})();
$( document ).ready( function() {
    resizeEmbeds();

    $( window ).resize( function(){
        resizeEmbeds();
    });
});

function resizeEmbeds() {
    var allEmbedCores = $( '.cmp-embed' );
    allEmbedCores.each( function( emdIdx, embed ) {
        var parentContainerWidth = $( embed ).width();
        var childIframe = $( embed ).find( 'iframe' );
        if ( childIframe.length !== 0 ) {
            var aspectRatio = childIframe.height() / childIframe.width();

            childIframe.each( function( ifrIdx, iframe ) {
                $( iframe ).attr( 'width', parentContainerWidth );
                $( iframe ).attr( 'height', $( iframe ).width() * aspectRatio );
            });
        }
    });
}
