// Define a globally available JS function that will modify sidebar width according to user input
Shiny.addCustomMessageHandler('sidebarWidth', function(width) {
  document.querySelector('.bslib-sidebar-layout').style.setProperty('--_sidebar-width', width + 'px');
});

// Define a globally available JS function that will show NA in datatables
function customRowCallback(row, data) {
  for(var i=0; i<data.length; i++){
    if(data[i] === null || data[i] === 'NA'){  // Check for null or string "NA"
      $('td:eq('+i+')', row).html('NA')
        .css({'color': 'rgb(151,151,151)', 'font-style': 'italic'});
    }
  }
}



/* the follow was an attempt to add a show more feature for long questions texts. Didnt make it work in reasonable time due to issues with reactivity i guess. Ill keep it in to try again later
function addShowMore() {
    var showChar = 100;
    var ellipsestext = "...";
    var moretext = "Show more >";
    var lesstext = "Show less";

    $(".more").each(function() {
        var content = $(this).text();
        if(content.length > showChar) {
            var c = content.substr(0, showChar);
            var h = content.substr(showChar);
            var html = c + "<span class='moreellipses'>" + ellipsestext
              + "&nbsp;</span><span class='morecontent'><span>" + h
              + "</span>&nbsp;&nbsp;<a href='#' class='morelink'>" + moretext + "</a></span>";
            $(this).html(html);
        }
    });

    $(".morelink").off("click").on("click", function(e) {
        e.preventDefault();
        if($(this).hasClass("less")) {
            $(this).removeClass("less");
            $(this).html(moretext);
        } else {
            $(this).addClass("less");
            $(this).html(lesstext);
        }
        $(this).parent().prev().toggle();
        $(this).prev().toggle();
    });
}

// Run once on document ready in case content is static
$(document).ready(function() {
    addShowMore();
});

// Register custom message handler to rerun truncation from Shiny
Shiny.addCustomMessageHandler("addShowMore", function(message) {
    addShowMore();
});







// js code for "show more/less" feature.
$(document).ready(function() {
    // Configure/customize these variables.
    var showChar = 100;  // How many characters are shown by default
    var ellipsestext = "...";
    var moretext = "Show more >";
    var lesstext = "Show less";

    $(".more").each(function() {
        var content = $(this).html();
        if(content.length > showChar) {
            var c = content.substr(0, showChar);
            var h = content.substr(showChar, content.length - showChar);
            var html = c + "<span class=\\\"moreellipses\\\">" + ellipsestext
              + "&nbsp;</span><span class=\\\"morecontent\\\"><span>" + h
              + "</span>&nbsp;&nbsp;<a href=\\\"\\\" class=\\\"morelink\\\">"
              + moretext + "</a></span>";
            $(this).html(html);
        }
    });

    $(".morelink").click(function(){
        if($(this).hasClass("less")) {
            $(this).removeClass("less");
            $(this).html(moretext);
        } else {
            $(this).addClass("less");
            $(this).html(lesstext);
        }
        $(this).parent().prev().toggle();
        $(this).prev().toggle();
        return false;
    });
});


function addShowMore() {
  var showChar = 100;
  var ellipsestext = "...";
  var moretext = "Show more";
  var lesstext = "Show less";

  $('.more').each(function() {
    var fullHtml = $(this).html();
    var fullText = $(this).text();

    if(fullText.length > showChar) {
      var c = fullText.substr(0, showChar);
      var h = fullText.substr(showChar);

      var html = c + '<span class="moreellipses">' + ellipsestext + '&nbsp;</span>' +
                 '<span class="morecontent"><span>' + h + '</span>&nbsp;&nbsp;' +
                 '<a href="#" class="morelink">' + moretext + '</a></span>';

      $(this).html(html);
    }
  });

  $(".morelink").off('click').on('click', function(e) {
    e.preventDefault();
    if($(this).hasClass("less")) {
      $(this).removeClass("less");
      $(this).html(moretext);
    } else {
      $(this).addClass("less");
      $(this).html(lesstext);
    }
    $(this).parent().prev().toggle();
    $(this).prev().toggle();
  });
}

$(document).ready(function() {
  addShowMore();
});

// Listen for Shiny message to re-run truncation after dynamic UI updates
Shiny.addCustomMessageHandler("addShowMore", function(message) {
  addShowMore();
});
*/
