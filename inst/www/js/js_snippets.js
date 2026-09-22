// Define a globally available JS function that will modify sidebar width according to user input
Shiny.addCustomMessageHandler('sidebarWidth', function(width) {
  document.querySelector('.bslib-sidebar-layout').style.setProperty('--_sidebar-width', width + 'px');
});

// Initialize grey Bootstrap-styled hover tooltips for data-toggle="tooltip" elements, app-wide
// (the Transform Data sidebar's inputs, Explore Datasets' tab/column tooltips, etc.), so they
// all match the same style instead of some falling back to the browser's native black tooltip.
// Uses Bootstrap 5's native JS API (bootstrap.Tooltip), NOT the jQuery $.fn.tooltip() plugin -
// this app only loads Bootstrap 5's bundle (bslib theme), which doesn't provide that jQuery
// plugin at all, so $.fn.tooltip is never defined here. A no-op if bootstrap.Tooltip or the
// target elements aren't available yet - safe and cheap to call repeatedly/speculatively.
window.initAppTooltips = function (selector) {
  if (typeof window.bootstrap === 'undefined' || !window.bootstrap.Tooltip) return;
  document.querySelectorAll(selector || '[data-toggle="tooltip"]').forEach(function (el) {
    if (!bootstrap.Tooltip.getInstance(el)) {
      new bootstrap.Tooltip(el, { html: true, trigger: 'hover', delay: { show: 500, hide: 100 } });
    }
  });
};

$(document).on('shiny:connected shiny:idle shiny:inputchanged', function () {
  window.initAppTooltips();
});

// Belt and suspenders: on a quiet page, the events above might all fire before every tab's
// content has actually loaded, with nothing left to re-trigger a retry afterwards. So also poll
// unconditionally for a while after each page load, rather than relying on an early "did it
// work?" check that could be fooled by an empty element set into declaring success too soon.
var _tooltipPollCount = 0;
var _tooltipPoll = setInterval(function () {
  _tooltipPollCount++;
  window.initAppTooltips();
  if (_tooltipPollCount > 20) clearInterval(_tooltipPoll);
}, 500);

// Define a globally available JS function that will show NA in datatables
function customRowCallback(row, data) {
  for(var i=0; i<data.length; i++){
    if(data[i] === null || data[i] === 'NA'){  // Check for null or string "NA"
      $('td:eq('+i+')', row).html('NA')
        .css({'color': 'rgb(151,151,151)', 'font-style': 'italic'});
    }
  }
}




(function () {
  function initSidebarResize() {
    var layout = document.querySelector('.bslib-sidebar-layout');
    if (!layout || layout.querySelector('.sidebar-resizer')) return;

    var handle = document.createElement('div');
    handle.className = 'sidebar-resizer';
    layout.appendChild(handle);

    var MIN = 150, MAX = 600, dragging = false;
    handle.addEventListener('mousedown', function (e) {
      dragging = true; document.body.classList.add('sidebar-resizing'); e.preventDefault();
    });
    document.addEventListener('mousemove', function (e) {
      if (!dragging) return;
      var w = e.clientX - layout.getBoundingClientRect().left;
      w = Math.max(MIN, Math.min(MAX, w));
      layout.style.setProperty('--_sidebar-width', w + 'px');
    });
    document.addEventListener('mouseup', function () {
      if (!dragging) return;
      dragging = false; document.body.classList.remove('sidebar-resizing');
      // optional: keep the slider in sync
      // Shiny.setInputValue('<your_slider_id>', Math.round(parseFloat(getComputedStyle(layout).getPropertyValue('--_sidebar-width'))));
    });
  }
  $(document).on('shiny:connected', initSidebarResize);
})();
