// Define a globally available JS function that will modify sidebar width according to user input
Shiny.addCustomMessageHandler('sidebarWidth', function(width) {
  document.querySelector('.bslib-sidebar-layout').style.setProperty('--_sidebar-width', width + 'px');
});

// Attach hover tooltips with question text to items in the "Additional Variables" picker.
// shinyWidgets::multiInput's underlying multi.js fully destroys and rebuilds EVERY item in
// both columns (via innerHTML = "") on every click, not just on dataset/language switches - so
// this keeps a single MutationObserver alive for as long as the picker exists, both to attach
// tooltips to freshly-created elements and to dispose any tooltip whose trigger element was
// just removed (otherwise a tooltip that's actively being shown becomes an orphaned, stuck
// popup the instant its trigger vanishes mid-hover, since nothing else would ever dispose it).
// Uses Bootstrap 5's native JS API (bootstrap.Tooltip), NOT the jQuery $.fn.tooltip() plugin -
// this app only loads Bootstrap 5's bundle (bslib theme), which doesn't provide that jQuery
// plugin at all, so $.fn.tooltip is never defined here.
Shiny.addCustomMessageHandler('variableQuestiontexts', function(map) {
  var wrapper = document.querySelector('.multi-wrapper');
  if (!wrapper) return;

  function disposeTooltip(el) {
    var existing = bootstrap.Tooltip.getInstance(el);
    if (existing) existing.dispose();
  }

  function applyTooltips() {
    if (typeof window.bootstrap === 'undefined' || !window.bootstrap.Tooltip) return;
    document.querySelectorAll('.multi-wrapper .item[data-value]').forEach(function (el) {
      if (bootstrap.Tooltip.getInstance(el)) return; // this exact DOM node is already set up
      var qtext = map[el.getAttribute('data-value')];
      if (qtext) {
        el.setAttribute('data-toggle', 'tooltip');
        el.setAttribute('title', qtext);
        new bootstrap.Tooltip(el, { trigger: 'hover', delay: { show: 500, hide: 100 } });
      }
    });
  }

  // Only one of these should ever be watching at a time - replace, don't stack, across
  // successive dataset/language switches (each of which sends a fresh map/call here).
  if (window.__variableTooltipObserver) window.__variableTooltipObserver.disconnect();

  applyTooltips();
  var observer = new MutationObserver(function (mutations) {
    mutations.forEach(function (m) {
      m.removedNodes.forEach(function (node) {
        if (node.nodeType !== 1) return;
        if (node.matches && node.matches('.item[data-value]')) disposeTooltip(node);
        if (node.querySelectorAll) node.querySelectorAll('.item[data-value]').forEach(disposeTooltip);
      });
    });
    applyTooltips();
  });
  observer.observe(wrapper, { childList: true, subtree: true });
  window.__variableTooltipObserver = observer;
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
