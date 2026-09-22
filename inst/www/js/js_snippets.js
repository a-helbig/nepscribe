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
