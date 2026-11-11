
window.sexEditCallback = function(table) {
  const id = $(table.table().container()).closest('.html-widget').attr('id') + '_sex_edit';

  table.on('change', 'select.sex-edit', function() {
    Shiny.setInputValue(id, {
      key: this.dataset.key,
      value: this.value,
      nonce: Math.random()
    }, {priority: 'event'});
  });

  return table;
};



$(document).on('expanded.lte.cardwidget', '#data-accordion .card', function() {
  let n = 0, nonempty = 0;
  $('#data-accordion table.dataTable').each(function() {
    if ($.fn.dataTable.isDataTable(this)) {
      const k = $(this).DataTable().rows().count();
      n += k;
      if(k > 0) nonempty++;
    }
  });

  if(n > 22 && nonempty === 2) {
    $('#data-accordion .card').not(this).each(function() {
      if (!$(this).hasClass('collapsed-card')) {
        $(this).CardWidget('collapse');
      }
    });
  }
});


Shiny.addCustomMessageHandler('initDataAccordion', function(x) {
  const cards = $('#data-accordion .card');
  const i = x.card === 'PM' ? 1 : 0;
  cards.each(function(j) {
    const open = !x.large || j === i;
    if(open && $(this).hasClass('collapsed-card'))
      $(this).CardWidget('expand');
    if(!open && !$(this).hasClass('collapsed-card'))
      $(this).CardWidget('collapse');
  });
});


Shiny.addCustomMessageHandler('clearOutputs', function(ids) {
  ids.forEach(function(id) {
    var el = document.getElementById(id);
    if (el) el.innerHTML = '';
  });
});
