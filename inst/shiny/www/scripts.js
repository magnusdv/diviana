
window.onbeforeunload = function(){ Shiny.onInputChange('browserClosed', Math.random()); };



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
  let n = 0;
  $('#data-accordion table.dataTable').each(function() {
    if ($.fn.dataTable.isDataTable(this)) {
      n += $(this).DataTable().rows().count();
    }
  });

  if (n > 22) {
    $('#data-accordion .card').not(this).each(function() {
      if (!$(this).hasClass('collapsed-card')) {
        $(this).CardWidget('collapse');
      }
    });
  }
});
