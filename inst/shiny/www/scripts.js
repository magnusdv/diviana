
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
  if (!$('#data-accordion').hasClass('single-open')) return;
  $('#data-accordion .card').not(this).each(function() {
    if (!$(this).hasClass('collapsed-card'))
      $(this).CardWidget('collapse');
  });
});

// Toggle by clicking anywhere in the header
$(document).on('click', '#data-accordion .card > .card-header', function(e) {
  if ($(e.target).closest('[data-card-widget="collapse"]').length) return;
  $(this).parent().CardWidget('toggle');
});

Shiny.addCustomMessageHandler('initDataAccordion', function(x) {
  $('#data-accordion').toggleClass('single-open', x.large);
  const cards = $('#data-accordion .card');
  const i = x.card === 'PM' ? 1 : 0;
  cards.each(function(j) {
    const open = !x.large || j === i;
    if (open && $(this).hasClass('collapsed-card'))
      $(this).CardWidget('expand');
    if (!open && !$(this).hasClass('collapsed-card'))
      $(this).CardWidget('collapse');
  });
});
