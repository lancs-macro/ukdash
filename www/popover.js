$(function () {
  $('[data-toggle="popover"]').popover({
     container: 'body',
     trigger: 'hover',
     placement: 'bottom',
     html : true
  });
});


$(function () {
  $('[data-toggle="popover-focus"]').popover({
     container: 'body',
     placement: 'bottom',
     html : true
  });
});