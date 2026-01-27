$( document ).ready(function() {
  Shiny.addCustomMessageHandler('hideCols', function(params) {
     Reactable.setHiddenColumns(params.id, params.cols)
  });
});
