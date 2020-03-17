$(document).ready(function() {
  $(document).on('click', '.advance_view', function () {
    Shiny.onInputChange('advance_clicked',this.id);
  });
  $(document).on('click', '.retract_view', function () {
    Shiny.onInputChange('retract_clicked',this.id);
  });
  Shiny.addCustomMessageHandler('resetValue', function(variableName) {
    Shiny.onInputChange(variableName, null);
  });
});
