$(function() {
  $('#call_flow_fusion_table_name').keypress(function(e){
    if (e.which > 0 // check that key code exists
      && e.which != 8 // allow backspace
      && e.which != 95 // allow underscore
      // && e.which != 32 && e.which != 45 //allow space and dash 
      && !(e.which >= 48 && e.which <= 57) // allow 0-9
      && !(e.which >= 65 && e.which <= 90) // allow A-Z
      && !(e.which >= 97 && e.which <= 121) // allow a-z
    ) {
      e.preventDefault()
    }
  })
})