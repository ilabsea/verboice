$(function() {
  $('#pause').on('click', function(){
    $("form").attr("action", "/calls/queued_paused").submit();
  });
  $('#resume').on('click', function(){
    $("form").attr("action", "/calls/queued_resumed").submit();
  });
  $('#select_all').on('click', function(){
    if($(this).attr("checked") == "checked"){
      $('.queued_calls').attr('checked', "checked");
    }else{
      $('.queued_calls').attr('checked', null);
    }
  });
})