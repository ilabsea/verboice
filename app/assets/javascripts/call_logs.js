$(function(){
  bindPerPageChange();
})


function bindPerPageChange(){
  $(".page_counter").on('change', function(){
     $this = $(this);
     value = $this.val();

     url = replaceParam('per_page', value);
     window.location.href = url;
  })
}

function replaceParam(key, value){
  pathname = window.location.pathname;
  params = toParams(window.location.search);
  if(params['page']){
    delete(params['page']);
  }
  params[key] = value;

  return pathname + "?" + jQuery.param(params)
}

function toParams(searchUrl) {
  var result = {}
  if(searchUrl == '')
    return result;

  var queryString = searchUrl.substr(1);

  var params = queryString.split("&");

  jQuery.each(params, function(index, param){
    keyPair = param.split("=")
    
    key = keyPair[0]
    value = decodeURIComponent(keyPair[1])

    if(result[key] == undefined)
      result[key] = value
    else{
      if(result[key] instanceof Array) //current var is an array just push another to it
        result[key].push(value)
      else{ //duplicate var, then it must store as an array
        result[key] = [result[key]]
        result[key].push(value)
      }
    }
  })

  return result;

}
