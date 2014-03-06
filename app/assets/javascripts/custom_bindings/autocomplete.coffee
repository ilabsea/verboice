ko.bindingHandlers.autocomplete =
  init: (element, valueAccessor, allBindingsAccessor) ->
    value = valueAccessor()
    bindings = allBindingsAccessor()

    $(element).val ko.utils.unwrapObservable value
    $(element).autocomplete
      minLength : $(element).attr("minChar")
      source: (term, callback) ->
        $.ajax $(element).attr("url") + "?term=" + $(element).val(),
          success: (data) ->
            callback(data)
      change: (event, ui) ->
        if(ui.item)
          value ui.item.value
          $(element).change()
              
      select    : (event, ui) ->
        value ui.item.value
        $(element).change()
      focus: (event, ui) ->
        value ui.item.value
        $(element).change()

  update: (element, valueAccessor) ->
    $(element).val ko.utils.unwrapObservable valueAccessor()