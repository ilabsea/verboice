ko.bindingHandlers.datePicker =
  init: (element, valueAccessor) ->
    value = valueAccessor()

    $(element).val ko.utils.unwrapObservable value
    unless $(element).is '[readonly]'
      $(element).datepicker
        dateFormat: 'dd/mm/yy'
        onSelect: (selectedDate) ->
          value selectedDate
          $(@).datepicker 'hide'
