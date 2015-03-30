#= require workflow/steps/input_setting

onWorkflow ->
  class window.PersonateOption extends window.InputSetting
    content_kinds: () =>
      return [
        {text: 'Variable', value: 'variable'},
        {text: 'Step', value: 'step'},
        {text: 'Response', value: 'response'}]
