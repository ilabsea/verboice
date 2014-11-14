$ ->
  if $(".account-form #account_password").length > 0
    $(document).on "input", "#account_password", ->
      email = $('#account_email').val()
      password = $('#account_password').val()
      validatePassword = PasswordStrength.test(email, password)

      if password.length > 0
        if (validatePassword.status == 'weak')
          $('#validateMeter').removeClass('strong-bar').addClass('weak-bar')
        else if (validatePassword.status == 'good' || 'strong')
          $('#validateMeter').removeClass('weak-bar').addClass('strong-bar')

        if password.length > 8
          length = 8
        else
          length = password.length

        $('#validateMeter').width(length*30)    
        $('#validateMessage').html(validatePassword.status)
      else
        $('#validateMeter').width(0)
        $('#validateMessage').html('')

      $(document).on "blur", "#account_password_confirmation", ->
        password = $('#account_password').val()
        password_confirmation = $('#account_password_confirmation').val()

        if password != password_confirmation
          $('#confirmMessage').html('Password does not match confirmation')
        else
          $('#confirmMessage').html('')