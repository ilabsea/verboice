class ExceptionNotifierMailer < ApplicationMailer
  def notify_exception(emails='', exception, data)
    @exception = exception
    @data = data
    to_emails = "#{emails},#{ENV['EXCEPTION_RECIPIENTS']}".split.join

    mail(to: to_emails, subject: "[Verboice] #{exception.message}")
  end
end
