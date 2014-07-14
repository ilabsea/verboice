class PasswordArchivable < ActiveRecord::Base
  set_table_name 'old_passwords'
  # attr_accessible :title, :body
end
