module Ext
  class Patient < ActiveRecord::Base
    belongs_to :reminder_phone_book
    attr_accessible :pregnancy_date, :reminder_phone_book_id
  end
end
