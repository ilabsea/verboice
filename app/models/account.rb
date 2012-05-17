class Account < ActiveRecord::Base
  # Include default devise modules. Others available are:
  # :token_authenticatable, :confirmable, :lockable and :timeoutable
  devise :database_authenticatable, :registerable,
         :recoverable, :rememberable, :trackable, :validatable, :confirmable

  # Setup accessible (or protected) attributes for your model
  attr_accessible :email, :password, :password_confirmation, :remember_me

  has_many :projects, :dependent => :destroy
  has_many :call_logs
  has_many :schedules
  has_many :channels, :dependent => :destroy
  has_many :contacts, :dependent => :destroy
  has_many :persisted_variables, :through => :contacts
  has_many :queued_calls, :through => :channels

  def call(options = {})
    channel = channels.find_by_name! options[:channel]
    channel.call options[:address], options
  end

  def distinct_variables
    persisted_variables.select(:name).uniq.pluck(:name)
  end

end
