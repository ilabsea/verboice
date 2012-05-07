class ApplicationController < ActionController::Base
  protect_from_forgery

  before_filter do
    @body_class = ['full-width']
  end

  def set_fixed_width_content
    @body_class << 'fixed-width-content'
  end
end
