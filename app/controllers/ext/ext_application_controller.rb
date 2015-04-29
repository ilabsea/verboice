module Ext
	class ExtApplicationController < ApplicationController
    before_filter :authenticate_account!
    
	end
end