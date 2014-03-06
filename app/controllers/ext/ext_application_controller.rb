module Ext
	class ExtApplicationController < ApplicationController
    before_filter :authenticate_account!
    
		def load_project project_id
      @project_id = project_id
			@project = Project.find project_id
		end
	end
end