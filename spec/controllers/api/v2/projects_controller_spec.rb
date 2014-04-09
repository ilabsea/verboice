# Copyright (C) 2010-2012, InSTEDD
#
# This file is part of Verboice.
#
# Verboice is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# Verboice is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with Verboice.  If not, see <http://www.gnu.org/licenses/>.

require 'spec_helper'

describe API::V2::ProjectsController do
  include Devise::TestHelpers

  before(:each) do
    @account = Account.make 

    @project1 = @account.projects.make name: 'Project1'
    @project2 = @account.projects.make name: 'Project2'
    @project3 = @account.projects.make name: 'Project3'

    sign_in @account

  end

  describe "get index" do
    it "render projects in json format" do
      get :index
      json = ActiveSupport::JSON.decode(response.body)
    end

  end

end
