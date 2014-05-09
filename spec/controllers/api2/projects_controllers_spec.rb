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

describe Api2::ProjectsController do
  include Devise::TestHelpers

  before(:each) do
    @account = Account.make
    @admin = Account.make role: Account::ADMIN

    @project = @account.projects.make
    @another_project = @admin.projects.make
  end

  context "sign in as admin" do
    before(:each) do
      sign_in @admin
    end

    it "should list all projects" do
      get :index

      response = JSON.parse(@response.body)
      response.length.should eq(2)
    end
  end

  context "sign in as normal user" do
    before(:each) do
      sign_in @account
    end

    it "should list all projects" do
      get :index

      response = JSON.parse(@response.body)
      response.length.should == 1
    end
  end
    
end
