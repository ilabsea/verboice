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

describe SessionsController do
  include Devise::TestHelpers

  before(:each) do
    setup_controller_for_warden
    request.env['devise.mapping'] = Devise.mappings[:account]

    @account = Account.make email: 'testing@instedd.org', password: 'V@lidw0rd'
  end

  describe "GET new" do
    it "render without captcha" do
      get :new

      assigns(:captcha).should be(false)
      response.status.should be 200
    end

    it "render without captcpa when an email is not attacked" do
      Login::Ip.any_instance.stub(:reaches_maximum_failed_attempt?).with("foo@example.org").and_return(false)
      get :new, account: { email: "foo@example.org" }

      assigns(:captcha).should be(false)
      response.status.should be 200
    end

    it "render with captcha when an email is attacked and reaches the maximum failed attempt" do
      Login::Ip.any_instance.stub(:reaches_maximum_failed_attempt?).with("foo@example.org").and_return(true)
      get :new, account: { email: "foo@example.org" }

      assigns(:captcha).should be(true)
      response.status.should be 200
    end
  end

  describe "POST create" do
    context "account is not attacked" do
      it "with an invalid username or password" do
        post :create, account: {email: 'foo@example.org', password: 'foo'}

        LoginTracker.count.should eq(1)
        LoginTracker.first.marked_as.should eq(LoginTracker::MARKED_AS_FAILED)

        flash[:error].should eq "Invalid email or password"
        assert_response :redirect
      end

      it "with an valid username and password" do
        post :create, account: {email: 'testing@instedd.org', password: 'V@lidw0rd'}

        LoginTracker.count.should eq(1)
        LoginTracker.first.marked_as.should eq(LoginTracker::MARKED_AS_SUCCESS)

        flash[:notice] = "Signed in successfully."
        assert_response :redirect
      end
    end

    context "account is attacked" do
      it "with an invalid captcha" do
        Login::Ip.any_instance.stub(:reaches_maximum_failed_attempt?).with("foo@example.org").and_return(true)
        controller.stub(:verify_recaptcha).and_return(false)

        post :create, account: {email: 'foo@example.org', password: 'foo'}

        LoginTracker.count.should eq(1)
        LoginTracker.first.marked_as.should eq(LoginTracker::MARKED_AS_FAILED)

        flash[:error].should eq "Invalid captcha"
        assert_response :redirect
      end

      it "with an valid captcha but invalid username or password" do
        Login::Ip.any_instance.stub(:reaches_maximum_failed_attempt?).with("foo@example.org").and_return(true)
        controller.stub(:verify_recaptcha).and_return(true)

        post :create, account: {email: 'foo@example.org', password: 'foo'}

        LoginTracker.count.should eq(1)
        LoginTracker.first.marked_as.should eq(LoginTracker::MARKED_AS_FAILED)

        flash[:error].should eq "Invalid email or password"
        assert_response :redirect
      end

      it "with an valid captcha and valid username and password" do
        Login::Ip.any_instance.stub(:reaches_maximum_failed_attempt?).with("testing@instedd.org").and_return(true)
        controller.stub(:verify_recaptcha).and_return(true)

        post :create, account: {email: 'testing@instedd.org', password: 'V@lidw0rd'}, recaptcha_response_field: "123456"

        LoginTracker.count.should eq(1)
        LoginTracker.first.marked_as.should eq(LoginTracker::MARKED_AS_SUCCESS)

        flash[:notice] = "Signed in successfully."
        assert_response :redirect
      end
    end
  end

end
