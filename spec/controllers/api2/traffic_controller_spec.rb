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

describe Api2::TrafficController do

  let(:admin) { Account.make role: Account::ADMIN }
  let(:account) { Account.make role: Account::USER }

  describe "index" do
    context "admin" do
      context "host is not allowed" do
        it "unauthorized" do
          request.stub(:remote_ip).and_return('192.168.1.1')
          get :index, email: admin.email, token: admin.auth_token

          assert_response :unauthorized
        end
      end

      context "host is allowed" do
        context "do not pass start_date or end_date" do
          it "response 422 when end_date is missed" do
            get :index, email: admin.email, token: admin.auth_token, start_date: '05/05/2014'

            assert_response :unprocessable_entity
          end

          it "response 422 when start_date is missed" do
            get :index, email: admin.email, token: admin.auth_token, end_date: '05/05/2014'

            assert_response :unprocessable_entity
          end
        end

        context "pass start_date and end_date" do
          context "invalid date format" do
            it "response 422 when start date is invalid" do
              get :index, email: admin.email, token: admin.auth_token, start_date: '0505/2014', end_date: '05/05/2014'

              assert_response :unprocessable_entity
            end

            it "response 422 when end date is invalid" do
              get :index, email: admin.email, token: admin.auth_token, start_date: '05/05/2014', end_date: '0505/2014'

              assert_response :unprocessable_entity
            end
          end

          context "valid date format" do
            it "response 200" do
              get :index, email: admin.email, token: admin.auth_token, start_date: '05/05/2014', end_date: '05/05/2014'

              assert_response :ok
            end

            it "response 200" do
              get :index, email: admin.email, token: admin.auth_token, start_date: '05-05-2014', end_date: '05-05-2014'

              assert_response :ok
            end
          end
        end
      end
    end

    context "normal user" do
      it "should response 200" do
        get :index, email: account.email, token: account.auth_token, start_date: '05/05/2014', end_date: '05/05/2014'

        assert_response :unauthorized
      end
    end
  end

end