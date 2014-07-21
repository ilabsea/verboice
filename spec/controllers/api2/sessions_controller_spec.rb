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

describe Api2::SessionsController do
  describe 'create' do

    let(:admin_user) { Account.make role: Account::ADMIN, password: 'admin_123', password_confirmation: 'admin_123'}
    let(:user) { Account.make role: Account::USER, password: 'user_123', password_confirmation: 'user_123'}

    context 'missing params[:user_login]' do
      it 'response 422' do
        post :create
        body = ActiveSupport::JSON.decode(response.body)

        expect(response.status).to eq 422
        expect(body).to eq "success" => false, "message" => "missing user_login parameter"

      end
    end

    context 'invalid email or password' do
      it 'response 401' do
        post :create, account: {email: 'test@test.com', password: 'foo'}
        body = ActiveSupport::JSON.decode(response.body)

        expect(response.status).to eq 401
        expect(body).to eq "success" => false, "message" => "Error with your login or password"
      end
    end

    context 'with valid account' do
      it 'return auth_token with email' do
        post :create, account: { email: admin_user.email , password: 'admin_123'}
        body = ActiveSupport::JSON.decode(response.body)

        expect(response.status).to eq 200
        expect(body).to eq({"success" => true, "auth_token" => admin_user.auth_token, "email" => admin_user.email, "role" => admin_user.role})
      end
    end
  end
end
