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

describe Api2Controller do

  controller described_class do

    before_filter :authenticate_account_from_token!

    def index
      render text: "I am logged-in"
    end
  end

  describe 'GET :index' do
    context 'user not logged in' do
      it 'redirect to login page' do
        get :index
        body = ActiveSupport::JSON.decode(response.body)

        expect(response.header['Content-Type']).to eq 'application/json; charset=utf-8'        
        expect(response.status).to eq 401
        expect(body).to eq({ 'success' => false, 'message' => "Not authorized access"})
      end
    end

    context 'user logged in' do
      it 'renders the result' do
        admin = Account.make role: Account::ADMIN
        get :index, email: admin.email , token: admin.auth_token

        expect(response.body).to eq "I am logged-in"
      end
    end
  end
end
