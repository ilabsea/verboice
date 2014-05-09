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

describe Account do
  let(:super_user) { Account.make role: Account::ADMIN }
  let(:user) { Account.make role: Account::USER }

  it { should have_many(:projects) }
  it { should have_many(:channels) }
  it { should have_many(:call_logs) }
  it { should have_many(:ext_reminder_groups) }

  describe "#admin?" do
    it { expect(super_user.admin?).to be true }
    it { expect(user.admin?).to be false }
  end

  describe "#user?" do
    it { expect(super_user.user?).to be false }
    it { expect(user.user?).to be true }
  end
  
  describe "#has_access_from?" do
    context "admin" do
      it { expect(super_user.admin?).to be true }
      it { expect(super_user.has_access_from?("127.0.0.1")).to be true }
      it { expect(super_user.has_access_from?("192.168.1.1")).to be false }
    end

    context "user" do
      it { expect(user.admin?).to be false }
      it { expect(user.has_access_from?("127.0.0.1")).to be false }
      it { expect(user.has_access_from?("192.168.1.1")).to be false }
    end
  end
end