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

describe CallLogsController do
  include Devise::TestHelpers

  let(:account) { Account.make }
  let(:project) { Project.make :account => account }
  let(:call_flow) { CallFlow.make :project => project }
  let(:call_log) {CallLog.make :project => project, :call_flow => call_flow}
  let(:channel) { Channel.all_leaf_subclasses.sample.make :call_flow => call_flow, :account => account }

  before(:each) do
    sign_in account
  end

  end

  it 'should order id of call log as DESC when user download the list as csv in project call log' do
    get :download_project_call_logs, project_id: project.id, :call_flow_id => call_flow.id, :format => :csv
    response.should be_success
  end

  describe 'GET index' do

    describe 'paginate' do
      before(:each) do
        get :index
      end

      it 'should assigns page' do
        assigns(:page).should == 1
      end

      it 'should assigns per_page' do
        assigns(:per_page).should ==  10
      end
    end

    it 'should search by call_flow_id' do
      get :index, project_id: project.id, call_flow_id: call_flow.id
      assigns(:search).should match /call_flow_id:"#{call_flow.id}"/
    end

    context 'when search by phone number' do
      it 'should include phone_number in search' do
        get :index, project_id: project.id, phone_number: '123'
        assigns(:search).should match /phone_number:"123"/
      end
    end

    context 'when search by date' do
      let(:before_date) { '2013-11-12' }
      let(:after_date) { '2013-11-01' }

      it 'should include before and after parameters' do
        get :index, project_id: project.id, before: before_date, after: after_date
        assigns(:search).should match /before:"#{before_date}"/
        assigns(:search).should match /after:"#{after_date}"/
      end

      context 'when only search by after date' do
        it 'should not include before date' do
          get :index, project_id: project.id, before: before_date
          assigns(:search).should match /before:"#{before_date}"/
          assigns(:search).should_not match /after:"#{after_date}"/
        end
      end
    end
end
