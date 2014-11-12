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

describe Api2::ReminderSchedulesController do

  let!(:account) { Account.make }
  let!(:project) { Project.make account: account }
  let!(:call_flow) { CallFlow.make project: project }
  let!(:reminder_group) { Ext::ReminderGroup.make project: project, addresses: [] }
  let!(:reminder_schedule) { Ext::ReminderSchedule.make(
    project_id: project.id,
    call_flow_id: call_flow.id,
    reminder_group_id: reminder_group.id,
    client_start_date: "25/10/2012",
    time_from: "10:00",
    time_to: "12:00",
    schedule_type: Ext::ReminderSchedule::TYPE_ONE_TIME)
  }

  describe "get index" do
    it "should response 200" do
      get :index, email: account.email, token: account.auth_token, project_id: project.id

      assert_response :success
    end
  end

  describe "post create" do
    it "should response 201" do
      expect {
        post :create, email: account.email, token: account.auth_token, project_id: project.id,
                      reminder_schedule: {
                        project_id: project.id,
                        call_flow_id: call_flow.id,
                        reminder_group_id: reminder_group.id,
                        client_start_date: "25/10/2012",
                        time_from: "10:00",
                        time_to: "12:00",
                        schedule_type: Ext::ReminderSchedule::TYPE_ONE_TIME
                      }

        assert_response :created
      }.to change(project.ext_reminder_schedules, :count).by(1)
    end
  end

  describe "put update" do
    it "should response 200" do
      put :update, email: account.email, token: account.auth_token, project_id: project.id, id: reminder_schedule.id,
                  reminder_schedule: {
                    project_id: project.id,
                    call_flow_id: call_flow.id,
                    reminder_group_id: reminder_group.id,
                    client_start_date: "25/10/2012",
                    time_from: "10:00",
                    time_to: "12:00",
                    schedule_type: Ext::ReminderSchedule::TYPE_ONE_TIME,
                  }

      assert_response :success
    end
  end

  describe "delete destroy" do
    it "should response 200" do
      expect{
        delete :destroy, email: account.email, token: account.auth_token, project_id: project.id, id: reminder_schedule.id

        assert_response :success
      }.to change(project.ext_reminder_schedules, :count).from(1).to(0)
    end
  end

end
