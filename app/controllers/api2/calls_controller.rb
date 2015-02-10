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
module Api2
  class CallsController < Api2Controller

    def call
      if api_current_account.has_access_from?(origin_host)
        options = params[:call]
        channel  = api_current_account.channels.find(options[:channel_id])
        call_log = channel.call(options[:address], options)
        render :json => {:call_id => call_log.id, :state => call_log.state}
      else
        return head :unauthorized
      end
    end

    def bulk_call
      if api_current_account.has_access_from?(origin_host)
        options = params[:call]
        call_logs = options.each do |call_options|
          channel  = api_current_account.channels.find(call_options[:channel_id])
          call_log = channel.call(call_options[:address], call_options)
        end
        render :json => call_logs.map{|call_log| {call_id: call_log.id, state: call_log.state} }
      else
        return head :unauthorized
      end
    end

    def redirect
      options = {}
      if request.post?
        options[:flow] = Parsers::Xml.parse request.body
      elsif params[:call_flow_id]
        if not api_current_account.call_flows.exists? params[:call_flow_id]
          return render :status => 404
        end
        options[:call_flow_id] = params[:call_flow_id]
      elsif params[:project_id]
        if not api_current_account.projects.exists? params[:project_id]
          return render :status => 404
        end
        options[:project_id] = params[:project_id]
      elsif params[:callback_url]
        options[:callback_url] = params[:callback_url]
      else
        return render :status => 400
      end

      channel = CallLog.find(params[:id]).channel
      BrokerClient.redirect options

      render :text => 'OK'
    end

    def state
      call_log = api_current_account.call_logs.where(:id => params[:id]).first
      render :json => {:call_id => call_log.id, :state => call_log.state}
    end
  end
end