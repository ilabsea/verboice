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
  class ChannelsController < Api2Controller

    def get
      channel = api_current_account.channels.find_by_name params[:name]

      if channel.present?
        render :json => channel
      else
        head :not_found
      end
    end

    def create
      data = request.raw_post
      data = JSON.parse(data).with_indifferent_access
      channel = Channel.from_json data
      channel.account = api_current_account
      channel.call_flow = api_current_account.call_flows.find_by_name(data[:call_flow])
      if channel.save
        render :json => channel
      else
        render :json => errors_to_json(channel, 'creating')
      end
    end

    def update
      channel = api_current_account.channels.find_by_name params[:name]

      if channel.present?
        data = request.raw_post
        data = JSON.parse(data).with_indifferent_access
        channel.from_json data
        if channel.save
          render :json => channel
        else
          render :json => errors_to_json(channel, 'updating')
        end
      else
        head :not_found
      end
    end

    def destroy
      channel = api_current_account.channels.find_by_name params[:name]

      if channel.present?
        channel.destroy
        head :ok
      else
        head :not_found
      end
    end

    def list
      p "------"
      p api_current_account
      if api_current_account.admin?
        if api_current_account.has_access_from?(origin_host)
          channels = Channel.all
          channels = Channel.where(account_id: params[:account_id]) if params[:account_id].present?
        else
          return head :unauthorized
        end
      else
        channels = api_current_account.channels
      end

      render json: channels, each_serializer: CustomChannelSerializer
    end

  end
end
