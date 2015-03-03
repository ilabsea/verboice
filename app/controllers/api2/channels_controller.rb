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
    before_filter :authorize_admin, only: [:mark_as_approved, :mark_as_pending]

    def index
      if api_current_account.admin?
        if api_current_account.has_access_from?(origin_host)
          channels = params[:account_id] ? Channel.by_account_id(params[:account_id]) : Channel.where("1=1") 
        else
          return head :unauthorized
        end
      else
        channels = api_current_account.channels
      end

      channels = channels.by_status(params[:status]) if params[:status].present?

      render json: channels, each_serializer: CustomChannelSerializer, account: true
    end

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

    # restrict admin tasks
    def activate
      load_channel

      if @channel.update_attributes({status: Channel::STATUS_APPROVED})
        render json: @channel
      else
        render :json => errors_to_json(@channel, 'updating')
      end
    end

    def deactivate
      load_channel

      if @channel.update_attributes({status: Channel::STATUS_PENDING})
        render json: @channel
      else
        render :json => errors_to_json(@channel, 'updating')
      end
    end

    private

    def load_channel
      @channel = Channel.find_by_id(params[:id])
      return head :not_found unless @channel
    end

  end
end
