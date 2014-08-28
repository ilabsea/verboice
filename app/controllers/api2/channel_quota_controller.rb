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
  class ChannelQuotaController < Api2Controller
    before_filter :validate_authorization

    def create
      channel = Channel.find_by_id(filter_params[:channel_id])
      if channel
        channel_quota = ChannelQuota.find_by_channel_id(channel.id)
        if channel_quota
          channel_quota.update_attributes(filter_params.except(:channel_id))
        else
          channel_quota = channel.build_quota(filter_params.except(:channel_id))
          channel_quota.save
        end
      else
        return head :bad_request
      end

      render json: channel_quota, serializer: CustomChannelQuotaSerializer
    end

    def destroy
      channel_quota = ChannelQuota.find_by_id(params[:id])
      if channel_quota
        channel_quota.destroy
      else
        return head :bad_request
      end

      render json: 'Channel quota has been removed'
    end

    private

    def validate_authorization
      return head :unauthorized if !api_current_account.admin? || !api_current_account.has_access_from?(origin_host)
    end

    def filter_params
      params.slice(:channel_id, :enabled, :blocked, :total, :used)
    end
  end
end
