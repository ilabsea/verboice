require 'uri'
require 'net/http'

module Ext
  class ReminderGroupService
    def initialize(reminder_group)
      @reminder_group = reminder_group
      @config = reminder_group.sync_config
    end

    def sync_with_go_data
      return unless @reminder_group.enable_sync

      get_contacts(get_token)
    end

    private
      def get_token
        uri = URI("#{@config[:endpoint]}/api/oauth/token")
        res = Net::HTTP.post_form(uri, 'username' => @config[:username], 'password' => @config[:password])
        res.is_a?(Net::HTTPSuccess) ? JSON.parse(res.body)["access_token"] : nil
      end

      def get_contacts(token)
        return unless token.present?

        date = (DateTime.yesterday.beginning_of_day + @config[:schedule].to_i.hours).iso8601(3)
        filter = {"filter"=>"{\"where\": {\"and\":[{\"updatedAt\":{\"gte\":\"#{date}\"}}]}}"}
        uri = URI("#{@config[:endpoint]}/api/outbreaks/8d9a7514-84a1-41ee-a527-3b4532f5d2b7/contacts?#{filter.to_query}&access_token=#{token}")
        res = Net::HTTP.get_response(uri)

        @reminder_group.upsert_reminder_group_contacts(JSON.parse(res.body)) if res.is_a?(Net::HTTPSuccess)
      end
  end
end
