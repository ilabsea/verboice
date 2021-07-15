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
        begin
          res = Net::HTTP.post_form(URI(@config[:authorize_url]), 'username' => @config[:username], 'password' => @config[:password])
          return JSON.parse(res.body)["access_token"] if res.is_a?(Net::HTTPSuccess)

          notify_error(Exception.new, res.body)
        rescue Exception => e
          notify_error(e)
        end
      end

      def get_contacts(token)
        begin
          res = get_contacts_response(token)
          return @reminder_group.upsert_reminder_group_contacts(JSON.parse(res.body)) if res.is_a?(Net::HTTPSuccess)

          notify_error(Exception.new, res.body)
        rescue Exception => e
          notify_error(e)
        end
      end

      def get_contacts_response(token)
        date = (DateTime.yesterday.beginning_of_day + @config[:schedule].to_i.hours).iso8601(3)
        filter = {"filter"=>"{\"where\": {\"and\":[{\"updatedAt\":{\"gte\":\"#{date}\"}}]}}"}
        uri = URI("#{@config[:endpoint]}?#{filter.to_query}&access_token=#{token}")

        http = Net::HTTP.new(uri.host, uri.port)
        http.read_timeout = ENV['TIMEOUT_DURATION'].present? ? ENV['TIMEOUT_DURATION'].to_i : 60
        http.get("#{uri.path}?#{uri.query}")
      end

      def notify_error(exception, message=nil)
        ExceptionNotifierMailer.notify_exception(@config[:exception_recipients], exception, default_data.merge({msg: message}) ).deliver
      end

      def default_data
        @default_data ||= {
          project: {
            id: @reminder_group.project_id,
            name: @reminder_group.project.name
          },
          reminder_group: {
            id: @reminder_group.id,
            name: @reminder_group.name
          }
        }
      end
  end
end
