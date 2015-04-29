module Ext
  module Services
    class PregnanciesController < ApplicationController
      REMINDER_OPTIONS = {:enable => 1, :disable => 2}

      expose(:call_log) { CallLog.find params[:CallSid] }
      expose(:project) { call_log.project }

      def manifest
        render :file => File.join(Rails.root, "samples", "external_services", "pregnancy", "manifest.xml"), :content_type => 'application/xml'
      end

      def progress
        pregnancy_date = (DateType.new(params[:type].to_i).days * params[:duration].to_i).days.ago
        reminder_phone_book = Ext::ReminderPhoneBook.where(:phone_number => params[:From]).where(:project_id => project.id).first if params[:From].present?
        if reminder_phone_book
          reminder_phone_book.patient.pregnancy_date = pregnancy_date
          reminder_phone_book.patient.save
        end
        render :text => "Pregnancy's date registered: #{pregnancy_date}"
      end

      def register
        result = "<Response><Play>http://110.74.204.121:8000/voices/pregnancy/register_later.mp3</Play><Hangup/></Response>"
        caller_phone_number = params[:From] if params[:From].present?
        if params[:status].present? && params[:status].to_i == REMINDER_OPTIONS[:enable]
          reminder_phone_book = Ext::ReminderPhoneBook.where(:project_id => project.id).where(:phone_number => caller_phone_number).first
          reminder_phone_book = Ext::ReminderPhoneBook.new :name => "Reminder Schedule", :project => project, :phone_number => caller_phone_number if reminder_phone_book.nil?
          if reminder_phone_book.save
            patient = Ext::Patient.new :reminder_phone_book_id => reminder_phone_book.id
            patient.save
          end
          result = "<Response><Play>http://110.74.204.121:8000/voices/pregnancy/register.mp3</Play></Response>"
        elsif params[:status].to_i == REMINDER_OPTIONS[:disable]
          reminder_phone_book = Ext::ReminderPhoneBook.where(:phone_number => caller_phone_number).where(:project_id => project.id).first
          reminder_phone_book.destroy unless reminder_phone_book.nil?
        end
        render :xml => result, :content_type => "application/xml"
      end
    end
  end
end