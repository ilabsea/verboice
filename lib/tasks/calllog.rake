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



namespace :call_log do
  desc "Migrate call log trace to step interaction"
  task :migrate_traces => :environment do
    log("Migrating call logs traces") do
      CallLog.includes(:traces).where(step_interaction: nil).find_each do |call_log|
        call_log.update_attribute :step_interaction, call_log.interaction_details.join(';')
        print "."
      end
    end

    failed_ids = CallLog.where(step_interaction: nil).pluck(:id)
    print "\n! - Failed to migrate ids: [#{failed_ids.join ','}]\n"
  end

  desc "Migrate call log duration"
  task :migrate_duration => :environment do
    call_logs = CallLog.where(duration: 0)
    call_logs = call_logs.where("finished_at is not null")
    call_logs = call_logs.where("started_at is not null")
    log("Migrating call logs duration") do
      call_logs.find_each do |call_log|
        call_log.update_attribute :duration, call_log.calculate_duration
        print "."
      end
    end

    failed_ids = call_logs.pluck(:id)
    print "\n! - Failed to migrate ids: [#{failed_ids.join ','}]\n"
  end
end

def log(message = "Starting task")
  started_at = Time.now

  print message
  yield if block_given?
  print "\nTask is done in #{Time.now - started_at} seconds."
end