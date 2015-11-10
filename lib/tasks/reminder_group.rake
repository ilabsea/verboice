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

namespace :reminder_group do
  desc "Replace country code with zero"
  task :replace_country_code_with_zero, [:project_id, :country_code] => :environment do |t, args|
    log("Replacing country code to zero in contact address\n") do
      project_id = args[:project_id]
      country_code = args[:country_code]

      reminder_groups = Ext::ReminderGroup.where(project_id: project_id)

      Ext::ReminderGroup.transaction do
        reminder_groups.each_with_index do |reminder_group, i|
          print "Processing #{i + 1}/#{reminder_groups.count}\r"
          reminder_group.addresses.each_with_index do |address, k|
            if address.start_with?(country_code)
              address_with_zero_prefix = address.gsub(/^#{country_code}/, "0")
              reminder_group.addresses[k] = address_with_zero_prefix
            end
          end
          reminder_group.save!
        end
      end

    end
  end
end
