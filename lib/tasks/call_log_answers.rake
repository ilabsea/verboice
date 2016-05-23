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

namespace :call_log_answer do
  desc "Migrate call log trace to step interaction"
  task :remove_duplicate => :environment do
    log("Removing duplication record\n") do
      duplicates = CallLogAnswer.select("call_log_id, project_variable_id, count(*)").group("call_log_id, project_variable_id").having("count(*) > 1").to_a
      duplicates.each_with_index do |duplicate, i|
        call_log_answers = CallLogAnswer.where(call_log_id: duplicate.call_log_id, project_variable_id: duplicate.project_variable_id).order(:updated_at)
        call_log_answers[0..-2].each do |call_log_answer|
          call_log_answer.destroy
        end

        puts "Processing #{i}/#{duplicates.count}"
      end

      puts
    end
  end
end