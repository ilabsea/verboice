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
class ImportFlowFromZip
  def initialize zip
  	@zip = zip
  end

  def user_flow flow, project, resources_entry, localized_resources_entry, audios_entry
  	@resources = {}
  	@project = project
  	resources_attributes = attributes_from_entries(resources_entry)
  	localized_resources_attributes = attributes_from_entries(localized_resources_entry)

  	flow.each do |step|
      step.each do |key, value|
        if key.match(/resource/) && guid=value['guid']

          attributes = resources_attributes[guid]
          if attributes
            new_resource =  @project.resources.build attributes.slice("name")

            if new_resource.save!
              value["guid"] = new_resource.guid

              localized_resources_attributes.each do |l_guid, l_attributes|
                r_guid = l_attributes['resource_guid']

                if r_guid == guid
                  localized_resource_class = l_attributes['type'].constantize
                  localized_resource = localized_resource_class.new(l_attributes.except("guid"))
      
                  localized_resource.resource = new_resource
                  localized_resource.save!

                  audios_entry.each do |entry|
                    al_guid =  File.basename(entry.name).split.last.gsub('.wav', '')
                    if l_guid == al_guid
                       localized_resource.audio = @zip.read(entry)
                       localized_resource.save!
                       # break
                    end
                  end
                  # break
                end 

              end
            end
          end
        end
      end
    end
    flow
  end

  def attributes_from_entries entries
  	attributes = {}
  	entries.each do |entry|
      attrs = YAML::load(@zip.read(entry))
      attributes[attrs['guid']] = attrs
    end
    attributes
  end
end