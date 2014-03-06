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

          resource = @project.resources.find_by_guid(guid)
          @resources[guid] = resource
          if resource
            new_resource =  build_resource(resource.attributes)

            if new_resource.save!
              @resources[guid] = new_resource	
              value["guid"] = new_resource.guid
            end
          else 
            attributes = resources_attributes[guid]
            new_resource = build_resource(attributes)
          	new_resource.guid = attributes['guid']
          	new_resource.save!
          	@resources[guid] = new_resource # no need
          end
        end
      end
    end

    # localized_resources
    localized_resources = {}
    localized_resources_attributes.each do |l_guid, l_attributes|
      r_guid = l_attributes['resource_guid']
      resource = @resources[r_guid]	
      next if resource.nil?
      localized_resource_class = l_attributes['type'].constantize

      localized_resource = localized_resource_class.new(l_attributes.except("guid"))
   
      localized_resource.resource = resource
      localized_resource.save!
      localized_resources[l_guid] = localized_resource
    end

	audios_entry.each do |entry|
	  l_guid = guid_localized_resource_from_entry entry
	  localized_resource = localized_resources[l_guid]

	  if localized_resource
	    localized_resource.audio = @zip.read(entry)
	    localized_resource.save!
	  end
	end
    flow
  end

  def guid_localized_resource_from_entry entry
  	 guid = File.basename(entry.name).split.last.gsub('.wav', '')
  	 guid
  end

  def build_resource attributes
  	resource = @project.resources.build attributes.slice("name")
  	resource
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