class Yaml
  # re-generate new resource guid to remove references
  def self.regenerate_new_resource_guid! yaml, project
    yaml.each do |step|
      step.each do |key, value|
        if key.match /resource/
          resource = project.resources.find_by_guid(value["guid"])
          if resource
            new_resource = project.resources.build resource.attributes
            new_resource.localized_resources.build resource.localized_resources.first.attributes if resource.localized_resources.first
            if new_resource.save
              value["guid"] = new_resource.guid
            end
          end
        end
      end
    end
    yaml
  end
end