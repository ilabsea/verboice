require 'yaml'

module Reports
  module Settings
    extend self

    CONFIG = YAML.load_file(File.expand_path('../config/settings.yml', __FILE__))

    def method_missing(method_name)
      if method_name.to_s =~ /(\w+)\?$/
        CONFIG[$1] == true
      else
        CONFIG[method_name.to_s]
      end
    end

  end
end
