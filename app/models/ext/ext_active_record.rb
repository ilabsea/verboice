module Ext
	class ExtActiveRecord < ActiveRecord::Base
	  self.abstract_class = true
	  self.table_name_prefix = 'ext_'

	  def self.assign_has_many_to class_name, associats, options = {}
	  	 class_name.constantize.instance_eval do
	  	 	has_many associats, options
	  	 end
	  end

    def self.assign_accepts_nested_attributes_for_to class_name, attributes, options = {}
      class_name.constantize.instance_eval do
        accepts_nested_attributes_for attributes, options
      end
    end

    def self.assign_attr_accessible_to class_name, attributes
      class_name.constantize.instance_eval do
        attr_accessible attributes
      end
    end

	end
end