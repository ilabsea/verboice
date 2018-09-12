class WitAiParser

	CASE_KEY = Reports::Settings.case_key
	NUMBER_OF_CASE_KEY = Reports::Settings.number_of_case_key
	SYMPTOMS_KEY = Reports::Settings.symptoms_key
	LOCATION_KEY = Reports::Settings.location_key

	def initialize(call_id)
		@call_id = call_id
	end

	def parse_to_report(response)
		index = 0
		report = {}
		properties = {}
		if response["entities"][CASE_KEY]
			response["entities"][CASE_KEY].each do |obj|
	    	properties[index] = extract_properties_from_case(obj)
	    	index = index + 1;
	    end
	  end
	  properties[index] = extract_properties_from_none_case response
	  report = Report.new
		report.message = response["_text"]
		report.properties = properties
		report.location = response["entities"][LOCATION_KEY] ? response["entities"][LOCATION_KEY][0]["value"] : ""
		report.call_id = @call_id
    return report
	end

	def extract_properties_from_case case_obj
		properties = {}
  	properties[NUMBER_OF_CASE_KEY] = case_obj["entities"][NUMBER_OF_CASE_KEY][0]["value"] if case_obj["entities"][NUMBER_OF_CASE_KEY]
  	properties[SYMPTOMS_KEY] = case_obj["entities"][SYMPTOMS_KEY].map {|s| s["value"]} if case_obj["entities"][SYMPTOMS_KEY]
		return properties
	end

	def extract_properties_from_none_case response
		properties = {}
  	properties[NUMBER_OF_CASE_KEY] = response["entities"][NUMBER_OF_CASE_KEY][0]["value"] if response["entities"][NUMBER_OF_CASE_KEY]
  	properties[SYMPTOMS_KEY] = response["entities"][SYMPTOMS_KEY].map {|s| s["value"]} if response["entities"][SYMPTOMS_KEY]
  	return nil if (properties[NUMBER_OF_CASE_KEY] == nil and properties[SYMPTOMS_KEY] == nil)
		return properties
	end

end
