class WitAiParser

	def initialize(call_id)
		@call_id = call_id
	end

	def parse(response)
		case_key = Reports::Settings.case_key
		number_of_case_key = Reports::Settings.number_of_case_key
		symptoms_key = Reports::Settings.symptoms_key
		location_key = Reports::Settings.location_key
		index = 0
		if response["entities"][case_key]
			cases = response["entities"][case_key] 
			@properties = {}
			cases.each do |obj|
	    	@properties[index] = {}
	    	@properties[index][number_of_case_key] = obj["entities"][number_of_case_key][0]["value"] if obj["entities"][number_of_case_key]
	    	@properties[index][symptoms_key] = obj["entities"][symptoms_key].map {|s| s["value"]} if obj["entities"][symptoms_key]
	      index = index + 1;
	    end
	  end
	  @properties[index] = {}
  	@properties[index][number_of_case_key] = response["entities"][number_of_case_key][0]["value"] if response["entities"][number_of_case_key]
  	@properties[index][symptoms_key] = response["entities"][symptoms_key].map {|s| s["value"]} if response["entities"][symptoms_key]
  	@properties.delete(index) if @properties[index][number_of_case_key] == nil and @properties[index][symptoms_key] == nil
	  @location = response["entities"][location_key] ? response["entities"][location_key][0]["value"] : ""
    @message = response["_text"]
	end

	def to_report_object()
		report = Report.new
		report.message = @message
		report.properties = @properties
		report.location = @location
		report.call_id = @call_id
		return report
	end

end
