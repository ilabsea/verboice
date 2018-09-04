class WitAiParser

	def initialize(call_id)
		@call_id = call_id
	end

	def parse(response)
		index = 0
		if response["entities"][Options.case_key]
			cases = response["entities"][Options.case_key] 
			@properties = {}
			cases.each do |obj|
	    	@properties[index] = {}
	    	@properties[index][Options.number_of_case_key] = obj["entities"][Options.number_of_case_key][0]["value"] if obj["entities"][Options.number_of_case_key]
	    	@properties[index][Options.symptoms_key] = obj["entities"][Options.symptoms_key].map {|s| s["value"]} if obj["entities"][Options.symptoms_key]
	      index = index + 1;
	    end
	  end
	  @properties[index] = {}
  	@properties[index][Options.number_of_case_key] = response["entities"][Options.number_of_case_key][0]["value"] if response["entities"][Options.number_of_case_key]
  	@properties[index][Options.symptoms_key] = response["entities"][Options.symptoms_key].map {|s| s["value"]} if response["entities"][Options.symptoms_key]
  	@properties.delete(index) if @properties[index][Options.number_of_case_key] == nil and @properties[index][Options.symptoms_key] == nil
	  @location = response["entities"][Options.location_key] ? response["entities"][Options.location_key][0]["value"] : ""
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