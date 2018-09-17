class Parsers::ReportParser
  def self.parse json_response
    entities = WitEntitiesExtractor.extract(json_response)

    report = Report.new
    report.message = entities['text']
    report.properties = entities[Reports::Settings.report_key]
    report.location = entities[Reports::Settings.location_key]

    return report
  end
end
