class WitEntitiesExtractor
  
  REPORT_KEY = Reports::Settings.report_key
  CASE_KEY = Reports::Settings.case_key
  SYMPTOMS_KEY = Reports::Settings.symptoms_key
  LOCATION_KEY = Reports::Settings.location_key

  class << self
    def extract(wit_json_response)
      return {} if wit_json_response.nil? || wit_json_response.empty?

      entities = { 'text' => wit_json_response['_text'] }
      entities[LOCATION_KEY] = wit_json_response['entities'][LOCATION_KEY] ? wit_json_response['entities'][LOCATION_KEY][0]['value'] : ''
      entities[REPORT_KEY] = extract_reports wit_json_response['entities']

      entities
    end

    private

    def extract_reports wit_entities
      return [] if wit_entities.nil? || wit_entities.empty?

      entities = []

      if wit_entities[REPORT_KEY]
        wit_entities[REPORT_KEY].each do |report_entity|
          if report_entity['entities']
            entity = extract_report(report_entity['entities'])

            entities << entity if (!entity.nil? and !entity.empty?)
          end
        end
      end

      entity = extract_report wit_entities
      entities << entity if (!entity.nil? and !entity.empty?)

      return entities
    end

    def extract_report report_entities
      entity = {}

      entity[SYMPTOMS_KEY] = report_entities[SYMPTOMS_KEY].map { |s| s['value'] } if report_entities[SYMPTOMS_KEY]
      entity[CASE_KEY] = report_entities[CASE_KEY][0]['value'] if report_entities[CASE_KEY]

      return entity
    end
  end

end
