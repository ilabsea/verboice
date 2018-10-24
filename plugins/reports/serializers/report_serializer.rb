class ReportSerializer < ActiveModel::Serializer
  attributes :id, :call_id, :message, :properties, :location, :address, :created_at
end
