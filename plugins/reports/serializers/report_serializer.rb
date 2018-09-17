class ReportSerializer < ActiveModel::Serializer
  attributes :call_id, :message, :properties, :location, :address, :created_at
end
