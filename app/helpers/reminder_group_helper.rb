module ReminderGroupHelper
  def source_attr_description
    str = '<div style="padding: 10px; width: 500px; line-height: 1.5">'
    str += '<div><u>1) Source attribute</u></div>'
    str += '<div>Example: response data [{"dateOfLastContact": "2021-07-03T00:00:00.000Z"}]</div>'
    str += '<div>So, the source attribute is <strong class="code-higlight">dateOfLastContact</strong></div>'
    str += '<br/>'

    str += '<div>Example: response data [{"addresses": [{"phoneNumber": "011222333"}]}]</div>'
    str += '<div>So, the source attribute is <strong class="code-higlight">addresses_0_phoneNumber</strong></div>'
    str += '<div>Note: to read attribute in the array, it should be <strong class="code-higlight">fieldName_index_fieldName</strong></div>'
    str += '<br/>'

    str += '<div><u>2) Destination attribute</u></div>'
    str += '<div>It is your project variables that have been created.</div>'
    str += '<div>eg: <strong class="code-higlight">last_contact_date</strong></div>'
    str += '<br/>'

    str += '<div>Note: If matching right, data will be upserted, or it will be ignored.</div>'
    str += '</div>'
    str
  end
end
