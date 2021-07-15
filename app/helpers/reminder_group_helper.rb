module ReminderGroupHelper
  def source_attr_description
    str = '<div class="dictionary-tip-wrapper">'
    str += '<div><u>1) Source attribute</u></div>'
    str += '<div>Example: response data</div>'
    str += image_tag('json_example.png', width: 300)
    str += '<div class="d-flex mt-1"><div class="type">Simple attribute:</div> <code class="code-higlight">firstName</code></div>'
    str += '<div class="d-flex mt-1"><div class="type">Nested attribute:</div> <code class="code-higlight">age_years</code></div>'
    str += '<div class="d-flex mt-1"><div class="type">Array attribute:</div> <code class="code-higlight">addresses_0_phoneNumber</code></div>'
    str += '<br/>'

    str += '<div><u>2) Destination attribute</u></div>'
    str += "<div>It is your project variables that have been created in <a href='#{project_contacts_path(@project.id)}' target='_blank'>Phone Book</a> => Manage Phone book\'s columns.</div>"
    str += '<div>eg: <code class="code-higlight">last_contact_date</code></div>'
    str += '<br/>'

    str += '<div>Note: If matching right, data will be upserted, or it will be ignored.</div>'
    str += '</div>'
    str
  end

  def schedule_description
    "<div style='width: 360px;padding-left: 5px;'>Schedule to pull data from yesterday at {number} o'clock</div>"
  end

  def recipient_emails_description
    str = "<div style='width: 400px;padding-left: 5px;'>"
    str += "<div>Notify request error to recipient emails.</div>"
    str += "<div>Note: If more than 1, use comma(,) separated.</div>"
    str += "</div>"
    str
  end
end
