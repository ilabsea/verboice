class Reports::Plugin < Plugin
  routes {
    get "call_finished" => "reports#call_finished"  
   	# get '/projects/:project_id/reports' => "reports#index", as: :project_reports
   	post '/projects/:project_id/reports/upload_excel' => "reports#upload_excel", as: :project_reports_upload_excel
   	post '/projects/:project_id/reports/process_excel' => "reports#process_excel", as: :project_reports_process_excel
   	get '/projects/:project_id/reports/view_excel' => "reports#view_excel", as: :project_reports_view_excel
  }
end
