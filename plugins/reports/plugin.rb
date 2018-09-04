class Reports::Plugin < Plugin
  routes {
    get "call_finished" => "reports#call_finished"  
   	namespace :api2, defaults: {format: 'json'} do
   		resources :reports, only: [:index]
   	end
  }
end
