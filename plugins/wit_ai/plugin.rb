class WitAi::Plugin < Plugin
  
  collection_tab "/wit_ais_tab"

  routes {
  	resources :wit_ais
    get "call_finished" => "wit_ais#call_finished"  
  }
end
