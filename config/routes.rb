# Copyright (C) 2010-2012, InSTEDD
#
# This file is part of Verboice.
#
# Verboice is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# Verboice is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with Verboice.  If not, see <http://www.gnu.org/licenses/>.

require 'api_constraints'

Verboice::Application.routes.draw do

  mount InsteddTelemetry::Engine => '/instedd_telemetry'

  match '/' => 'home#index',  :as => 'home'

  devise_for :accounts, controllers: {omniauth_callbacks: "omniauth_callbacks"}
  guisso_for :account

  scope '/plugin' do
    Plugin.all.each do |plugin|
      scope plugin.name do
        plugin.hooks[:routes].each { |plugin_routes_block| instance_eval &plugin_routes_block }
      end
    end
  end
  
  resources :call_log_recorded_audios, only: [:update]

  resources :channels do
    resources :queued_calls
    member do
      get :call
      post :enable
      post :disable
    end
  end

  resources :nuntium_channels, except: [:show] do
    put :mark_as_default, on: :member
  end

  resources :feeds, controller: :feed_server do
    member do
      get :recordings
      get 'recording/:recording_id', action: :get_recording, as: :recording
    end
  end

  # Register both shallow and deep routes:
  # - Shallow routes allow for easier path helper methods, such as contact_recorded_audios(@contact) instead of project_contact_recorded_audios(@project, @contact)
  # - Deep routes ensure that form_for directives work as expected, so form_for([@project, @contact]) works no matter it is a creation or an update
  [true, false].each do |shallow|
    resources :projects, :shallow => shallow do
      resources :call_flows
      member do
        post :enqueue_call
        put :update_variables
      end
      
      
      resources :call_flows, except: [:new, :edit] do
        member do
          get :edit_workflow, path: :designer
          put :update_workflow, path: :update_flow
          post :import
          get :export
          get :download_results
          get :oauth
        end
      end

      resources :external_services, except: [:show, :new, :edit] do
        member do
          put :update_manifest
        end
      end

      resources :schedules

      resources :contacts, except: [:show] do
        collection do
          get :search, :action => :index, :as => 'search'
          get :download, as: 'download'
          get :invitable
          post :upload_csv
          post :import_csv
          delete :destroy_from_filter
        end
        member do
          get :calls
          get :queued_calls
        end
      end

      resources :resources do
        collection do
          get :find
        end

        resources :localized_resources do
          member do
            post :save_recording
            get :play_recording
            post :save_file
            get :play_file
          end
        end
      end

      resources :feeds

      resources :scheduled_calls do
        collection do
          post :from_filter, :action => :new, :as => 'from_filter'
        end
      end
      
      resources :call_logs, :path => :calls, :only => :index do |r|
        collection do
          get :download, to: 'call_logs#download_project_call_logs'
          get :generate_zip
          get :download_zip
        end

        member do
          get :download_details
          get 'results/:key', :action => :play_result, :as => 'result'
        end
      end

      resources :feeds
    end
  end

  resources :call_logs, path: :calls, only: [:index, :show] do
    member do
      get :progress
      get 'results/:key', :action => :play_result, :as => 'result'
      get :download_details
    end
    collection do
      put :queued_paused
      put :queued_resumed
    end
  end

  namespace :ext do 
    namespace :services do
      resources :pregnancies do
        collection do
          get :manifest
          post :register
          post :progress
        end
      end
    end

    resources :projects do
      resources :reminder_groups, :only => [:index] do
        post :import, :on => :member
      end

      resources :reminder_schedules do
        get :references_data, :on => :collection
        post 'remove_reminder_channel'
        collection do
          get 'channels_autocomplete'
        end
      end

      resources :pregnancy_reminders
    end
  end  
    

  resource :synthesizer do
    get :voices
  end

  resources :alerts do
    member do
      get :dismiss
    end
  end

  namespace :api, defaults: {format: 'json'} do
    match "call" => "calls#call"
    resources :calls, only: [:destroy] do
      member do
        match :state
        match :redirect
        match :cancel
      end
    end
    get "channels" => "channels#list"
    resources :channels, only: [:create] do
      collection do
        get "all", :action => "all"
        get "all/:id", :action => "get_by_id"
        get ":name", :action => "get"
        put ":name", :action => "update"
        delete ":name", :action => "destroy"
        post ":id/enable", :action => "enable"
        post ":id/disable", :action => "disable"
      end
    end
    resources :projects, only: [:index, :show] do
      resources :project_variables, only: :index
      resources :call_flows, only: [:index, :show]
      resources :contacts, only: [:index, :create] do
        collection do
          get 'by_address/:address', :action => "show_by_address"
          put 'by_address/:address', :action => "update_by_address"
          put 'all', :action => "update_all"
          delete :unregistration
        end
      end
      resources :schedules, only: [:index, :create] do
        collection do
          get ':name', :action => "show"
          put ':name', :action => "update"
          delete ':name', :action => "destroy"
        end
      end

      resources :reminder_groups, only: [:index, :create, :update, :destroy]

      post 'reminder_groups/:id/contacts' => 'reminder_groups#contacts', as: 'register_contact_to_reminder_group'
      
    end

    resources :logs, only: [] do
      collection do
        get ':call_id', action: :list
      end
    end

    get '/contacts/:address/call_logs', controller: :call_logs, action: :index
    resources :call_logs, only: [:index, :show]

    get "call_flows" => "call_flows#list"
  end

  namespace :api2, defaults: {format: 'json'} do

    post "call" => "calls#call"
    post "bulk_call" => 'calls#bulk_call'
    resources :calls, only: [] do
      member do
        match :state
        match :redirect
      end
    end

    resources :channels, only: [:create, :index] do
      collection do
        get ":name", :action => "get"
        put ":name", :action => "update"
        delete ":name", :action => "destroy"
      end

      member do
        put "activate"
        put "deactivate"
      end
    end

    resources :channel_quota, only: [:create, :destroy]

    resources :projects, only: [:index] do
      resources :contacts, only: [:index, :create] do
        collection do
          get 'by_address/:address', :action => "show_by_address"
          put 'by_address/:address', :action => "update_by_address"
          put 'all', :action => "update_all"
          delete :unregistration
        end
      end
      resources :schedules, only: [:index, :create] do
        collection do
          get ':name', :action => "show"
          put ':name', :action => "update"
          delete ':name', :action => "destroy"
        end
      end

      resources :reminder_groups, only: [:index, :create, :update, :destroy], shallow: true
    end

    resources :project_variables, only: [:index]
    resources :call_log_answers, only: [:index]

    resources :logs, only: [] do
      collection do
        get ':call_id', action: :list
      end
    end

    get '/channels/:channel_id/call_logs' => "call_logs#list_by_channel"
    get '/contacts/:address/call_logs', controller: :call_logs, action: :index
    resources :call_logs, only: [:index, :show] do
      member do
        get 'play_audio'
      end
      
      resources :recorded_audios, path: :audios, only: [:index] do
        collection do
          get ':filename', action: :play
        end
      end
    end

    get "call_flows" => "call_flows#list"

    get "accounts" => "accounts#index"

    post '/auth' => 'sessions#create'

    resources :traffics, only: [:index]
  end

  get 'permissions' => 'permissions#index'
  get 'permissions/autocomplete' => 'permissions#autocomplete'
  post 'permissions/add_account' => 'permissions#add_account'
  post 'permissions/update' => 'permissions#update'

  post 'call_simulator/start'
  post 'call_simulator/resume'

  get 'oauth/google' => 'oauth#google', :as => 'google_oauth'
  match 'oauth/google/callback' => 'oauth#google_callback', :as => 'google_callback_oauth'

  root :to => 'home#index'

  get 'terms_and_conditions', :to => redirect('http://instedd.org/terms-of-service/')

  match '/hub/*path' => 'hub#api', format: false
  get '/projects/:project_id/reports' => "reports#index", as: :project_reports
  mount Listings::Engine => "/listings"
  mount InsteddTelemetry::Engine => "/instedd_telemetry"
end
