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

  resources :call_log_recorded_audios, only: [:update]

  resources :channels do
    resources :queued_calls
    member do
      get :call
    end
  end

  resources :nuntium_channels, except: [:show] do
    put :mark_as_default, on: :member
  end

  match '/' => 'home#index',  :as => 'home'

  devise_for :accounts

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
        get :invitable, on: :collection
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
      
      resources :call_logs, :path => :calls, :only => :index do |r|
        collection do
          get :download, to: 'call_logs#download_project_call_logs'
          get :generate_zip
          get :download_zip
        end
      end

      resources :feeds
    end
  end

  resources :call_logs, path: :calls do
    member do
      get :progress
      get 'results/:key', :action => :play_result, :as => 'result'
      get :download_details
    end
    collection do
      get :queued
      put :queued_paused
      put :queued_resumed
      get :download
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

  namespace :api, defaults: {format: 'json'} do
    match "call" => "calls#call"
    resources :calls, only: [] do
      member do
        match :state
        match :redirect
      end
    end
    get "channels" => "channels#list"
    resources :channels, only: [:create] do
      collection do
        get ":name", :action => "get"
        put ":name", :action => "update"
        delete ":name", :action => "destroy"
      end
    end
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

    match "call" => "calls#call"
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

    resources :logs, only: [] do
      collection do
        get ':call_id', action: :list
      end
    end

    get '/channels/:channel_id/call_logs' => "call_logs#list_by_channel"
    get '/contacts/:address/call_logs', controller: :call_logs, action: :index
    resources :call_logs, only: [:index, :show]

    get "call_flows" => "call_flows#list"

    get "accounts" => "accounts#index"

    post '/auth' => 'sessions#create'

    resources :traffics, only: [:index]
  end

  post 'call_simulator/start'
  post 'call_simulator/resume'

  get 'oauth/google' => 'oauth#google', :as => 'google_oauth'
  match 'oauth/google/callback' => 'oauth#google_callback', :as => 'google_callback_oauth'

  root :to => 'home#index'

  get 'terms_and_conditions', :to => redirect('/')
end
