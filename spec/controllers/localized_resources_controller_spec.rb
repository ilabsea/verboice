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

require 'spec_helper'

describe LocalizedResourcesController do
  include Devise::TestHelpers

  before(:each) do
    @account = Account.make
    @project = @account.projects.make
    @resource = @project.resources.make

    sign_in @account
  end

  describe "recording" do

    before(:each) do
      @localized_resource = RecordLocalizedResource.make :resource => @resource
    end

    describe "POST save_recording" do

      it "should save recording" do
        request.env['RAW_POST_DATA'] = 'some recording'
        post :save_recording, {:project_id => @project.id, :resource_id => @resource.id, :id => @localized_resource.id}
        @localized_resource.reload.recorded_audio.should eq('some recording')
      end

      it "should succeed" do
        post :save_recording, {:project_id => @project.id, :resource_id => @resource.id, :id => @localized_resource.id}
        response.should be_ok
      end

    end

    describe "GET play_recording" do

      it "should return audio" do
        controller.should_receive(:send_data).with(@localized_resource.recorded_audio).and_return{controller.render :nothing => true}
        get :play_recording, {:project_id => @project.id, :resource_id => @resource.id, :id => @localized_resource.id}
      end

    end

  end

  describe "Upload file" do

    before(:each) do
      @localized_resource = UploadLocalizedResource.make :resource => @resource
    end

    describe "POST save_file" do

      it "should invalid audio file when mime type of file upload is not .mp3 or .wav" do
        request.env['CONTENT_TYPE'] = "application/pdf"
        request.env['RAW_POST_DATA'] = "some file"
        post :save_file, {:project_id => @project.id, :resource_id => @resource.id, :id => @localized_resource.id, :filename => 'filename'}
        response.body.should eq("Invalid audio file")
        response.should be_ok
      end

      it "should save mp3 file" do
        request.env['CONTENT_TYPE'] = 'audio/mpeg'
        controller.stub!(:convert_to_8000_hz_wav).with(anything(), 'audio/mpeg').and_return('mp3 content')

        post :save_file, {:project_id => @project.id, :resource_id => @resource.id, :id => @localized_resource.id, :filename => 'filename.mp3'}
        @localized_resource.reload.uploaded_audio.should eq('mp3 content')
        @localized_resource.reload.filename.should eq('filename.mp3')
        response.body.should eq("OK")
      end

      it "should save wav filename" do
        request.env['CONTENT_TYPE'] = "audio/x-wav"
        controller.stub!(:convert_to_8000_hz_wav).with(anything(), 'audio/x-wav').and_return('wav content')

        post :save_file, {:project_id => @project.id, :resource_id => @resource.id, :id => @localized_resource.id, :filename => 'filename.wav'}
        @localized_resource.reload.uploaded_audio.should eq('wav content')
        @localized_resource.reload.filename.should eq('filename.wav')
        response.body.should eq("OK")
        response.should be_ok
      end
    end

    describe "GET play_file" do

      it "should return file" do
        controller.should_receive(:send_data).with(@localized_resource.uploaded_audio, :filename => @localized_resource.filename).and_return{controller.render :nothing => true}
        get :play_file, {:project_id => @project.id, :resource_id => @resource.id, :id => @localized_resource.id}
      end

    end
  end

end