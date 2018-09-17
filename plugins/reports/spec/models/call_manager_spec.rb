require 'spec_helper'

describe CallManager, type: :model do
  context 'handle' do
    let(:phrase_speech) { 'I live in Phnom Penh, I want to report two chicken died in Kampong Cham' }
    let(:first_audio_file) { '1507648840602' }
    let(:call_id) { 9999 }

    before(:each) do
      Reports::Settings.should_receive(:verboice_first_audio_file).and_return(first_audio_file)
      Service.should_receive(:from_voice_to_speech).with(first_audio_file, call_id).and_return(phrase_speech)
    end

    it 'call finished' do
      Service.should_receive(:from_speech_to_understanding).with(phrase_speech).once

      CallManager.handle_call_finished(call_id)
    end
  end
end
