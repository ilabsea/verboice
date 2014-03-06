require 'spec_helper'

describe CallLogHelper do
  describe '.render_csv_download?' do
    before(:each) do
      @logs = double('logs', count: 0)
      @audios = double('audios', count: 2)
      CallLog.should_receive(:search).and_return(@logs)
      CallLogRecordedAudio.should_receive(:where).with(call_log_id: @logs).and_return(@audios)
    end

    it "should be false" do
      helper.render_csv_download?.should eq false
    end

    context "when over CSV_MAX_ROWS" do
      before { @logs.stub(count: 270000) }
      it "should be true" do
        helper.render_csv_download?.should eq true
      end
    end

    context "when no recorded audios" do
      before { @audios.stub(count: 0) }
      it "should be true" do
        helper.render_csv_download?.should eq true
      end
    end
  end
end