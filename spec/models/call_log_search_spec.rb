require 'spec_helper'

describe CallLogSearch do
  let!(:channel) { Channels::Custom.make }

  def assert_search(text)
    CallLog.search(text, account: channel.account).to_sql
  end  

  describe "keyword search" do
    let(:keyword) { '123' }

    %w(id state direction).each do |column|
      it "should find by call #{column}" do
        assert_search(keyword).should match /call_logs\.#{column} = '#{keyword}'/
      end
    end

    it "should find by address" do
      assert_search(keyword).should match /call_logs.address LIKE '%#{keyword}'/
    end
  end

  describe "search by id" do
    it "should find by call id" do
      assert_search("id:12").should match /call_logs\.id = 12/
    end

    it "should find by id conditions" do
      assert_search("id:>12").should match /call_logs\.id > 12/
      assert_search("id:<=12").should match /call_logs\.id <= 12/
    end
  end

  it "should search by call direction" do
    assert_search("direction:incoming").should match /direction = 'incoming'/
  end

  %w(address caller caller_id).each do |key|
    it "should search by #{key}" do
      assert_search("#{key}:123").should match /address LIKE '%123'/
    end
  end

  describe "search by date" do
    let(:search_date) { '2013-11-14' }
    let(:date) { Time.smart_parse(search_date).utc.strftime "%F %X" }    

    it "should search by after" do
      assert_search("after:#{search_date}").should match /started_at >= '#{date}'/
    end

    it "should search by before" do
      assert_search("before:#{search_date}").should match /started_at <= '#{date}'/
    end
  end

  describe "search by channel" do
    it "should find by channel name" do
      assert_search("channel:\"#{channel.name}\"").should match /channel_id = #{channel.id}/
    end

    context "when channel does not exist" do
      it "should find nothing" do
        assert_search("channel:foooooooo").should match /WHERE \(1 = 1\) AND \(1 = 2\)/
      end
    end
  end

  it "should search by project_id" do
    assert_search("project_id:12").should match /call_logs.project_id = '12'/
  end

  describe "search by project" do
    let(:project) { channel.call_flow.project }

    it "should find by project name" do
      assert_search("project:\"#{project.name}\"").should match /call_logs.project_id = #{project.id}/
    end

    context "when project does not exist" do
      it "should find nothing" do
        assert_search("project:foooooooo").should match /WHERE \(1 = 1\) AND \(1 = 2\)/
      end
    end

    context "when account option is nil" do
      it "should find by project name" do
        CallLog.search("project:\"#{project.name}\"").to_sql.should match /projects.name = '#{project.name}'/
      end
    end
  end

  it "should search by call_flow_id" do
    assert_search("call_flow_id:123").should match /call_logs\.call_flow_id = '123'/
  end

  it "should search by phone_number" do
    assert_search("phone_number:123").should match /address like '%123%'/
  end
end