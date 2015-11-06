require 'spec_helper'

describe "Call simulation", integration: true do
  before(:all) do
    @account = Account.make
    @channel = Channels::Custom.create! name: "Local", account: @account, dial_string: "Local/{number}@verboice-integration"
    @channel2 = Channels::Custom.create! name: "Local2", account: @account, dial_string: "Local/{number}@verboice-integration"
    @project = @account.projects.create! name: "Testing", languages: [{
      "language" => "en",
      "voice" => TTS::SystemSynthesizer.instance.voices["en"].first[:id]
    }], default_language: "en", tts_engine: "built-in"
  end

  after(:all) do
    @account.destroy
  end

  self.use_transactional_fixtures = false

  it "makes a call" do
    call_log = make_call @project, @channel, "1", "call_flow_with_play"
    sleep 0.5
    call_log.reload
    call_log.state.should eq(:active)

    wait_call(call_log)
  end

  it "make interactive call" do
    digit = rand(10)
    call_log = make_interactive_call(@project, @channel, "2", "call_flow_with_input") do |session|
      session.send_dtmf digit
    end

    call_log.call_log_answers.includes(:project_variable).where(project_variables: {name: "number"}).first.value.should eq(digit.to_s)
  end

  it "make a bunch of interactive calls" do
    @channel.limit = 100
    @channel.save!

    call_flow = import_call_flow @project, "simulation_interactive_call_flow"

    # HACK: make a single call just to make sure resources are generated properly
    call_log = @channel.call 1000, call_flow_id: call_flow.id
    wait_call(call_log)

    call_logs = 1000.times.map do |i|
      @channel.call 1000 + i, call_flow_id: call_flow.id
    end

    zero = 0

    loop do
      sleep 5
      active_calls = @channel.active_calls
      puts "Active calls: #{active_calls}"      
      if active_calls == 0
        zero += 1
      else
        zero = 0
      end
      break if zero == 5
    end

    call_logs.map &:reload
    call_logs_by_state = call_logs.group_by(&:state)
    call_logs_by_state.each do |state, calls|
      puts "#{state}: #{calls.count}"
    end

    call_logs_by_state[:completed].count.should eq(1000)
  end

  it "make a parallel bunch of interactive calls" do
    @channel.limit = 100
    @channel.save!

    @channel2.limit = 100
    @channel2.save!

    call_flow = import_call_flow @project, "many_input_simulation_interactive_call_flow"

    # HACK: make a single call just to make sure resources are generated properly
    call_log = @channel.call 1000, call_flow_id: call_flow.id
    wait_call(call_log)

    not_before = Time.now + 2.minute

    call_logs = 1000.times.map do |i|
      [
        @channel.call(1000 + i, call_flow_id: call_flow.id, not_before: not_before),
        @channel2.call(2000 + i, call_flow_id: call_flow.id, not_before: not_before)
      ]
    end.flatten

    # just keep it waits till the time arrived
    while Time.now < not_before
      sleep 5
    end

    zero = 0

    loop do
      channel1_active_calls = @channel.active_calls
      channel2_active_calls = @channel2.active_calls
      puts "Active calls: channel1: #{channel1_active_calls}, channel2: #{channel2_active_calls}"      
      if channel1_active_calls + channel2_active_calls == 0
        zero += 1
      else
        zero = 0
      end

      break if zero == 5

      sleep 3
    end

    call_logs.map &:reload
    call_logs_by_state = call_logs.group_by(&:state)
    call_logs_by_state.each do |state, calls|
      puts "#{state}: #{calls.count}"
    end

    call_logs_by_state[:completed].count.should eq(2000)
  end

  def import_call_flow(project, call_flow_name)
    call_flow = project.call_flows.find_by_name(call_flow_name)
    unless call_flow
      call_flow = project.call_flows.create! name: call_flow_name
      VrzContainer.for(call_flow).import File.expand_path("../../fixtures/call_flows/#{call_flow_name}.zip", __FILE__)
    end
    call_flow
  end

  def make_call(project, channel, address, call_flow_name)
    call_flow = import_call_flow(project, call_flow_name)
    channel.call address, call_flow_id: call_flow.id
  end

  def make_interactive_call(project, channel, address, call_flow_file, &block)
    call_log = make_call(project, channel, address, call_flow_file)
    asterisk_session(&block)
    call_log.reload
    call_log
  end

  def wait_call(call_log)
    60.times do
      sleep 0.5
      call_log.reload
      break if call_log.state == :completed
    end

    call_log.state.should eq(:completed)
  end

  class TestSession
    def initialize(socket)
      @socket = socket
    end

    def answer
      @socket.puts "ANSWER"
      @socket.gets
    end

    def send_dtmf(digits)
      @socket.puts "EXEC SendDTMF #{digits}"
      @socket.gets
    end

    def wait_for_digit(timeout = 3000)
      @socket.puts "WAIT FOR DIGIT #{timeout}"
      @socket.gets
    end
  end

  def asterisk_session
    server = TCPServer.new(5000)
    begin
      socket = Timeout::timeout(5) { server.accept }
      loop do
        line = socket.gets.strip
        break if line.empty?
      end

      session = TestSession.new(socket)
      session.answer

      yield session
      
      socket.close
    ensure
      server.close
    end
  end
end