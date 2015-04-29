require 'spec_helper'

describe 'ActiveSupport' do
  describe 'TimeZone' do
    let(:phnom_penh_tz) { ActiveSupport::TimeZone['Phnom Penh'] }

    it 'should create phnom penh timezone' do
      phnom_penh_tz.should_not be_nil
    end

    it 'should include phnom penh timezone' do
      ActiveSupport::TimeZone.all.should include(phnom_penh_tz)
    end
  end
end
