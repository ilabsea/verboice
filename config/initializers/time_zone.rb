module ActiveSupport
  class TimeZone
    class << self
      def add_phnom_penh_tz!
        name = 'Phnom Penh'
        @lazy_zones_map[name] = create name, nil, TZInfo::Timezone.get('Asia/Phnom_Penh')
      end
    end
  end
end

ActiveSupport::TimeZone.add_phnom_penh_tz!
