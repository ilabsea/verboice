class BasicObject
  def self.subclasses
    @subclasses ||= []
  end

  def self.inherited a_subclass
    subclasses << a_subclass
    super
  end

  def self.all_leaf_subclasses
    all_subclasses.select do |a_class|
      a_class.subclasses.empty?
    end
  end

  def self.all_subclasses

    # This is the equivalent of doing:
    # scan = subclasses.clone
    # subclasses.each do |a_subclass|
    #  scan << a_subclass.all_subclasses
    # end
    # scan.flatten
    # But faster...

    scan = subclasses.clone
    index = 0
    while (index < scan.size) do
      scan << scan[index].subclasses
      scan.flatten!
      index += 1
    end
    scan
  end
end