require 'pathname'

class AocSolution
  def initialize(input_dir)
    @input_dir = input_dir
    @sample_filename = Pathname(input_dir) + 'sample.txt'
    @input_filename = Pathname(input_dir) + 'input.txt'

    @sample_filename = nil unless File.exist? @sample_filename
    @input_filename = nil unless File.exist? @input_filename
  end

  def has_part_1
    self.class.method_defined? :part_1
  end

  def has_part_2
    self.class.method_defined? :part_2
  end

  def run_part(n, sample: true)
    raise StandardError, "No sample.txt in #{@input_dir}" if sample && @sample_filename.nil?
    raise StandardError, "No input.txt in #{@input_dir}" if !sample && @input_filename.nil?

    method_name = "part_#{n}".to_sym
    public_send(method_name, sample ? @sample_filename : @input_filename, sample: sample) if self.class.method_defined? method_name
  end
end
