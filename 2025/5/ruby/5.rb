def part1(filename)
  ranges, values = File.readlines(filename, chomp: true).slice_before('').to_a
  values.shift

  ranges = ranges.map { |r| Range.new(*r.split('-').map(&:to_i), false) }
  values = values.map(&:to_i)

  values.count do |v|
    ranges.any? { |r| r.include?(v) }
  end
end

def part2(filename)
  ranges, = File.readlines(filename, chomp: true).slice_before('').to_a
  ranges = ranges.map { |r| Range.new(*r.split('-').map(&:to_i), false) }

  ranges.sort_by! { |r| [r.max, r.min] }

  groups = ranges.slice_when { |a, b| !a.overlap?(b) }.to_a

  groups.sum { |g| Range.new(g.map(&:min).min, g.map(&:max).max, false).count }
end

p part1('../input/sample.txt')
p part1('../input/input.txt')

p part2('../input/sample.txt')
p part2('../input/input.txt')
