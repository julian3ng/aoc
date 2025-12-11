require_relative '../../utils/ruby/utils'

class Day9 < AocSolution
  def part_1(filename, sample: true)
    points = File.readlines(filename, chomp: true).map { it.scan(/\d+/).map(&:to_i) }
    points.combination(2).map { |(x1, y1), (x2, y2)| ((x1-x2).abs+1) * ((y1-y2).abs+1) }.max
  end

  def part_2(filename, sample: true)
    points = File.readlines(filename, chomp: true).map { it.scan(/\d+/).map(&:to_i) }
    edges = points.each_cons(2).to_a.append([points[-1], points[0]])
    rects = points.combination(2).sort_by { |(x1, y1), (x2, y2)| ((x1-x2).abs+1) * ((y1-y2).abs+1) }.reverse!
    rects.each do |(x1, y1), (x2, y2)|
      l, r = [x1, x2].minmax
      u, d = [y1, y2].minmax
      return (r - l + 1) * (d - u + 1) if edges.all? do |(ex1, ey1), (ex2, ey2)|
        el, er = [ex1, ex2].minmax
        eu, ed = [ey1, ey2].minmax
        # miss
        er <= l || el >= r || ed <= u || eu >= d
      end
    end
  end
end

sol = Day9.new('../input')
p sol.run_part(1, sample: true)
p sol.run_part(1, sample: false)

p sol.run_part(2, sample: true)
p sol.run_part(2, sample: false)
