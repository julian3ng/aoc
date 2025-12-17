require 'z3'
require_relative '../../utils/ruby/utils'

def bfs(goal, buttons)
  puts "Searching for #{goal.to_s(2)} with #{buttons.map { it.to_s(2) }}" if $DEBUG
  visited = Set[[]]
  q = [[]]
  until q.empty?
    cur = q.shift
    p [cur.map { it.to_s(2) }, cur.reduce(:^).to_s(2)] if $DEBUG
    return cur.length if cur.reduce(:^) == goal

    buttons.each do |button|
      next if cur.include?(button)

      next_state = cur + [button]
      visited.add(next_state)
      q << next_state
    end
  end

  raise StandardError, "No path found for #{goal}"
end

def one_hot(xs, n)
  r = Array.new(n, 0)
  xs.each { |x| r[x] = 1 }
  r
end

class Day10 < AocSolution
  def part_1(filename, sample)
    specs = File.readlines(filename, chomp: true).map do
      machine, *buttons, joltage = it.split(' ')
      [machine.tr('.#[]', '01  ').reverse.to_i(2),
       buttons.map{ it.scan(/\d+/).map(&:to_i).reduce(0){ |acc, i| acc | (1 << i) }},
       joltage.scan(/\d+/).map(&:to_i)]
    end

    specs.sum do |machine, buttons, joltage|
      bfs(machine, buttons)
    end
  end

  def part_2(filename, sample)
    specs = File.readlines(filename, chomp: true).map do
      _, *buttons, joltage = it.split(' ')
      joltage = joltage.scan(/\d+/).map(&:to_i)
      buttons = buttons.map { one_hot(it.scan(/\d+/).map(&:to_i), joltage.length) }
      [buttons, joltage]
    end

    specs.sum do |buttons, joltages|
      vars = buttons.map.with_index { |_, i| Z3.Int(i.to_s) }
      solver = Z3::Optimize.new
      vars.each { solver.assert(it >= 0) }

      buttons.transpose.zip(joltages).each do |row, j|
        expr = row.zip(vars).inject(0) do |acc, (b, v)|
          acc + v * b
        end
        solver.assert(j == expr)
      end

      solver.minimize(vars.inject(&:+))

      if solver.unsatisfiable?
        # p buttons, joltage
        raise StandardError, 'Something is wrong!'
      end

      model = solver.model
      model.to_h.values.map(&:to_i).sum
    end
  end
end

sol = Day10.new('../input')
# p sol.run_part(1)
# p sol.run_part(1, sample: false)

p sol.run_part(2, sample: false)
