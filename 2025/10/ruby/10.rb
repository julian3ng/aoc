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

def bfs2(goal1, goal2, buttons)
  # strategy: bfs over goal2 first (joltage), and if we made it, check goal1?
  # if we exceed any part of goal2, skip
  visited = Set[[0]]
  q = [[0]]

  until q.empty?
    cur = q.shift
    state1 = cur.reduce(:^)
    state2 = cur.reduce(Array.new(goal2.length, 0)) do |acc, button|
      p [acc, button]
      button.each { acc[it] += 1 }
    end

    return cur.length if state1 == goal1 && state2 == goal2

    buttons.each do |button|
      next_state = cur + [button]
      visited.add(next_state)
      q << next_state
    end
  end
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
      machine, *buttons, joltage = it.split(' ')
      [machine.tr('.#[]', '01  ').reverse.to_i(2),
       buttons.map { it.scan(/\d+/).map(&:to_i) },
       joltage.scan(/\d+/).map(&:to_i)]
    end

    bfs2(specs[0][0], specs[0][2], specs[0][1])
  end
end

sol = Day10.new('../input')
p sol.run_part(1)
# p sol.run_part(1, sample: false)

p sol.run_part(2)
