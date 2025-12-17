require 'z3'

machine, *buttons, joltages = s.split(' ')

def one_hot(xs, n)
  r = Array.new(n, 0)
  xs.each { r[it] = 1 }
  r
end

machine = machine.tr('.#[]', '01  ').reverse.strip.scan(/\d/).map(&:to_i)
joltages = joltages.scan(/\d+/).map(&:to_i)
buttons = buttons.map { it.scan(/\d+/).map(&:to_i) }
            .map { one_hot(it, joltages.length) }
vars = buttons.map.with_index { |_,i| Z3.Int(i.to_s) }

solver = Z3::Optimize.new

vars.each { solver.assert(it >= 0) }

buttons.transpose.zip(joltages, machine).each do |row, joltage, parity|
  expr = row.zip(vars).inject(0) do |acc, (b, v)|
    acc + v * b
  end
  solver.assert(joltage == expr)
end

solver.minimize(vars.inject(&:+))

if solver.satisfiable?
  model = solver.model

  p model.to_h.values.map(&:to_i).sum

end
