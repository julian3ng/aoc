def part_1(filename)
  input = []
  File.readlines(filename, chomp: true).each do |line|
    input << line.split(' ')
  end

  input = input.transpose

  input.sum do |equation|
    op = equation.pop.to_sym
    equation.map(&:to_i).reduce(op)
  end
end

p part_1('../input/input.txt')

def part_2(filename)
  input = []
  File.readlines(filename, chomp: true).each do |line|
    input << line.split('')
  end

  input = input.transpose

  groups = [[]]
  ops = []
  input.each do |line|
    if line.all? { |c| c == ' ' }
      groups << []
    else
      maybe_op = line.pop
      if maybe_op != ' ' then
        ops << maybe_op.to_sym
      end

      groups[-1] << line.join.to_i
    end
  end

  groups.zip(ops).sum do |group, op|
    group.reduce(op)
  end
end

p part_2('../input/input.txt')
