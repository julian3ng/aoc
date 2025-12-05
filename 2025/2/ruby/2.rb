def parse_input(s)
  s.split(',').map { |inner| inner.split('-').map(&:to_i) }
end

def naive(a, b)
  (a..b).reduce(0) do |acc, x|
    /^(\d+)\1$/.match(x.to_s) ? acc + x : acc
  end
end

def naive2(a, b)
  (a..b).reduce(0) do |acc, x|
    /^(\d+)\1+$/.match(x.to_s) ? acc + x : acc
  end
end

def golf(s)
  s.split(/[,-]/).each_slice(2).reduce([0, 0]) { |(s1,s2),(a,b)|
    (a.to_i..b.to_i).reduce([s1, s2]) { |(acc1,acc2),x|
      [
        /^(\d+)\1$/.match(x.to_s) ? acc1+x : acc1,
        /^(\d+)\1+$/.match(x.to_s) ? acc2+x : acc2
      ]
    }
  }
end

p golf(File.read('../input/input.txt'))

# p parse_input(File.read('../input/input.txt'))
#     .map{ |arr| naive(arr[0], arr[1]) }
#     .sum

# p parse_input(File.read('../input/input.txt'))
#     .map{ |arr| naive2(arr[0], arr[1]) }
#     .sum
