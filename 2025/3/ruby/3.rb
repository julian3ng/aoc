def max_joltage_1(line)
  a, x = line.chars.slice(0..-2).each_with_index.max_by { |c, i| c }
  b, y = line.chars.slice((x + 1)..-1).each_with_index.max_by { |c, i| c }
  (a + b).to_i
end

def max_joltage_2(line)
  # dp[i, n] is the best score using n numbers ending at index i
  # n <= i
  dp = Array.new(12) { Array.new(line.length) }
  0.upto(11) do |i|
    0.upto(line.length - 1) do |j|
      if i.zero?
        dp[i][j] = line[j].to_i
      else
        dp[i][j] = (dp[i - 1].slice(0...j).max || 0) * 10 + line[j].to_i
      end
    end
  end
  dp[-1].max
end

p File.readlines('../input/input.txt').sum { |s|
  max_joltage_1(s.chomp)
}

p File.readlines('../input/input.txt').sum { |s|
  max_joltage_2(s.chomp)
}
