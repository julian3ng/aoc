def build_counts(board, counts)
  board.each_with_index do |line, i|
    line.each_with_index do |char, j|
      unless board[i][j] == '.'
        counts[i][j] = ((i - 1)..(i + 1)).sum do |ii|
          ((j - 1)..(j + 1)).count do |jj|
            (ii >= 0) && (jj >= 0) &&
            (ii < line.length) && (jj < line.length) &&
            (ii != i || jj != j) && board[ii][jj] == '@'
          end
        end
      end
    end
  end
end

def part1(filename)
  board = Array.new
  counts = Array.new
  l = nil
  File.readlines(filename, chomp: true).each do |line|
    board << line.chars
    counts << Array.new(line.length)
    l = line.length
  end

  build_counts(board, counts)

  counts.each.sum do |line|
    line.each.count do |c|
      !c.nil? && c < 4
    end
  end
end

p part1('../input/sample.txt')
p part1('../input/input.txt')

def reducible?(counts)
  counts.each do |line|
    line.each do |c|
      return true if c && c < 4
    end
  end

  return false
end

def part2(filename)
  board = Array.new
  counts = Array.new
  l = nil
  File.readlines(filename, chomp: true).each do |line|
    board << line.chars
    counts << Array.new(line.length)
    l = line.length
  end

  build_counts(board, counts)

  removed = 0

  while reducible?(counts)
    board.each_with_index do |line, i|
      line.each_with_index do |_, j|
        if board[i][j] == '@' && counts[i][j] < 4
          board[i][j] = '.'
          counts[i][j] = nil
          removed += 1
        end
      end
    end

    build_counts(board, counts)

    # board.each { |line| puts line.join('') }
    # counts.each { |line| puts line.map { |x| x.nil? ? 'n' : x.to_s }.join('') }
  end

  removed
end

puts part2('../input/sample.txt')
puts part2('../input/input.txt')
