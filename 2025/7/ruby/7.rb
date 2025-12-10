require_relative '../../utils/ruby/utils'

class Day7 < AocSolution
  def part_1(filename)
    lasers = []
    splits = 0
    File.readlines(filename, chomp: true).each_with_index do |line, i|
      if i == 0
        lasers << line.chars.find_index('S')
      else
        splitters = line.length.times.select { |i| line[i] == '^' }
        next if splitters.length.zero?
        if $DEBUG
          puts "\e[96;6m================================================================\e[m"
          p ['LASERS', lasers]
          p line
          p ['SPLITTERS', splitters]
        end
        missed_lasers = lasers - splitters
        hit_splitters = lasers & splitters
        splits += hit_splitters.length
        next_lasers = (missed_lasers + (hit_splitters.flat_map do |s|
          [s - 1, s + 1]
        end)).uniq

        p ['HITS', hit_splitters] if $DEBUG
        p ['AFTER', next_lasers] if $DEBUG
        p ['ADDING', hit_splitters.length] if $DEBUG
        p ['SPLITS', splits] if $DEBUG
        lasers = next_lasers
      end
    end

    splits
  end

  def part_2(filename)
    board = []
    # table[i][j] contains the number of timelines ending at line i, index j
    table = []
    File.readlines(filename, chomp: true).each do |line|
      board << line unless line.match?(/^\.+$/)
      table << Array.new(line.size, 0) unless line.match?(/^\.+$/)
    end

    start = board[0].chars.find_index('S')
    table[0][start] = 1

    board.drop(1).each_with_index do |line, i|
      ix = i + 1
      line.chars.each_with_index do |char, j|
        if char == '^'
          table[ix][j - 1] += table[ix - 1][j]
          table[ix][j + 1] += table[ix - 1][j]
        else
          table[ix][j] += table[ix - 1][j]
        end
      end
    end

    if $DEBUG
      board.each { |line| p line }
      table.each { |line| p line }
    end

    table[-1].sum
  end
end

day7 = Day7.new('../input')
p day7.run_part(2, sample: false)
