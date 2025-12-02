def process_lines(lines)
  pw1 = 0
  pw2 = 0
  dial = 50
  diff = 0
  lines.each do |line|
    diff = line.sub('L', '-').sub('R', '').to_i
    div, rem = (diff / 100.0).truncate.abs, diff.remainder(100)
    pw2 += div

    if dial > 0 and (dial + rem <= 0 or dial + rem >= 100)
      pw2 += 1
    end
    if (dial + rem) % 100 == 0
      pw1 += 1
    end

    # p [line.chomp, dial, diff, div, rem, dial + rem, pw1, pw2]
    dial += rem
    dial %= 100
  end

  return [pw1, pw2]
end

p process_lines(File.readlines("./sample.txt"))
p process_lines(File.readlines("./mine.txt"))
p process_lines(File.readlines("./input.txt"))
