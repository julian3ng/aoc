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

def process_lines_golfed(lines)
  lines.reduce([50, 0, 0]) do |acc, line|
    dial = acc[0]
    diff = line.tr('LR', '-+').to_i
    div, rem = (diff.abs / 100), diff.remainder(100)
    [
      (dial + rem) % 100,
      acc[1] + ((dial + rem) % 100 == 0 ? 1 : 0),
      acc[2] + div + ((dial > 0 and (dial + rem <= 0 or dial + rem >= 100)) ? 1 : 0)
    ]
  end[1,2]
end

p process_lines(File.readlines("./sample.txt"))
p process_lines(File.readlines("./mine.txt"))
p process_lines(File.readlines("./input.txt"))

p process_lines_golfed(File.readlines("./sample.txt"))
p process_lines_golfed(File.readlines("./mine.txt"))
p process_lines_golfed(File.readlines("./input.txt"))
