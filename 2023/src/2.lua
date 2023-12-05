local common = require("common")

lines = common.read_lines("../input/2.txt")

valid_games = {}
thresholds = {
   red=12,
   green=13,
   blue=14,
}
powers = {}
function get_pulls(game)
   local pulls = {}
   for pull in game:gmatch("([^;]+)") do
      table.insert(pulls,pull)
   end
   return pulls
end
function print_table(t)
   print('-----')
   for k,v in pairs(t) do
      print(k, '\t', v)
   end
   print('=====')
end


for game_id,line in ipairs(lines) do
   local game = line:match("Game %d+: (.*)")
   if game then
      local pulls = get_pulls(game)

      local maxes = {
         red=0,
         green=0,
         blue=0,
      }

      for _,pull in ipairs(pulls) do
         for color,count in pairs(maxes) do
            maxes[color] = math.max(maxes[color], tonumber(pull:match("(%d+) "..color)) or 0)
         end
         print_table(maxes)
      end
      print('xxxxxx')

      powers[game_id] = maxes["red"] * maxes["green"] * maxes["blue"]

      local valid = true
      for color,max in pairs(maxes) do
         if max > thresholds[color] then
            valid = false
            break
         end
      end

      if valid then
         table.insert(valid_games, game_id)
      end
   end
end

sum = 0
for _,v in pairs(valid_games) do
   sum = sum + v
end

sum2 = 0
for _,v in pairs(powers) do
   sum2 = sum2 + v
end
print("Part 1: ", sum)
print("Part 2: ", sum2)
