local common = require("common")

-- lines = common.read_lines("../input/6.txt")
-- I don't want to parse :)

times = {47, 84, 74, 67}
distances = {207, 1394, 1209, 1014}
--[[ demo
times = {7, 15, 30}
distances = {9, 40, 200}
]]
-- holding for x means we have x speed for the remainder of the time


function distance_for_hold(race_time, hold_time)
   -- hold_time is exactly the speed
   return (race_time - hold_time) * hold_time
end

function min_win(race_time, distance_record)
   local hold_time = 0
   local distance = distance_for_hold(race_time, hold_time)
   while distance < distance_record do
      hold_time = hold_time + 1
      distance = distance_for_hold(race_time, hold_time)
   end
   return hold_time, distance
end

function ways_to_win(race_time, distance_record)
   return race_time - 2 * min_win(race_time, distance_record) + 1
end

prod = 1
for i=1,#times do
   prod = prod * ways_to_win(times[i], distances[i])
end
print("Part 1:", prod)

time = 47847467
distance = 207139412091014
 


print("Part 2:", ways_to_win(time, distance))

