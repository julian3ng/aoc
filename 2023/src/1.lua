local common = require("common")

lines = common.read_lines("../input/1.txt")

function get_calibration_value(line)
   local s = line:match("[0-9]")
   local r = line:reverse():match("[0-9]")
   if s and r then
      return tonumber(s..r)
   end
   return 0
end


local sum = 0
for i,line in ipairs(lines) do
   sum = sum + get_calibration_value(line)
end

print("Part 1:", sum)

lookup = {
   one="1",
   two="2",
   three="3",
   four="4",
   five="5",
   six="6",
   seven="7",
   eight="8",
   nine="9",
   eno="1",
   owt="2",
   eerht="3",
   ruof="4",
   evif="5",
   xis="6",
   neves="7",
   thgie="8",
   enin="9",
}

function find_first_match(line)
   local best, match = #line+1, nil
   
   local mstart, mfinish, mdigit = line:find("([1-9])")
   if mdigit and mstart < best then
      best = mstart
      match = mdigit
   end

   for word, num in pairs(lookup) do
      local wstart = line:find(word)
      if wstart and wstart < best then
         best = wstart
         match = num
      end
   end

   return match
end

local sum2 = 0
for i,line in ipairs(lines) do
   local fm = find_first_match(line)
   local lm = find_first_match(line:reverse())
   if fm and lm then
      sum2 = sum2 + tonumber(fm..lm)
   end
end

print("Part 2:", sum2)
