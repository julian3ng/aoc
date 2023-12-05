local common = require("common")

lines = common.read_lines("../input/3.txt")

function symbol_in(y, x1, x2)
   return lines[y]:sub(x1, x2):match("[^.%d]")
end

--[[
   y is line num
   x1, x2 are the first and last index of the block we want to check
]]
function check_neighbors(y, x1, x2)
   local lower, higher = math.max(x1-1, 1), math.min(x2+1, #lines[y])
   if y == 1 then
      return symbol_in(y, lower, higher) or symbol_in(y+1, lower, higher)
   elseif y == #lines then
      return  symbol_in(y-1, lower, higher) or symbol_in(y, lower, higher)
   else
      return symbol_in(y-1, lower, higher) or symbol_in(y, lower, higher) or symbol_in(y+1, lower, higher)
   end
end

sum = 0
for y, line in ipairs(lines) do
   for x1, x2 in line:gfind("%d+") do
      local num = tonumber(line:sub(x1, x2))
      local part = check_neighbors(y, x1, x2)
      if part then
         sum = sum + num
      end
   end
end
print("Part 1:", sum)

nums = {}
gears = {}

for y, line in ipairs(lines) do
   table.insert(gears, {})
   table.insert(nums, {})
   for x1, x2 in line:gfind("%d+") do
      local num = line:sub(x1, x2)
      nums[y][x1] = num
   end

   for x, _ in line:gfind("*") do
      gears[y][x] = "*"
   end
end

gearsum = 0
for y, row in pairs(gears) do
   for x, _ in pairs(row) do
      print('-=*=-', y, x, '-=*=-')
      local gear_nums = {}
      for i=x-3,x+1 do
         local required_length = i < x and x - i or 0
         local a = nums[y-1][i]
         local b = nums[y][i]
         local c = nums[y+1][i]
         if a and #a >= required_length then
            --print(y-1, i, a)
            table.insert(gear_nums, a)
         end

         if b and #b >= required_length then
            --            print(y, i, b)
            table.insert(gear_nums, b)
         end

         if c and #c >= required_length then
            --print(y+1, i, c)
            table.insert(gear_nums, c)
         end
      end
      --print("\n")
      if #gear_nums == 2 then
         gearsum = gearsum + gear_nums[1] * gear_nums[2]
      end
   end
end
print("Part 2:", gearsum)
