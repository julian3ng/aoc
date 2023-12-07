local common = require("common")

lines = common.read_lines("../input/4.txt")

function hashset(t)
   local h = {}
   for _,x in ipairs(t) do
      h[x] = true
   end
   return h
end

function count_contains(numbers, container)
   local count = 0
   for _,n in ipairs(numbers) do
      if container[n] then
         count = count + 1
      end
   end
   return count
end

local sum1 = 0
local part2_cards = {}
for i=1,#lines do
   part2_cards[i] = 1
end
for card_num, line in ipairs(lines) do
   local card, entry = table.unpack(line:split(':'))
   local winners, container = table.unpack(entry:split('|'))
   local winning_numbers = table.map(winners:split(' '), tonumber)
   local contained_numbers = hashset(table.map(container:split(' '), tonumber))
   local c = count_contains(winning_numbers, contained_numbers)
   --print(card_num, c)
   if c > 0 then
      sum1 = sum1 + 2 ^ (c - 1)
      for i = card_num+1, card_num + c do
         if part2_cards[i] then
            part2_cards[i] = part2_cards[i] + part2_cards[card_num]
         else
            part2_cards[i] = part2_cards[card_num]
         end
         --print("\t", i, part2_cards[i])
      end
   end
end

print("Part 1:", sum1)
local sum2 = 0
for i,count in ipairs(part2_cards) do
   sum2 = sum2 + count
end
print("Part 2:", sum2)
