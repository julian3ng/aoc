local common = require("common")

lines = common.read_lines("../input/7.txt")

function parse_line(line)
   local parts = line:split(" ")
   return table.unpack(parts)
end

function string.chars(str)
   local i = 0
   local l = #str
   return function()
      i = i + 1
      if i > l then return nil end
      return string.char(str:byte(i))
   end
end

function compile(hand_str, bid)
   local hand = {
      str=hand_str,
      cards={},
      kind='',
      bid=bid
   }
   for c in hand_str:chars() do
      if hand.cards[c] == nil then
         hand.cards[c] = 1
      else
         hand.cards[c] = hand.cards[c] + 1
      end
   end

   local counts = {}
   for _,count in pairs(hand.cards) do
      table.insert(counts, count)
   end

   table.sort(counts, function (a,b) return a > b end)

   for _,count in ipairs(counts) do
      hand.kind = hand.kind..count
   end

   return hand
end

local card_ranks = {
   A=14,
   K=13,
   Q=12,
   J=11,
   T=10,
   ['9']=9,
   ['8']=8,
   ['7']=7,
   ['6']=6,
   ['5']=5,
   ['4']=4,
   ['3']=3,
   ['2']=2,
}

local hand_mt = {
   __tostring = function(self)
      return self.str..': '..self.kind
   end,
   __lt = function(self, other)
      if self.kind < other.kind then
         return true
      elseif self.kind == other.kind then
         for i=1,5 do
            local r1 = card_ranks[self.str:sub(i,i)]
            local r2 = card_ranks[other.str:sub(i,i)]
            if r1 < r2 then
               return true
            elseif r1 > r2 then
               return false
            end
         end
      else
         return false
      end
   end
}

function make_hand(hand_str, bid)
   local hand = compile(hand_str, bid)
   return setmetatable(hand, hand_mt)
end


local hands = {}
for i,line in ipairs(lines) do
   table.insert(hands, make_hand(parse_line(line)))
end

table.sort(hands)

sum = 0
for i,hand in ipairs(hands) do
   --print(hand, i, hand.bid, i*hand.bid)
   sum = sum + i*hand.bid
end
print("Part 1:", sum)

function compile2(hand_str, bid)
   local hand = {
      str=hand_str,
      cards={},
      kind='',
      bid=bid
   }
   for c in hand_str:chars() do
      if hand.cards[c] == nil then
         hand.cards[c] = 1
      else
         hand.cards[c] = hand.cards[c] + 1
      end
   end

   local counts = {}
   local jokers = hand.cards['J'] or 0
   for card,count in pairs(hand.cards) do
      if card ~= 'J' then
         table.insert(counts, count)
      end
   end

   table.sort(counts, function (a,b) return a > b end)

   if counts[1] then
      counts[1] = counts[1] + jokers
   else
      counts[1] = jokers
   end

   for _,count in ipairs(counts) do
      hand.kind = hand.kind..count
   end

   return hand
end

local card_ranks2 = {
   A=14,
   K=13,
   Q=12,
   J=1,
   T=10,
   ['9']=9,
   ['8']=8,
   ['7']=7,
   ['6']=6,
   ['5']=5,
   ['4']=4,
   ['3']=3,
   ['2']=2,
}

local hand_mt2 = {
   __tostring = function(self)
      return self.str..': '..self.kind
   end,
   __lt = function(self, other)
      if self.kind < other.kind then
         return true
      elseif self.kind == other.kind then
         for i=1,5 do
            local r1 = card_ranks2[self.str:sub(i,i)]
            local r2 = card_ranks2[other.str:sub(i,i)]
            if r1 < r2 then
               return true
            elseif r1 > r2 then
               return false
            end
         end
      else
         return false
      end
   end
}

function make_hand2(hand_str, bid)
   local hand = compile2(hand_str, bid)
   return setmetatable(hand, hand_mt2)
end

local hands2 = {}
for i,line in ipairs(lines) do
   table.insert(hands2, make_hand2(parse_line(line)))
end

table.sort(hands2)

sum = 0
for i,hand in ipairs(hands2) do
   print(hand, i, hand.bid, i*hand.bid)
   sum = sum + i*hand.bid
end
print("Part 2:", sum)
