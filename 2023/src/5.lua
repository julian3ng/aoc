local common = require("common")

lines = common.read_lines("../input/5.txt")

seeds = {}
sections = {}
for i,line in ipairs(lines) do
   if i == 1 then
      local _, raw_seeds = table.unpack(line:split(":"))
      seeds = table.map(raw_seeds:split(" "), tonumber)
   elseif #line == 0 then
   elseif line:sub(#line) == ":" then
      table.insert(sections, {})
   else
      table.insert(sections[#sections], table.map(line:split(" "), tonumber))
   end
end


local locations = {}
for i,seed in ipairs(seeds) do
   local cur = seed
   for si, section in ipairs(sections) do
      for ti, transform in ipairs(section) do
         dest, src, len = table.unpack(transform)
         if src <= cur and cur <= src + len then
            if (i == 3) then
               print(cur, '-->', dest+(cur-src), 'via', src..'-'..(src+len)..'/'..(dest-src))
            end
            cur = dest + (cur - src)
            break
         end
      end
   end
   locations[seed] = cur
end

min_loc = 999999999999
for seed, loc in pairs(locations) do
   --print(seed, loc)
   if loc < min_loc then
      min_loc = loc
   end
end
print("Part 1:", min_loc)

-- CONFIRM THERE ARE NO OVERLAPPING CHANGES
function overlaps(raw_interval_1, raw_interval_2)
   local _, start1, len1 = table.unpack(raw_interval_1)
   local _, start2, len2 = table.unpack(raw_interval_2)
   return (start1 < start2 and start1 + len1 > start2) or
      (start1 > start2 and start2 + len2 > start1)
end

for s,section in pairs(sections) do
   for i,raw_interval_1 in pairs(section) do
      for j,raw_interval_2 in pairs(section) do
         if i ~= j then
            assert(not overlaps(raw_interval_1, raw_interval_2))
         end
      end
   end
end

seed_ranges = {}
for i=1,#seeds,2 do
   --print("RAW SEED", seeds[i], seeds[i+1])
   table.insert(seed_ranges, { seeds[i], seeds[i] + seeds[i+1] })
end

intervals = {}
print('aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa')
for si, section in ipairs(sections) do
   table.insert(intervals, {})
   for ti, transform in ipairs(section) do
      local dst, src, len = table.unpack(transform)
      local delta = dst - src
      local t1, t2 = src, src + len
      table.insert(intervals[#intervals], { t1, t2, delta })
   end
   --pprint(intervals[si])
end
print('bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb')

function print_table(t, prefix)
   if t == nil then print(nil) return end
   if prefix == nil then prefix = '' end
   for k,v in pairs(t) do
      if type(v) == 'table' then
         print(prefix, k)
         print_table(v, prefix..'\t')
      else
         print(prefix, k, v)
      end
   end
end

function print_table_compact(t)
   if t == nil then io.write('nil') return end
   io.write('[')
   for i,v in pairs(t) do
      if type(v) == 'table' then
         print_table_compact(v)
      else
         io.write(v)
      end
      if i ~= #t then
         io.write(', ')
      end
   end
   io.write(']')
end

function pprint(t)
   print_table_compact(t)
   io.write('\n')
end

function apply_change(range, change)
   local range_l, range_r = table.unpack(range)
   local change_l, change_r, delta = table.unpack(change)
   local remainders = {}
   local update = nil
   if change_l <= range_l and range_r <= change_r then
      update = { range_l+delta, range_r+delta }
   elseif range_l <= change_l and change_r <= range_r then
      if range_l ~= change_l then
         table.insert(remainders, { range_l, change_l })
      end
      update = { change_l+delta, change_r+delta }
      if change_r ~= range_r then
         table.insert(remainders, { change_r, range_r })
      end
   elseif change_l < range_l and range_l < change_r and change_r < range_r then
      update = { range_l+delta, change_r+delta }
      table.insert(remainders, { change_r, range_r })
   elseif range_l < change_l and change_l < range_r and range_r < change_r then
      update = { change_l+delta, range_r+delta }
      table.insert(remainders, { range_l, change_l })
   else
      update = nil
      table.insert(remainders, { range_l, range_r })
   end

   return update, remainders
end


-- apply_change(range, change) returns an update and a remainder.
-- the remainder should go back into the pool of ranges for the rest of the
-- changes to apply to, and the update should get added to updates
function apply_changes_to_range(range, changes)
   local cur_ranges = {range}
   local updates = {}
   for i,_ in pairs(cur_ranges) do
      for _,change in pairs(changes) do
         local cur_range = cur_ranges[i]
         if cur_range == nil then
            break
         end
         local update, remainders = apply_change(cur_range, change)
         if update ~= nil then
            --print('uuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuu')
            --pprint(cur_range)
            -- print('\tupdating to...')
            -- pprint(update)
            -- pprint(remainders)
            -- print('\tdue to...', table.unpack(change))
            table.insert(updates, update)
            cur_ranges[i] = nil
            for _,rem in pairs(remainders) do
               table.insert(cur_ranges, rem)
            end
         end
      end
   end
   for _,unchanged in pairs(cur_ranges) do
      table.insert(updates, unchanged)
   end
   return updates
end

function apply_changes_to_ranges(ranges, changes)
   local all_updates = {}
   for _,range in pairs(ranges) do
      local updates = apply_changes_to_range(range, changes)
      for _, update in pairs(updates) do
         table.insert(all_updates, update)
      end
   end
   return all_updates
end


local sr = 10
local ranges = {seed_ranges[sr]}
pprint(ranges)
for i,section in pairs(intervals) do
   print(i, '================================================================')
   --pprint(ranges)
   ranges = apply_changes_to_ranges(ranges, section)
   --print('\t updates to..............')
   pprint(ranges)
end

local min2 = 999999999999999
for _,range in pairs(ranges) do
   local l,_ = table.unpack(range)
   if l < min2 then
      min2 = l
   end
end
print("Smallest for", sr, ':', min2)
-- yeah we're manually running this, changing sr as we go
--  281453544
--  170944797
-- 1897502919
--  857586243
--   32956608
-- 1756603980
--   58556595
-- section 8 errors, hope the answer isn't here! :)
--  214995753
--   27992443

-- test_cases = {
--    { range={5,10}, change={0, 4, 1} }, -- no overlap
--    { range={5,10}, change={0, 5, 1} }, -- kinda overlap
--    { range={5,10}, change={0, 6, 1} }, -- left overlap
--    { range={5,10}, change={5, 6, 1} }, -- full left overlap
--    { range={5,10}, change={6, 8, 1} }, -- middle overlap
--    { range={5,10}, change={8, 10, 1} }, -- full right overlap
--    { range={5,10}, change={10, 12, 1} }, -- kinda overlap
--    { range={5,10}, change={11, 15,1}}, -- no overlap
-- }

-- for i,tc in pairs(test_cases) do
--    local update, remainders = apply_change(tc['range'], tc['change'])
--    print('TEST', i)
--    pprint(update)
--    pprint(remainders)
-- end
-- --
