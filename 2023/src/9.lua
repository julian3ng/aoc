local common = loadfile('common.lua')()

local lines = table.map(common.read_lines('../input/9.txt'), function (l) return l:split(" ") end)

function diffs(t)
   if #t <= 1 then
      return nil
   end

   local tt = {
      diff = t[2]-t[1]
   }

   for i=1,#t-1 do
      tt[i] = t[i+1] - t[i]
      if tt[i] ~= tt.diff then
         tt.diff = nil
      end
   end
   return tt
end

function predict_next(t)
   local ds = diffs(t)
   --common.pprint(ds)
   if ds.diff == nil then
      local diffs_next = predict_next(ds)
      return t[#t] + diffs_next
   end
   
   return t[#t] + ds.diff
end

print("Part 1:", common.sum_values(table.map(lines, predict_next)))

-- predict_next(lines[1])
-- bleh = table.map({'0 3 6 9 12 15',
-- '1 3 6 10 15 21',
-- '10 13 16 21 30 45'}, function (s) return s:split(' ') end)

-- common.pprint(table.map(bleh, function (l)
--                            return predict_next(table.reverse(l))
-- end))
-- common.pprint(table.reverse(lines[1]))
-- -- print(predict_next(table.reverse(lines[1])))

print("Part 2:", common.sum_values(table.map(lines, function (l) return predict_next(table.reverse(l)) end)))
