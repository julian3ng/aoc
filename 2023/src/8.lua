local common = require("common")

local lines = common.read_lines("../input/8.txt")

directions = lines[1]
graph = {}

-- build graph
for i,line in pairs(lines) do
   if i >= 3 then
      local node, left, right = line:match("(%a+) = %((%a+), (%a+)%)")
      graph[node] = { L=left, R=right }
   end
end

function count_steps(start, goal_pred)
   local i = 0
   local cur = start
   local l = #directions
   while not goal_pred(cur) do
      local direction = directions:sub((i % l) + 1, (i % l) + 1)
      local next_cur = graph[cur][direction]
      --print(i..': '..cur..'=['..graph[cur].L..','..graph[cur].R..']-'..direction..'->'..next_cur)
      cur = next_cur
      i = i + 1
   end
   return i
end

print("Part 1:", count_steps('AAA', function (cur) return cur == 'ZZZ' end))


for start,_ in pairs(graph) do
   if start:match("A$") then
      print(start, count_steps(start, function (cur) return cur:match('Z$') end))
   end
end

-- output was:
-- 16579
-- 20513
-- 22199
-- 13207
-- 14893
-- 12083

-- Then I stuffed it into emacs calc's LCM (k l) command
print("Part 2:", 10241191004509)
