local common = loadfile('common.lua')()

local lines = common.read_lines('../input/10.txt')
-- local lines = {
--    '..F-7..',
--    '..|.|..',
--    'F-S.L-7',
--    '|.....|',
--    'L-7.F-J',
--    '..|.|..',
--    '..L-J..',
-- }

local map = setmetatable({}, {
      __index = function (self, i)
         if i < 1 or i > #self then
            return {}
         else
            return rawget(self, i)
         end
      end
})
local start = {}
for i=1,#lines do
   map[i] = setmetatable({}, {
         __index = function (self, j)
            if j < 1 or j > #self then
               return '.'
            else
               return rawget(self, j)
            end
         end
   })
   for j=1,#lines[1] do
      local c = lines[i]:sub(j,j)
      map[i][j] = c
      if c == 'S' then
         start = {i, j}
      end
   end
end

function connects_east(c)
   return c == '-' or c == '7' or c == 'J' or c == 'S'
end

function connects_west(c)
   return c == '-' or c == 'F' or c == 'L' or c == 'S'
end

function connects_north(c)
   return c == '|' or c == 'J' or c == 'L' or c == 'S'
end

function connects_south(c)
   return c == '|' or c == '7' or c == 'F' or c == 'S'
end

function connected(y1, x1, y2, x2)
   if (y1 == y2) then
      if (x2-x1 == 1) then
         return connects_east(map[y2][x2]) and connects_west(map[y1][x1])
      elseif (x2-x1 == -1) then
         return connects_east(map[y1][x1]) and connects_west(map[y2][x2])
      else
         return false
      end
   elseif (x1 == x2) then
      if (y2-y1 == 1) then
         return connects_north(map[y2][x2]) and connects_south(map[y1][x1])
      elseif (y2-y1 == -1) then
         return connects_north(map[y1][x1]) and connects_south(map[y2][x2])
      else
         return false
      end
   else
      return false
   end
end



local start_char = nil
local start_y, start_x = table.unpack(start)

local s_n = connected(start_y, start_x, start_y-1, start_x)
local s_e = connected(start_y, start_x, start_y, start_x+1)
local s_s = connected(start_y, start_x, start_y+1, start_x)
local s_w = connected(start_y, start_x, start_y, start_x-1)

if s_n then
   if s_s then
      start_char = '|'
   elseif s_e then
      start_char = 'L'
   elseif s_w then
      start_char = 'J'
   end
elseif s_s then
   if s_e then
      start_char = 'F'
   elseif s_w then
      start_char = '7'
   end
else
   start_char = '-'
end


function neighbors(y, x)
   local checks = {}
   for cy=-1,1 do
      for cx=-1,1 do
         if math.abs(cy) ~= math.abs(cx) then
            table.insert(checks, {cy, cx})
         end
      end
   end

   local i=0
   return function()
      i = i + 1
      if i > 4 then return nil end
      local cy, cx = table.unpack(checks[i])
      if connected(y, x, y + cy, x + cx) then
         return true, y+cy, x+cx, map[y+cy][x+cx]
      end
      return false, y+cy, x+cx, map[y+cy][x+cx]
   end

end


function flood_fill(start)
   local flood_map = {}
   local visited = {}
   for y=1,#map do
      table.insert(flood_map, {})
      table.insert(visited, {})
      for x=1,#map[1] do
         flood_map[y][x] = 0
         visited[y][x] = false
      end
   end

   local q = {start}
   while #q > 0 do
      local cur_y, cur_x = table.unpack(table.remove(q, 1))
      local cur_f = flood_map[cur_y][cur_x]
      --print(cur_y, cur_x, cur_f, map[cur_y][cur_x])
      visited[cur_y][cur_x] = true
      for ctd, n_y, n_x in neighbors(cur_y, cur_x) do
         if ctd and not visited[n_y][n_x] then
            flood_map[n_y][n_x] = cur_f + 1
            table.insert(q, {n_y, n_x})
         end
      end
   end

   return flood_map
end

local ff = flood_fill(start)
local max1 = 0
for i,row in ipairs(ff) do
   for j,cell in ipairs(row) do
      if cell > max1 then
         max1 = cell
      end

      if not (i == start_y and j == start_x) then
         if cell == 0 then
            map[i][j] = '.'
         end
      end
   end
end

print("Part 1:", max1)

local sum2 = 0
for i=1,#map do
   local inside = false
   local last_bend = nil
   for j=1,#map[1] do
      local c = map[i][j]
      if i == start_y and j == start_x then
         c = start_char
      end
      
      if c == '.' and inside then
         sum2 = sum2 + 1
         map[i][j] = '*'
      end

      if c == '|' or c == 'F' or c == 'L' then
         inside = not inside
         last_bend = c
      end

      if c == '7' then
         if last_bend == 'F' then
            inside = not inside
         end
         last_bend = nil
      end

      if c == 'J' then
         if last_bend == 'L' then
            inside = not inside
         end
         last_bend = nil
      end
   end
end

print(sum2)
-- 1159, 1153, too high

for i=1,#map do
   for j=1,#map[1] do
      io.write(map[i][j])
   end
   io.write('\n')
end

--.....|..
--.OF--JI.
--..|.....
--.OL--7I.
--.....|..
