local common = {}
function common.read_lines(filename)
   local lines = {}
   for line in io.lines(filename) do
      table.insert(lines, line)
   end
   return lines
end

function string.gfind(str, pattern)
   local s,e = 0,0
   return function()
      local m_start, m_end = str:find(pattern, e+1)
      if m_start and m_end then
         s, e = m_start, m_end
         return s, e
      end
      return nil
   end
end

function string.split(str, c)
   local t = {}
   for s in string.gmatch(str, "([^"..c.."]+)") do
      table.insert(t, s)
   end
   return t
end

function table.map(t, f)
   local tt = {}
   for _,x in ipairs(t) do
      table.insert(tt, f(x))
   end
   return tt
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

function common.pprint(t)
   print_table_compact(t)
   io.write('\n')
end

return common
