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

return common
