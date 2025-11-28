--[[
Set data structure
  https://www.lua.org/pil/11.5.html
  implementation becomes trivial with this trick: (key = the SET VALUE, value = boolean TRUE)
--]]

local set = {}

function set.create()
  return {}
end

function set.add(val, s)
  s[val] = true
  return s                    -- non-mandatory: https://github.com/epatrizio/ola/issues/35
end

function set.remove(val, s)
  s[val] = nil
  return s                    -- non-mandatory: https://github.com/epatrizio/ola/issues/35
end

function set.has(val, s)
  for k, v in pairs(s) do
    if k == val and v then return true end
  end  
  return false
end

local s = set.create()
s = set.add("word_1", s)
s = set.add("word_2", s)
s = set.add("word_2", s)      -- same value
s = set.add("word_3", s)
s = set.remove("word_3", s)

print(set.has("ko", s))       -- false
print(set.has("word_1", s))   -- true
print(set.has("word_2", s))   -- true
print(set.has("word_3", s))   -- false

for k, _v in pairs(s) do
  print(k)
end
