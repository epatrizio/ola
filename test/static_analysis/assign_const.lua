do
  local a <const>
end
local a = 42 -- ok (scope)

-- local c <const>, d <const> = 21, 42
local c, d <const> = 20, 41
print(c) -- isn't print (static analysis before interpretation)
c = 21   -- ok
d = 42   -- ko
