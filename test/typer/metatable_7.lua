local mt = {__index = 42}
local tbl = {}

tbl = setmetatable(tbl, mt)

print(tbl[1])
