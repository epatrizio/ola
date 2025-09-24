local mt = { __newindex = 42 }
local tbl = {}

tbl = setmetatable(tbl, mt)

tbl[1] = 1
