local mt = { __pairs = 42 }
local tbl = {}

setmetatable(tbl, mt)

for k, v in pairs(tbl) do print(k, v) end
