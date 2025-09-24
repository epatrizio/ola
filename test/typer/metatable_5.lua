local mt = {}
mt.__metatable = "protected metatable!"

local tbl = {}

tbl = setmetatable(tbl, mt)
print(getmetatable(tbl)) -- protected metatable!

setmetatable(tbl, {})    -- raise an error: previous tbl metatable is protected
