print(getmetatable(42))         -- nil
print(getmetatable(false))      -- nil
print(getmetatable({}))         -- nil
print(getmetatable(1,2))        -- nil

local mt1 = {}
mt1.__metatable = "protected metatable!"

local tbl = {}

tbl = setmetatable(tbl, mt1)
print(getmetatable(tbl))        -- protected metatable!

tbl = setmetatable(tbl, nil)
print(getmetatable(tbl))        -- nil

local mt2 = {["key_mt2"] = "value_mt2"}

tbl = setmetatable(tbl, mt2)
mt = getmetatable(tbl)
print(mt["key_mt2"])            -- value_mt2
