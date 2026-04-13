local tbl = {}

setmetatable(tbl, {})
-- print(getmetatable(tbl))  -- table id

setmetatable(tbl, nil)
print(getmetatable(tbl))
