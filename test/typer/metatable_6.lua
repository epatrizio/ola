local mt = {
  __tostring = "__tostring incorrect!"
}

local tbl = {}

print(setmetatable(tbl, mt))
