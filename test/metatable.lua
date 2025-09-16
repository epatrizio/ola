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

local mt3 = {
  __tostring = function(arr)
      local arr_str = ""
      local k_str = ""
      local v_str = ""
      for k, v in pairs(arr) do
        k_str = tostring(k)
        v_str = tostring(v)
        arr_str = arr_str .. k_str .. ":" .. v_str .. ", "
      end
      return "{" .. arr_str .. "}"
    end
  }

tbl = {11, 22, 33, ["key"] = "val"}

tbl = setmetatable(tbl, mt3)
print(tbl)
