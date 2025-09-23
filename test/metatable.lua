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

local mt2 = {key_mt2 = "value_mt2"}

tbl = setmetatable(tbl, mt2)
-- setmetatable(tbl, mt2)       -- BUG: same as tbl = setmetatable(tbl, mt2) (side effect on tbl, but env isn't updated)
mt = getmetatable(tbl)
print(mt["key_mt2"])            -- value_mt2 (memo: ~ mt.key_mt2)

local mt3 = {
  __index = {key_new = "val_new",0,0,0,44};
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

tbl = {11, 22, 33, key = "val"}

tbl = setmetatable(tbl, mt3)
print(tbl)

-- __index tests

print(tbl.key)        -- exists: val
print(tbl.key_new)    -- not exists, check in __index table: val_new
print(tbl.key_new_2)  -- not exists, check in __index table: KO > nil
print(tbl[3])         -- exists: 33
print(tbl[4])         -- not exists: idem, 44
print(tbl[5])         -- not exists: idem, nil

local mt4 = {
  __index = function(arr, key)
      return "__index function result"
    end
}

tbl = setmetatable(tbl, mt4)

print(tbl.key)        -- exists: val
print(tbl.key_new)    -- not exists: fun call
print(tbl.key_new_2)  -- not exists: fun call
print(tbl[3])         -- exists: 33
print(tbl[4])         -- not exists: fun call
print(tbl[5])         -- not exists: fun call

local mt5 = {
  __newindex = function(arr, key, value)
      print("__newindex function - skip assignement", key, value)
    end
}

tbl = setmetatable(tbl, mt5)

tbl[2] = 222              -- ok, already exists
tbl.key_new = "val_new"   -- new index: fun call
tbl[4] = 44               -- new index: fun call

print(tbl[2])             -- 222
print(tbl.key_new)
print(tbl[4])
