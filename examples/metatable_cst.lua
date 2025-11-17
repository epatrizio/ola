-- classic meta-mechanism application: table of constants

-- 1. table created once at the beginning and it is impossible to add new constants after
--    from: https://www.lua.org/pil/13.4.5.html

print("1. -----")

function To_constants(tbl)
  local proxy = {}
  local mt = { -- create metatable
    __index = tbl,
    __newindex = function(t, key, value)
      print("attempt to change constants", key, value)
    end
  }
  proxy = setmetatable(proxy, mt)
  return proxy
end

CST = To_constants { 11, 22, my_cst = "my_cst_value" } -- constants init
-- syntactic sugar: To_constants { ... } = To_constants({ ... }) (function call)
-- CST = To_constants(CST)

print(CST[1])                -- 11
print(CST[2])                -- 22
print(CST.my_cst)            -- my_cst_value

CST[1] = 111                 -- KO: attempt to change constants
CST[2] = 222                 -- KO: attempt to change constants
CST.my_cst = "new_value"     -- KO: attempt to change constants

CST[3] = 33                  -- KO: same with new value
CST.my_new_cst = "new_value" -- KO: same with new value

print(CST[3])                -- nil
print(CST.my_new_cst)        -- nil

-- 2. Constants can be created as needed
--    (experimental implementation. Idea: use a function to add constants that updates the metatable each time)

print("2. -----")

function Add_const(key_cst, val_cst, tbl)
  local tbl_mt = getmetatable(tbl)
  if tbl_mt ~= nil then
    tbl = tbl_mt.__index
  end
  tbl[key_cst] = val_cst
  return setmetatable({}, {
    __index = tbl,
    __newindex = function(t, key, value)
      print("attempt to change constant directly", key, value)
    end
  })
end

CST = {}

CST = Add_const(1, 11, CST)
CST = Add_const(2, 22, CST)
CST = Add_const("my_cst", "my_cst_value", CST)

print(CST[1])                -- 11
print(CST[2])                -- 22
print(CST.my_cst)            -- my_cst_value

CST[1] = 111                 -- KO: attempt to change constant directly
CST[2] = 222                 -- KO: attempt to change constant directly
CST.my_cst = "new_value"     -- KO: attempt to change constant directly

CST[3] = 33                  -- KO: Warning, for new value, add_const fonction
CST.my_new_cst = "new_value" -- KO: Warning, for new value, add_const fonction

CST = Add_const(3, 33, CST)
CST = Add_const("my_new_cst", "new_value", CST)

print(CST[3])         -- 33
print(CST.my_new_cst) -- new_value
