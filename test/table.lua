local table1 = {}
-- print(table1)        -- table id (ok)

-- table1 = {;}         -- syntax error
local table1 = { 1, 2, 3, } -- ok (end separator and ',' & ';' combined separators)
-- table1 = {1, 2; 3;,} -- syntax error (double end separator)

local function id(e)
  return e
end

table1 = { 1, "2", 1 + 2, nil, true, { 11, 22, { 111, 222, 333, 444 } }, id, }
print(table1[1])
print(table1[2])
print(table1[3])
print(table1[4])
print(table1[5])
-- print(table1[6])     -- table id (ok)
-- print(table1[7])     -- function id (ok)

print(table1[7](false)) -- false

print(table1[42])

table1[2] = 2.0
print(table1[2])

print(table1[6][2])
table1[6][2] = "22.22"
print(table1[6][2])

print(table1[6][3][3])
table1[6][3][3] = "333.333"
print(table1[6][3][3])

table1[6] = "nil_val"
print(table1[6])
print(table1[6][3])

table1["key1"] = "val1"
table1["key" .. "2"] = "val" .. "2"
print(table1["key1"])
print(table1["key2"])
print(table1.key2)
table1.key3 = "val3"
print(table1.key3)

function p(a)
  print("my_print:")
  print(a)
end

p(table1["key1"])
p(table1["key" .. "1"])

local tmp_tbl = {}
-- print(tmp_tbl)   -- table id (ok)
tmp_tbl = nil
print(tmp_tbl) -- nil

tmp_tbl = { 1, 2, 3 }
-- print(tmp_tbl)   -- table id (ok)
local tmp, tmp_tbl = 0
print(tmp)     -- 0
print(tmp_tbl) -- nil

local function t()
  local t = { 42, true, 42.5 }
  return t
end

-- print(t())       -- table id (ok)
print(t()[1]) -- 42

-- doc §3.4.9 https://www.lua.org/manual/5.4/manual.html#3.4.9

local function add42(i)
  return 42 + i
end
local x = 10

local tbl1 = { [add42(1)] = 0, "x", "y", x = 1, add42(x), [30] = 23, 45 }

local tbl2 = {}
tbl2[add42(1)] = 0
tbl2[1] = "x"      -- 1st exp
tbl2[2] = "y"      -- 2nd exp
tbl2.x = 1         -- t["x"] = 1
tbl2[3] = add42(x) -- 3rd exp
tbl2[30] = 23
tbl2[4] = 45       -- 4th exp

-- tbl1 and tbl2 must be equivalent
print(tbl1[43])
print(tbl1[43] == tbl2[43])
print(tbl1[1])
print(tbl1[1] == tbl2[1])
print(tbl1[2])
print(tbl1[2] == tbl2[2])
print(tbl1.x)
print(tbl1.x == tbl2.x)
print(tbl1["x"])
print(tbl1["x"] == tbl2["x"])
print(tbl1[3])
print(tbl1[3] == tbl1[3])
print(tbl1[30])
print(tbl1[30] == tbl1[30])
print(tbl1[4])
print(tbl1[4] == tbl1[4])

-- return function

local function f()
  return 1, 2, 3
end

local tbl3 = { 0, f() }
print(tbl3[1]) -- 0
print(tbl3[2]) -- 1
print(tbl3[3]) -- 2
print(tbl3[4]) -- 3
print(tbl3[5]) -- nil

local tbl4 = { f(), 0 }
print(tbl4[1]) -- 1
print(tbl4[2]) -- 0
print(tbl4[3]) -- nil

local function g()
  return
end

local tbl5 = { 0, g() }
print(tbl5[1]) -- 0
print(tbl5[2]) -- nil

local tbl6 = { g(), 0 }
print(tbl6[1]) -- nil
print(tbl6[2]) -- 0

-- syntactic sugar: nothing { 42 } = nothing({ 42 }) function call

local function nothing(tbl)
  print("nothing")
end

nothing { 42 }

-- remove

local tbl7 = { ["key_1"] = "val_1", ["key_2"] = "val_2" }
tbl7["key_1"] = nil

for k, _v in pairs(tbl7) do
  print(k)
end
