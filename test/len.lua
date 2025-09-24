print(# "str")            -- 3 (string length)
print(# "str" .. "str")   -- 3str (check Operator precedence!)
print(# ("str" .. "str")) -- 6
print(# "str" .. 123)     -- 3123 (check Operator precedence!)
--print(# 123 .. 456)       -- Typing error (#123 not correct)
--print(# 123.456 .. 78.9)  -- Typing error (#123.456 not correct)
print(# (123 .. 456))      -- 6
print(# (123.456 .. 78.9)) -- 11

-- table length (= border)

local tbl = { 1, 2, "3", "4", { "a", "b" } }
print(#tbl)    -- 5
print(#tbl[5]) -- 2
-- print(#tbl[1])  -- Typing error
tbl["key"] = "val"
tbl[5]["key"] = "val"
print(#tbl)    -- 5
print(#tbl[5]) -- 2

local table = {}
print(#table) -- 0
table["key1"] = "val1"
table["key2"] = "val2"
print(#table) -- 0
table[0] = "val 0"
table[-1] = "val -1"
table[-42] = false
print(#table) -- 0 (only non-negative integer)
table[42] = 42.0
print(#table) -- 0 (zero, when index 1 is absent)
table[1] = 1
table[2] = 2
table[3] = 3
table[4] = 4
print(#table) -- 4
table[10] = 10
table[5] = 5  -- +1
table[-5] = -5
print(#table) -- 5
