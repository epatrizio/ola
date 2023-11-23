print(# "str")              -- 3 (string length)
print(# "str" .. "str")     -- 3str (check Operator precedence!)
print(# ("str" .. "str"))   -- 6
print(# "str" .. 123)       -- 3123 (check Operator precedence!)
--print(# 123 .. 456)       -- Typing error (#123 not correct)
--print(# 123.456 .. 78.9)  -- Typing error (#123.456 not correct)
print(# (123 .. 456))       -- 6
print(# (123.456 .. 78.9))  -- 11

tbl = {1, 2, "3", "4", {"a","b"}}
print(#tbl)        -- 5
print(#tbl[5])     -- 2
-- print(#tbl[1])  -- Typing error
tbl["key"] = "val"
tbl[5]["key"] = "val"
print(#tbl)        -- 6
print(#tbl[5])     -- 3
