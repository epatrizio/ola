print(nil, true, 1, 2.2, "string")

print(type(nil))
print(type(true))
print(type(4 < 2))
print(type(42))
print(type(41.0+1))
print(type({1,2,3}))

print(tostring(nil))
print(tostring(true))
print(tostring(42))
print(tostring(41.0+1))
-- print(tostring({1,2,3}))

-- table next
-- https://www.lua.org/manual/5.4/manual.html#6.1
-- TODO: Warning spec not fully implemented

local tbl = {}
print(next(tbl))
tbl = {1, 2, 3}
print(next(tbl))
print(next(tbl, 1))
print(next(tbl, 2))
print(next(tbl, 3))
-- print(next(tbl, 4))      -- ko, invalid key to 'next' (4 not exists)
-- print(next(tbl, "3"))    -- ko, invalid key to 'next' ("3" not exists)

local table = {}
table[3] = true
table[4] = nil
table[42] = "42"
table[10] = 10.10
table[true] = false
table["key"] = "val"
print(next(table))
print(next(table, 3))
print(next(table, 4))
print(next(table, 10))
print(next(table, 42))
print(next(table, true))
print(next(table, "key"))

assert(true)
assert(42)
assert(42.42, "message")
assert("42")
assert({}, "message")
-- assert(nil)              -- ok, stop
-- assert(false)            -- ok, stop
assert(4 < 2, "message")    -- ok, stop
