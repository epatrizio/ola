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

assert(true)
assert(42)
assert(42.42, "message")
assert("42")
assert({}, "message")
-- assert(nil)              -- ok, stop
-- assert(false)            -- ok, stop
assert(4 < 2, "message")    -- ok, stop
