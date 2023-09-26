--[[
    Basic stuff
--]]

-- nil

print(nil)

-- boolean

print(true)
print(false)
print(not true)
print(not false)
print(true and false)
print(true or false)

-- integer

print(42)
print(40+2)     -- binary operators
print(44-2)
print(21*2)
print(-42)      -- unary operator

-- priority
print(-40-2)    -- -42
print(1+2-4)    -- -1
print(1-2+3)    -- 2    KO (-4)
print(-(40-2))  -- -38
print(1+(2-4))  -- -1
print(1-(2+3))  -- -4
print(((10+10)-(-10*3))*2)  -- 100

-- comparaison

print(42 < 42)
print(42 <= 42)
print(42 > 42)
print(42 >= 42)
print(42 == 42)
print(42 ~= 42)

-- float

print(42.)
print(42.0)
print(0.42)
print(.42)
print(42.1e1)
print(42.1e+1)
print(42.1e-1)
print(42.1E1)
print(42.1E+1)
print(42.1E-1)
print(42e1)

print(-42.)
print(-.42)
print(-42.1e1)

-- integer float operations

print(1+2.5)
print(2.5+1)
print(2.5+1.5)
print(-40.0-2)
print(-42.1e1+1)
