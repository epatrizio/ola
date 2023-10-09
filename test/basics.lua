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
print(42.0 == 42)
print(42 == 42.0)
print(42.0 == 42.0)
-- division
print(4/2)
print(4.0/2)
print(4/2.0)
print(4.0/2.0)
print(2./4.44)
print(42.42/0) -- inf (infinity)
-- exponentiation
print(4^2)
print(4.0^2)
print(4^2.0)
print(4.^2.)
print(4.^2.123)
print(0^2)
print(4^0.0)
-- floor division
print(1//2)
print(1%2)
print(1.0//2)
print(1%2.0)
print(1.0//2.0)
print(1.0%2.0)
print(10.0123//2.234)
print(10.0123%2.234)
print(-10.0123//-2.234)
print(-10.0123%-2.234)
-- print(1//0)  -- todo error div by 0
print(1.//0)    -- inf

-- bitwise

print(1&2)
print(1.0&1.5+0.5)
print(1|2.0)
print((1.0+0)|1)
print(1~2)
print(1.0~2.0)
print(1<<2)
print(1.0<<2.0)
print(1>>2)
print(1.0>>2.0)
print(~1)
print(~(1.5+2.5))

-- string

print("")
print("my string 123")
print("²&é\"#'{}[]()-|èçàù%*!§/:;,~+-*/=£$¤")
print('')
print('my string 123')
print('²&é"#\'{}[]()-|èçàù%*!§/:;,~+-*/=£$¤')

print("" .. "my string")
print("my" .. "string")
print('' .. 'my string')
print('my' .. 'string')

print("my string" .. 123)
print("my string" .. 123.345)

print(123 .. 456)
print(123.456 .. 789.0)
print(123 .. 456.789)
print(123.456 .. 789)
