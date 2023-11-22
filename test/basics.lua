--[[
    Basic stuff
--]]

t = {1, 2, true, false}

function id(a)
  return a
end

-- nil

print(nil)

-- boolean

print(true)
print(false)

print(not true)
print(not false)
print(not nil)        -- true
print(not 42)         -- false
print(not 42.42)      -- false
print(not "str")      -- false
print(not t)          -- false
print(not id)         -- false
print(not t[1])       -- false
print(not t[4])       -- true
print(not id(false))  -- true
print(not id(42))     -- false

print(true and false)       -- false
print(true or false)        -- true
print(t[3] and id(false))   -- false
print(id(true) or t[4])     -- true

-- doc 3.4.5 - Logical Operators
-- https://www.lua.org/manual/5.4/manual.html#3.4.5
-- 10 or 20            --> 10
-- 10 or error()       --> 10
-- nil or "a"          --> "a"
-- nil and 10          --> nil
-- false and error()   --> false
-- false and nil       --> false
-- false or nil        --> nil
-- 10 and 20           --> 20

print(10 or 20)       -- 10
print(nil or "a")     -- a
print(nil and 10)     -- nil
print(false and nil)  -- false
print(false or nil)   -- nil
print(10 and 20)      -- 20

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

-- relational operators

print(42 < 42)
print(42 <= 42)
print(42 > 42)
print(42 >= 42)
print(42 == 42)
print(42 ~= 42)

print(nil == nil)
print(nil ~= nil)

print(true == true)
print(true == false)
print(true ~= true)
print(true ~= false)

print("str" < "str")
print("str" <= "str")
print("str" > "str")
print("str" >= "str")
print("str" == "str")
print("str" ~= "str")
print("str1" == "str2")
print("str1" ~= "str2")

print(1 == true)
print(1 ~= true)
print("str" == 42.42)
print("str" ~= 42.42)
print(nil == 42)
print(nil ~= 42)

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

-- string (number operators)

print(-"42")
print(-"42.")
print(-"-42")
print(-"-42.")

print("40" + "2")
print("40.5" + "1.5")
print(44 - "2")
print("44" - 2.0)
print(1%"2.0")
print("1.0"//2.0)

-- operator precedence

print(1+21*2)     -- 43
print((1+21)*2)   -- 44

print(true and true or false and false)     -- true
print(true and (true or false) and false)   -- false

print(not false and true or false and not true)     -- true
print(not false and (true or false) and not true)   -- false

print(1+4^2)    -- 17
print((1+4)^2)  -- 25

-- end basic stuff