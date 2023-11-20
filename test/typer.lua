print(not 10)   -- ok
print(not 10.5) -- ok
print(not nil)  -- ok

-- print(10+false) -- ko
-- print(false+10) -- ko
-- print(10+nil)   -- ko
-- print(10+"str") -- ko
-- print(1+"str")  -- ko

-- print(-nil)     -- ko
-- print(-false)   -- ko
-- print(-"str")   -- ko

-- print(nil//1)   -- ko
-- print(true//1)  -- ko
-- print("str"//1) -- ko
-- print(nil%1)    -- ko
-- print(nil%"2.") -- ko
-- print(true%1)   -- ko
-- print("str"%1)  -- ko

-- print(42^nil)   -- ko
-- print(42^true)  -- ko
-- print(42^"str") -- ko

-- print(1&nil)    -- ko
-- print(true|2)   -- ko
-- print(1~"str")  -- ko
-- print(nil<<2)   -- ko
-- print(1>>false) -- ko
-- print(~nil)     -- ko
-- print(~true)    -- ko
-- print(~"str")   -- ko
-- print(1&1.5)    -- ok > interpretation error (1.5 not integer - 1&1.0 ok)

-- print(1 < true)      -- ko
-- print(1 < nil)       -- ko
-- print(nil < nil)     -- ko
-- print(true > false)  -- ko
-- print("str" >= 42.42)-- ko

print(10 and 20)    -- ok
print(20 and 10)    -- ok
print(10 or 20)     -- ok
print(20 or 10)     -- ok

-- print(#nil)    -- ko
-- print(#42)     -- ko
-- print(#42.0)   -- ko
-- print(#true)   -- ko
-- print(#false)  -- ko

print(1 .. "str")       -- ok 1str
print(1.1 .. "str")     -- ok 1.1str
-- print(nil .. "str")  -- ko
-- print(true .. "str") -- ko

if false then print(0)
elseif false then print(0)
elseif true then
  a = 2
  b = 40
  print(a+b)
  b = false
  print(a+b)
else print(0) end

-- for i = nil,10,1 do      -- ko (todo)
--   print(i)
-- end
