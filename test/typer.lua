-- print(not 10)   -- ok todo
-- print(not 10.5) -- ok todo
-- print(not nil)  -- ok todo

print(10+false) -- ko
-- print(false+10) -- ko
-- print(10+nil)   -- ko
-- print(10+"str") -- ko

-- print(-nil)     -- ko
-- print(-false)   -- ko
-- print(-"str")   -- ko

-- print(42^nil)   -- ko
-- print(42^true)  -- ko
-- print(42^"str") -- ko

-- print(1 < true) -- ko todo
-- print(1 < nil)  -- ko todo

-- print(10 and 20)-- ok todo
-- print(20 and 10)-- ok todo
-- print(10 or 20) -- ok todo
-- print(20 or 10) -- ok todo

-- print(#nil)    -- ko
-- print(#42)     -- ko
-- print(#42.0)   -- ko
-- print(#true)   -- ko
-- print(#false)  -- ko

-- print(1 .. "str")       -- ok 1str
-- print(1.1 .. "str")     -- ok 1.1str
-- print(nil .. "str")  -- ko
-- print(true .. "str") -- ko

--while 0 -- ko pas false mais passe
  --  do
    --    print(4)
    --end
