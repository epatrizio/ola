a = b
local a = 0
a = 0
a = 1
print(a)
print(b)    -- nil, not an error

a1, a2, a3 = 40, 41, 40+2
print(a1)
print(a2)
print(a3)

a1, a2, a3 = 44-1, 88/2
print(a1)
print(a2)
print(a3)

a1, a2 = 45.5, #"assign"+40, false
print(a1)
print(a2)
print(a3)

print(a4)   -- nil, not an error
a4 = "a4"
print(a4)

-- blocks

do
    local a4, b = 0, 0
    local a4, b = 11
    a4 = 22     -- local scope
    --c = 1       -- global var
    local d <const> = 2
    local e <close> = false     -- nil or false or __close metamethod
    print(a)
    print(a4)   -- 22, local block scope, not the same var!
    print(b)
    print(c)
    print(d)
    -- d = 3       -- error : d is a constant local var
    print(e)
end

print(a4)       -- "a4"
print(b)
print(c)

-- b = 2
-- print(b+c)

-- if

if false then print(a1)
elseif false then print(a2)
elseif true then
    a5 = true
    print(a5)
else print(a3) end

-- while

cnt = 0
while cnt < 5
  do
    print(cnt)
    cnt = cnt + 1
  end

-- repeat

local cnt = 0
repeat
    print(cnt)
    cnt = cnt + 1
until cnt >= 5

-- for

for i = 0,2
    do
        a6 = true
        print(a6)
    end

-- doc §3.5 https://www.lua.org/manual/5.4/manual.html#3.5
x = 10                -- global variable
do                    -- new block
  local x = x         -- new 'x', with value 10
  print(x)            --> 10
  x = x+1
  do                  -- another block
    local x = x+1     -- another 'x'
    print(x)          --> 12
  end
  print(x)            --> 11
end
print(x)              --> 10  (the global one)
