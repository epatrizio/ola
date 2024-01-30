-- blocks

print(0)

do
  ;
  print(1);
  print(2)
  print(3);
end

while 1 > 2
  do
    print(4)
  end

repeat
  print(4)
  break
  print(4.5)
until 1 < 2

-- init, max/min value, incr (1:default)
for i = 4+1.0,10    -- i in local scope
  do
    print(i)
    if i > 6 then break end
  end
print(i)  -- i new in global scope

for i = 41+1.0,10,"-1"..".5"
  do
    print(i)
  end
print(i)

for i = 0, 10, 2 do
  print(i)            -- scd iteration i=2 NOT i=3 (1+2)
  i = i + 1           -- this local control variable assignment don't modify the iteration
  print(i)            -- fst iteration i=1
end

local cnt = 0
while cnt < 2
  do
    for i = "20.5","10"..".5","-2"..".1"
      do
        print(i)
        if i < 15 then break end
      end
    cnt = cnt + 1
  end

-- for i=1,f(x) do print(i) end

-- for iterator (todo)
-- a = {"one", "two", "three"}
-- for i, v in ipairs(a) do
--   print(i)
--   print(v)
-- end

if true then print(42) end
if false then print(24) else print(42) end

if false then print(241)
elseif false then print(242) elseif true then print(42)
else print(243) end

-- warning, todo: full return semantic implem'
do
  local tmp = 1
  print(tmp)
  --return      -- ok & only one return in a block (TODO bug: equivalent to a return at the end of the block)
  --return 1, 2 -- only at the end of the block
  print(tmp + 1)
  --return tmp, tmp+1;
end

if true then return end   -- useful for stoping a script during the flow

-- doc §3.4.4 https://www.lua.org/manual/5.4/manual.html#3.3.4
-- return           -- ok with lua interpreter. Syntax error with ola
-- do return end    -- transform in "do return end" as mentioned in doc

print(0)

--break                    -- Bug: must generate a syntax error (break outside loop - for / while / repeat)
--do break end             -- same
--if true then break end   -- same
