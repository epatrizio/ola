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
  --return      -- ok & only one return in a block
  --return 1, 2 -- only at the end of the block
  print(tmp + 1)
  -- return tmp, tmp+1;
end

--return

print(0)
