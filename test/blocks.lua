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
until 1 < 2

-- init, max/min value, incr (1:default)
for i = 5,10,1
    do
        print(i)
        break
    end
--for i=1,f(x) do print(i) end

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

print(0)
