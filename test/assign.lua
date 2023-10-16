a = 0
print(a)

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

-- a4 = "a4"
print(a4)       -- nil, not an error

-- blocks

do
    local a4, b = 0, 0
    local a4 = 11
    --a4 = 22     -- local scope
    c = 1       -- global var
    local d <const> = 2
    local e <close> = false     -- nil or false or __close metamethod
    print(a4)   -- 0, local block scope, not the same var!
    print(b)
    print(c)
    print(d)
    -- d = 3       -- error : d is a constant local var
    print(e)
end

print(a4)       -- always nil
print(b)
print(c)

b = 2
print(b+c)

-- if

if false then print(a1)
elseif false then print(a2)
elseif true then
    a5 = true
    print(a5)
else print(a3) end

-- while

while 1 > 2
    do
        print(a6)
    end

-- repeat

repeat
    print(a7)
until 1 < 2

-- for

for i = 0,2
    do
        a6 = true
        print(a6)
    end
