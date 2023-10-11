a = 42
print(a)

a1, a2, a3 = 40, 41, 40+2
print(a1)
print(a2)
print(a3)

a1, a2, a3 = 40, 41
print(a1)
print(a2)
print(a3)

a1, a2 = 40, 41, 42
print(a1)
print(a2)
print(a3)

print(a4)       -- nil, not an error

-- blocks

do
    local a4, b = 0, 0
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

-- if

-- while

-- repeat

-- for
