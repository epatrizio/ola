--[[
classic example : The Fibonacci sequence
multiple implementations
--]]

-- 1: Naive recursive version
--    (same 'fibo' calculation performed several times)
function Fibo1(n)
  if n <= 0 then
    return 0
  elseif n == 1 then
    return 1
  else
    return Fibo1(n - 1) + Fibo1(n - 2)
  end
end

print(Fibo1(15))
--print(fibo1(30))    -- 832040 (~2sec)

-- 2: Recursive version with memoization
local mem = {}
mem[0] = 0
mem[1] = 1
function Fibo2(n)
  if mem[n] ~= nil then
    return mem[n]
  else
    local fib = Fibo2(n - 1) + Fibo2(n - 2)
    mem[n] = fib
    return fib
  end
end

print(Fibo2(15))
print(Fibo2(30)) -- 832040 (compared with the previous version 'fibo1', performance is OK!)

-- 3: Polynomial algorithm
function Fibo3(n)
  local a, b = 0, 1
  for i = 1, n do
    a, b = b, a + b
  end
  return a
end

print(Fibo3(15))
print(Fibo3(30))
print(Fibo3(60))

-- 4: Tail recursive
--    (same 'fibo' calculation performed only one time!)
function Fibo4(n, a, b)
  if n <= 0 then
    return a
  elseif n == 1 then
    return b
  else
    return Fibo4(n - 1, b, a + b)
  end
end

print(Fibo4(15, 0, 1))
print(Fibo4(30, 0, 1))
print(Fibo4(60, 0, 1))
