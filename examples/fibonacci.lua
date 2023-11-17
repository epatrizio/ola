--[[ 
Second classic example : The Fibonacci sequence
multiple implementations
--]]

-- 1: Naive recursive version
--    (same 'fibo' calculation performed several times)
function fibo1(n)
  if n <= 0 then
    return 0
  elseif n == 1 then
    return 1
  else
    return fibo1(n-1) + fibo1(n-2)
  end
end
print(fibo1(15))
--print(fibo1(30))    -- 832040 (~2sec)

-- 2: Recursive version with memoization
local mem = {}
mem[0] = 0
mem[1] = 1
function fibo2(n)
  if mem[n] ~= nil then
    return mem[n]
  else
    local fib = fibo2(n-1) + fibo2(n-2)
    mem[n] = fib
    return fib
  end
end
print(fibo2(15))
--print(fibo1(30))    -- 832040 (again ~2sec : Warning, bug! recursive function implementation problem)

-- 3: Polynomial algorithm
function fibo3(n)
  local a, b = 0, 1
  for i = 1, n do
    a, b = b, a+b
  end
  return a
end
print(fibo3(15))
print(fibo3(30))
print(fibo3(60))

-- 4: Tail recursive
--    (same 'fibo' calculation performed only one time!)
function fibo4(n, a, b)
  if n <= 0 then
    return a
  elseif n == 1 then
    return b
  else
    return fibo4(n-1, b, a+b)
  end
end
print(fibo4(15, 0, 1))
print(fibo4(30, 0, 1))
print(fibo4(60, 0, 1))
