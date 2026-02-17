-- recursive functions

-- BUG: Recursive function in local context #23
-- https://github.com/epatrizio/ola/issues/23

-- local function Fact(n)
function Fact(n)
  if n <= 1 then
    return 1
  else
    return n * Fact(n - 1)
  end
end

print(Fact(5)) -- 120

-- local function Fib(n)
function Fibo(n)
  if n <= 0 then
    return 0
  elseif n == 1 then
    return 1
  else
    return Fibo(n - 1) + Fibo(n - 2)
  end
end

print(Fibo(15)) -- 610
