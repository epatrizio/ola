-- recursive functions

-- global

function Fact(n)
  if n <= 1 then
    return 1
  else
    return n * Fact(n - 1)
  end
end
print(Fact(5)) -- 120

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

-- local

local function fact_l(n)
  if n <= 1 then
    return 1
  else
    return n * fact_l(n - 1)
  end
end
print(fact_l(5)) -- 120

local function fibo_l(n)
  if n <= 0 then
    return 0
  elseif n == 1 then
    return 1
  else
    return fibo_l(n - 1) + fibo_l(n - 2)
  end
end
print(fibo_l(15)) -- 610
