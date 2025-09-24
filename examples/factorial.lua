--[[
classic example : Factorial
multiple implementations
--]]

-- 1: Basic imperative version
function Fact1(n)
  local f = 1
  for i = 1, n do
    f = f * i
  end
  return f
end

print(Fact1(20)) -- 2 432 902 008 176 640 000

-- 2.1: Standard recursive version
function Fact2(n)
  if n <= 0 then
    return 1
  else
    return n * Fact2(n - 1)
  end
end

print(Fact2(20))

-- 2.2: Recursive version with lambda approach
Fact3 = function(n)
  if n <= 0 then
    return 1
  else
    return n * Fact3(n - 1)
  end
end
print(Fact3(20))
