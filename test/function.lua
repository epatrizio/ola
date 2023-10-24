function max(num1, num2)
  if (num1 > num2) then
    result = num1
  else
    result = num2
  end
  return result
end

max(10,4)

print(max(10,4))
print(max(5,6))

-- recursive call

function fact(n)
  if n <= 1 then
    return 1
  else
    return n * fact(n-1)
  end
end

print(fact(5))

-- function def as a value

print_fct = function (exp)
  print("print::")
  print(exp)
end

local function add_f(a, b, fun)   -- local function (scope current block)
  fun(a+b)
end

add_f(40, 2, print_fct)

-- variadic functions

--function add_f(..., a)  -- memo: must be incorrect
--function add_f(a, ...)  -- todo: bug
function add_f(a)
  f(a)
end

function add_f(...)
  f("variadic")
end
