function f()
  print("f1")
end

function f()
  print("f2")
end

f()   -- f2

result = false

function max(num1, num2)
  if num1 > num2 then
    result = num1
  else
    result = num2
  end
  return result
end

print(result)             -- false
max(10,4)
print(result)             -- 10

-- recursive call

function fact(n)
  if n <= 1 then
    return 1
  else
    return n * fact(n-1)
  end
end
print(n)                -- nil
print(fact(5))          -- 120

-- function def as a value

print_fct = function (p)
  print("print::")
  print(p)
end
print(p)                -- nil
print(print_fct)        -- Type = function > print = "function: id"

-- local function (current block scope)

local function add_f(a, b, fun)
  local add = a + b
  fun(add)
end

add_f(40, 2, print_fct)

do
  local function p(str)
    print(str)
  end
  p("local function")
end
-- p("local function")  -- p function undefined

-- variadic functions

--function var1(..., a)  -- memo: must be incorrect
--function var1(a, ...)  -- todo: bug (syntax error)
function var1(a)
  print_fct(a)
end
var1(42)

function var2(...)
  f()
end
