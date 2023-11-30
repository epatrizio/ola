function f()
  print("f1")
end

function f()
  print("f2")
end

f()   -- f2

-- args init

function f(a, b)
  print(a)
  print(b)
end
function r()
  return 1, 2, 3
end

f(3)        -- a=3, b=nil
f(3, 4)     -- a=3, b=4
f(3, 4, 5)  -- a=3, b=4
f(r(), 10)  -- a=1, b=10
f(r())      -- a=1, b=2

function f(a, b, c)
  print(a)
  print(b)
  print(c)
end
function r()
  return 1, 2
end
f(r(), 10)  -- a=1, b=10, c=nil
f(10, r())  -- a=10, b=1, c=2

result = false

function max(num1, num2)
  if num1 > num2 then
    result = num1         -- result in global scope
  else
    result = num2
  end
  return result
end

print(result)             -- false
max(10,4)
print(max(10,4))
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

function fib(n)
  if n <= 0 then
    return 0
  elseif n == 1 then
    return 1
  else
    return fib(n-1) + fib(n-2)
  end
end
print(fib(15))          -- 610

-- function def as a value

print_fct = function (p)
  print("print::")
  print(p)
end
print(p)                -- nil
--print(print_fct)      -- Type = function > print = "function: id"

-- single string arg
-- doc §3.4.10 https://www.lua.org/manual/5.4/manual.html#3.4.10

print(print_fct("arg"))
-- print(print_fct"arg")  -- bug (syntax error)
print(print_fct('arg'))
-- print(print_fct'arg')  -- bug (syntax error)
print(print_fct[[arg]])

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
