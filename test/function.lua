local function f()
  print("f1")
end

local function f()
  print("f2")
end

f() -- f2

-- args init

function f(a, b)
  print(a)
  print(b)
end

local function r()
  return 1, 2, 3
end

f(3)       -- a=3, b=nil
f(3, 4)    -- a=3, b=4
f(3, 4, 5) -- a=3, b=4
f(r(), 10) -- a=1, b=10
f(r())     -- a=1, b=2

function f(a, b, c)
  print(a)
  print(b)
  print(c)
end

function r()
  return 1, 2
end

f(r(), 10) -- a=1, b=10, c=nil
f(10, r()) -- a=10, b=1, c=2

local result = false

function max(num1, num2)
  if num1 > num2 then
    result = num1 -- result in global scope
  else
    result = num2
  end
  return result
end

print(result) -- false
max(10, 4)
print(max(10, 4))
print(result) -- 10

-- recursive call

function Fact(n)
  if n <= 1 then
    return 1
  else
    return n * Fact(n - 1)
  end
end

print(n)       -- nil
print(Fact(5)) -- 120

function Fib(n)
  if n <= 0 then
    return 0
  elseif n == 1 then
    return 1
  else
    return Fib(n - 1) + Fib(n - 2)
  end
end

print(Fib(15)) -- 610

-- function def as a value

local print_fct = function(p)
  print("print::")
  print(p)
end
print(p) -- nil
--print(print_fct)      -- Type = function > print = "function: id"

-- single string arg
-- doc §3.4.10 https://www.lua.org/manual/5.4/manual.html#3.4.10

print_fct("arg")
print_fct "arg"
print_fct('arg')
print_fct 'arg'
print_fct [[arg]]

-- function return function

local function hello()
  return function()
    print("hello")
  end
end

local h = hello()
h()
hello()()
-- (hello())()    -- OK (Typing error: attempt to call a nil value)

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

local function void1()
end

local function void2()
  return
end

local function void3()
  return nil
end

print(void1())    -- ""
print(void2())    -- ""
print(void3())    -- "nil"

-- variadic functions

--function var1(..., a)  -- memo: must be incorrect (ok - direct in parser.mly)
local function var1(a1, a2, ...)
  print(a1, a2)
  local l1, l2, l3 = 42, ...
  print(l1, l2, l3)
  g1, g2, g3 = 24, ...
  print(g1, g2, g3)
end

var1()
var1(1)
var1(1, 2)
var1(1, 2, 3)
var1(1, 2, 3, 4)
var1(1, 2, 3, 4, 5)

local function var2(...)
  local l1, l2 = ...
  print(l1, l2)
  g1, g2 = ...
  print(g1, g2)
end

var2()
var2(1)
var2(1, 2)
var2(1, 2, 3)

local function var3(...)
  local args = { ... }
  -- print(args)    -- table
  for i, v in ipairs(args) do
    print(i, v)
  end
end

var3(11, 22, 33, 44, 55)

local function var4(a, ...)
  local args = { ... }
  print(a)
  for i, v in ipairs(args) do
    print(i, v)
  end
end

var4(11, 22, 33, 44, 55)

-- 3.4.10 - Function Calls - results adjusted to 1

local function f_res_adj()
  return 42, 41, 40
end

print((f_res_adj())) -- 42

-- 

local function f()
    return { 11, 22, 33 }
end

print(f()[1])

local function f()
    return { 11, 22, 33 }, 2, true, nil
end

print(f()[1])
