-- Classics

local i = "i_var"

function Counter()
  local i = 0
  return function()
    i = i + 1
    return i
  end
end

print(i) -- i_var

C1 = Counter() -- c1
print(C1())    --> 1
print(C1())    --> 2
C2 = Counter() -- c2 = v3
print(C2())    --> 1
print(C1())    --> 3
print(C2())    --> 2
print(C2())    --> 3

print(i) -- i_var

local function add(x)
  return function(n)
    return x + n
  end
end
local add2 = add(2)
local addMin2 = add(-2)
-- print(add2)       -- Type = function > print = "function: id"
-- print(addMin2)    -- Type = function > print = "function: id"
print(add2(0))       -- 2
print(add2(40))      -- 42
print(addMin2(0))    -- -2
print(addMin2(44))   -- 42

-- Side effect

Var = false

local function setVar(v)
  Var = v
end

print(Var) -- false
setVar("Value")
print(Var) -- Value
