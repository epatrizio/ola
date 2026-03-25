local arr_1 = { 1 }

local function f(a)
  a[1] = 42
  a = { 4242 }
  print(a[1])     -- 4242
end

print(arr_1[1])     -- 1
f(arr_1)            -- pass by ref
print(arr_1[1])     -- 42 - ref modified in f function

local arr_2 = { 2 }

print(arr_2[1])     -- 2
f(arr_2)            -- pass by ref
print(arr_2[1])     -- 42 - ref modified in f function

local function g(a)
  local b = a
  b[1] = 42
end

-- BUG: https://github.com/epatrizio/ola/issues/47
-- local arr_3 = { 3 }

-- print(arr_3[1])     -- 3
-- g(arr_3)            -- pass by ref
-- print(arr_3[1])     -- 42 - ref modified in g function via local b var
