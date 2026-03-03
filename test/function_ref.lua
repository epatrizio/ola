local arr = { 0 }

local function f(a)
  a[1] = 42
  a = { 4242 }
  print(a[1])     -- 4242
end

print(arr[1])     -- 0
f(arr)            -- pass by ref
print(arr[1])     -- 42 - ref modified in f function
