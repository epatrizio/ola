--[[
Array
  Table is the only data structure in Lua:
  Tables are flexible and powerful maps (~associative arrays).

  This is an experimental implementation of the classic array above Lua tables.
  (a more complete example of meta mechanism powerful)

  https://www.lua.org/pil/13.4.4.html
  https://www.tutorialspoint.com/lua/lua_proxy_tables_with_metatables.htm
--]]

local array = {}

function array.create(size)
  local array = {}
  array.__size = size
  local array_proxy = array
  array = {}
  local array_metatable = {
    -- TODO: add more logic for size control
    __index = function(arr, key) return array_proxy[key] end,
    __newindex = function(arr, key, value)
      -- TODO: refacto, naive implementation
      local floor
      if type(key) == "number" and key > 0 then
        floor = math.floor(key)
        if key == floor then
          array_proxy[key] = value
        else
          print("attempt to perform an incorrect key for an array", key)
        end
      else
        print("attempt to perform an incorrect key for an array", key)
      end
    end,
    __tostring = function(arr)
      local arr_str = ""
      local tmp_str = ""
      local arr_size = arr.__size
      for i = 1, arr_size do
        tmp_str = tostring(arr[i])
        arr_str = arr_str .. tmp_str .. ", "
      end
      return "{" .. arr_str .. "}"
    end
  }
  return setmetatable(array, array_metatable)
end

function array.size(arr)
  return arr.__size
end

-- basic usage

local arr = array.create(10)
print("size", array.size(arr))

arr[1] = 20
arr[3] = 42
arr["5"] = "incorrect"
arr["ko"] = "incorrect"
arr[true] = "incorrect"
arr[1.1] = "incorrect"
arr[1.0] = 21
arr[9] = "hello, world!"
arr[10] = false

print(arr) -- __tostrng: {21, nil, 42, nil, nil, nil, nil, nil, hello, world!, false, }

-- BUG FIX: https://github.com/epatrizio/ola/issues/34
print(arr[1])   -- 21
print(arr[1.0]) -- 21 (same field!)
