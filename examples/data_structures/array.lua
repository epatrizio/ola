--[[
Array
  Table is the only data structure in Lua:
  Tables are flexible and powerful maps (~associative arrays).

  This is an experimental implementation of the classic array above Lua tables.

  https://www.lua.org/pil/13.4.4.html
  https://www.tutorialspoint.com/lua/lua_proxy_tables_with_metatables.htm
--]]

array = {}

function array.create(size)
  local array = {}
  array.__size = size
  local array_proxy = array
  array = {}
  local array_metatable = {
    -- TODO: add more logic for size control
    __index = function(arr, key) return array_proxy[key] end,
    __newindex = function(arr, key, value)
        if type(key) == "number" and key > 0 and key == math.floor(key) then
          array_proxy[key] = value
        else
          print("attempt to perform an incorrect key for an array", key)
        end
      end,
    __tostring = function(arr)
        local arr_str = ""
        for i = 1, arr.__size do
          arr_str = arr_str .. tostring(arr[i]) .. ", "
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

arr = array.create(10)
print("size", array.size(arr))

arr[1] = 20
arr[3] = 42
arr["5"] = "incorrect"
arr["ko"] = "incorrect"
arr[true] = "incorrect"
arr[1.1] = "incorrect"
arr[1.0] = 21 -- 1.0: correct (1)
arr[9] = "hello, world!"
arr[10] = false

print(arr)
