--[[ 
Only with "naked" Lua and the Stdlib.next function,
it's possible to implement the standard ipairs and pairs iterators.
Then, thanks to them, it becomes possible to implement some classic table processing functions
(table_map, table_filter, table_fold) in a functional style.
--]]

-- 1. ipairs custom implementation as a stateless iterator
-- https://www.lua.org/manual/5.4/manual.html#6.1
-- Iterate over integer keys.
-- Pre-condition: consecutive integer keys starting with 1

print("1. -----")

local function next_ielt(table, idx)
  if idx < #table then
    idx = idx + 1
    return idx, table[idx]
  end
end

local function ipairs_impl(table)
  return next_ielt, table, 0
end

local tbl = {}
tbl[1] = "elt1"         -- Nb. without an index 1, iteration returns nothing
tbl[2] = "elt2"
tbl[3] = "elt3"         -- ipairs: last element
tbl[5] = "elt5"
tbl[-42] = -42.0
tbl["key1"] = "val1"
tbl["key2"] = "val2"

for i, v in ipairs_impl(tbl) do
  print(i, v)
end

-- 2. pairs custom implementation as a stateless iterator
-- Iterate over the whole table.

print("2. -----")

local function pairs_impl(table)
  return next, table, nil
end

for k, v in pairs_impl(tbl) do
  print(k, v)
end

-- 3. table_map
-- Builds a new table by applying a "transform" function to each element of an initial table

print("3. -----")

local function table_map(fun_map, table)
  local table_res = {}
  for k, v in pairs_impl(table) do
    table_res[k] = fun_map(v)
  end
  return table_res
end

local tbl_names = {"john", "mike", "marlon"}
tbl_names["name1"] = "steven"
tbl_names["name2"] = "world;)"

local function hello(str)
  return "hello, " .. str .. "!"
end

for k, v in pairs_impl(table_map(hello, tbl_names)) do
  print(k, v)
end

-- 4. table_filter
-- Builds a new table by filtering each element with a condition function

print("4. -----")

local function table_filter(fun_filter, table)
  local table_res = {}
  for k, v in pairs_impl(table) do
    if fun_filter(v) then
      table_res[k] = v
    end
  end
  return table_res
end

local tbl_nums = {0, -1, 42, -0.5, 17, 99.99, -42}

-- filtering function must return a boolean
-- (= the filter condition; here, positive number)
local function is_positive(num)
  return num > 0
end

for k, v in pairs_impl(table_filter(is_positive, tbl_nums)) do
  print(k, v)
end

-- 5. table_fold
-- Iterate over the whole table to calculate an accumulator by a step function

print("5. -----")

local function table_fold_left(fun_fold, acc, table)
  local acc_res = acc
  for _k, v in pairs_impl(table) do
    acc_res = fun_fold(acc_res, v)
  end
  return acc_res
end

local function add(num1, num2)
  return num1 + num2
end

-- calculating the sum of all table elements
print(table_fold_left(add, 0, tbl_nums))

-- 6. table_iter
-- iterates over table elements with a "statement" function

print("6. -----")

local function table_iter(fun_iter, table)
  for _k, v in pairs_impl(table) do
    fun_iter(v)
  end
end

local function table_iteri(fun_iteri, table)
  for k, v in pairs_impl(table) do
    fun_iteri(k, v)
  end
end

table_iter(print, tbl_names)
table_iteri(print, tbl_names)

-- 7. table_find
-- find the first table element that satisfies the predicate function

print("7. -----")

local function table_find(fun_pred, table)
  for _k, v in pairs_impl(table) do
    if fun_pred(v) then return v end
  end
end

local function greater_than(num, val)
  return num > val
end

local function greater_than_42(num)
  return greater_than(num, 42)
end

print(table_find(greater_than_42, tbl_nums))
