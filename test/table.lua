table1 = {}
-- print(table1)        -- table id (ok)

function id(e)
  return e
end

table1 = {1, "2", 1+2, nil, true, {11;22;{111,222,333,444}}, id}
print(table1[1])
print(table1[2])
print(table1[3])
print(table1[4])
print(table1[5])
-- print(table1[6])     -- table id (ok)
-- print(table1[7])     -- function id (ok)

print(table1[7](false)) -- false

print(table1[42])

table1[2] = 2.0
print(table1[2])

print(table1[6][2])
table1[6][2] = "22.22"
print(table1[6][2])

print(table1[6][3][3])
table1[6][3][3] = "333.333"
print(table1[6][3][3])

table1[6] = "nil_val"
print(table1[6])
print(table1[6][3])

table1["key1"] = "val1"
table1["key".."2"] = "val".."2"
print(table1["key1"])
print(table1["key2"])

function p(a)
  print("my_print:")
  print(a)
end
p(table1["key1"])
p(table1["key".."1"])

tmp_tbl = {}
-- print(tmp_tbl)   -- table id (ok)
tmp_tbl = nil
print(tmp_tbl)      -- nil

tmp_tbl = {1, 2, 3}
-- print(tmp_tbl)   -- table id (ok)
tmp, tmp_tbl = 0
print(tmp)          -- 0
print(tmp_tbl)      -- nil

function t()
  local t = {42, true, 42.5}
  return t
end
-- print(t())       -- table id (ok)
print(t()[1])       -- 42

-- doc §3.4.9 https://www.lua.org/manual/5.4/manual.html#3.4.9

-- function idx(i)
--   return 42 + i
-- end
-- local x = 10

-- a = { [idx(1)] = g; "x", "y"; x = 1, idx(x), [30] = 23; 45 }
-- --is equivalent to
-- do
--   local t = {}
--   t[idx(1)] = g
--   t[1] = "x"         -- 1st exp
--   t[2] = "y"         -- 2nd exp
--   t.x = 1            -- t["x"] = 1
--   t[3] = idx(x)        -- 3rd exp
--   t[30] = 23
--   t[4] = 45          -- 4th exp
--   a = t
-- end
