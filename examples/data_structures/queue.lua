--[[
Queue data structure
  https://www.lua.org/manual/5.4/manual.html#6.6
  stdlib.table implementation becomes trivial!
--]]

local queue = {}

function queue.create()
  return {}
end

function queue.push(val, q)
  table.insert(q, 1, val)
end

function queue.pop(q)
  return table.remove(q)
  --table.remove(self, 1) -- if stack, remove first element
end

function queue.len(q)
  return #q
end

function queue.is_empty(q)
  return queue.len(q) == 0
end

local q = queue.create()
queue.push(10, q)
queue.push(20, q)
queue.push(30, q)
print(queue.pop(q))       -- 10
print(queue.pop(q))       -- 20

print(queue.len(q))       -- 1 (= +3 -2)
print(queue.is_empty(q))  -- false

for i, v in ipairs(q) do
  print(i, v) -- 1: 30 /  1: 10
end
