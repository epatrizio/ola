--[[
Linked list data structure
  https://www.lua.org/pil/11.3.html
  https://www.tutorialspoint.com/lua/lua_lists.htm
--]]

local node = {}

function node.create(value)
  return { value = value, next = {} }
end

local linked_list = {}

function linked_list.create()
  return nil
end

function linked_list.is_empy(ll)
  return ll == nil
end

function linked_list.add(value, ll)
  local node = node.create(value)
  node.next = ll
  return node
end

function linked_list.len(ll)
  if linked_list.is_empy(ll) then
    return 0
  else
    return 1 + linked_list.len(ll.next)
  end
end

function linked_list.print(ll)
  if linked_list.is_empy(ll) then
    return
  else
    print(ll.value)
    linked_list.print(ll.next)
  end
end

-- basic usage

local ll = linked_list.create()
ll = linked_list.add(21, ll)
ll = linked_list.add(42, ll)
ll = linked_list.add(84, ll)
print("linked_list size", linked_list.len(ll))
linked_list.print(ll)

-- classic application 1: stack (LIFO)

local stack = {}

function stack.create()
  return linked_list.create()
end

function stack.is_empy(st)
  return linked_list.is_empy(st)
end

function stack.len(st)
  return linked_list.len(st)
end

function stack.print(st)
  return linked_list.print(st)
end

function stack.push(value, st)
  return linked_list.add(value, st)
end

function stack.top(st)
  if stack.is_empy(st) then
    return nil
  else
    return st.value
  end
end

function stack.pop(st)
  if stack.is_empy(st) then
    return nil
  else
    return st.next
  end
end

local st = stack.create()
st = stack.push(21, st)
st = stack.push(42, st)
st = stack.push(84, st)
print("stack size", stack.len(st))
print("stack top", stack.top(st))
st = stack.pop(st)
print("stack size", stack.len(st))
print("stack top", stack.top(st))
stack.print(st)

-- classic application 2: queue (LILO)
-- only difference: pop implementation >> last element must be pop, not first!
-- (the entire structure must be scanned, another approach might be more effective)
--    See queue.lua (for an another implementation approach)
