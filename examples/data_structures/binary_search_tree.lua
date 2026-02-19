--[[
Binary search tree data structure
(classic example of recursive function use - graph traversal)
  https://en.wikipedia.org/wiki/Binary_search_tree
--]]

local function default_compare_func(a, b)
  assert(type(a) == type(b))
  if a < b then
    return -1
  elseif a == b then
    return 0
  else
    return 1
  end
end

local node = {}

function node.create_leaf(value, compare_func)
  if compare_func == nil then
    compare_func = default_compare_func
  end
  return { value = value, left = nil, right = nil, __compare = compare_func }
end

function node.create(value, left, right, compare_func)
  assert(compare_func ~= nil)
  return { value = value, left = left, right = right, __compare = compare_func }
end

-- BST = Binary Search Tree
local bst = {}

function bst.create()
  return nil
end

function bst.is_empty(bt)
  return bt == nil
end

function bst.nb_node(bt)
  if bst.is_empty(bt) then
    return 0
  else
    return 1 + bst.nb_node(bt.left) + bst.nb_node(bt.right)
  end
end

function bst.nb_leaf(bt)
  if bst.is_empty(bt) then
    return 0
  elseif bst.is_empty(bt.left) and bst.is_empty(bt.right) then
    return 1
  else
    return bst.nb_leaf(bt.left) + bst.nb_leaf(bt.right)
  end
end

function bst.max(bt)
  if bst.is_empty(bt) then
    return
  elseif not bst.is_empty(bt.right) then
    return bst.max(bt.right)
  else
    return bt.value
  end
end

function bst.min(bt)
  if bst.is_empty(bt) then
    return
  elseif not bst.is_empty(bt.left) then
    return bst.min(bt.left)
  else
    return bt.value
  end
end

function bst.add(value, bt, compare_func)
  if bst.is_empty(bt) then
    return node.create_leaf(value, compare_func)
  elseif bt.__compare(value, bt.value) < 0 then
    return node.create(bt.value, bst.add(value, bt.left), bt.right, bt.__compare)
  else
    return node.create(bt.value, bt.left, bst.add(value, bt.right), bt.__compare)
  end
end

function bst.delete(value, bt)
  if bst.is_empty(bt) then
    return bt
  elseif bt.__compare(value, bt.value) == 0 and bst.is_empty(bt.left) and bst.is_empty(bt.right) then
    return nil
  elseif bt.__compare(value, bt.value) == 0 and bst.is_empty(bt.left) then
    return bt.right
  elseif bt.__compare(value, bt.value) == 0 and bst.is_empty(bt.right) then
    return bt.left
  elseif bt.__compare(value, bt.value) == 0 then
    local left_max = bst.max(bt.left)
    return node.create(left_max, bst.delete(left_max, bt.left), bt.right, bt.__compare)
  elseif bt.__compare(value, bt.value) < 0 then
    return node.create(bt.value, bst.delete(value, bt.left), bt.right, bt.__compare)
  else
    return node.create(bt.value, bt.left, bst.delete(value, bt.right), bt.__compare)
  end
end

function bst.search(value, bt)
  if bst.is_empty(bt) then
    return false
  elseif bt.__compare(value, bt.value) == 0 then
    return true
  elseif bt.__compare(value, bt.value) < 0 then
    return bst.search(value, bt.left)
  else
    return bst.search(value, bt.right)
  end
end

function bst.print_prefix(bt)
  if bst.is_empty(bt) then
    return
  else
    print(bt.value)
    bst.print_prefix(bt.left)
    bst.print_prefix(bt.right)
  end
end

function bst.print_infix(bt)
  if bst.is_empty(bt) then
    return
  else
    bst.print_infix(bt.left)
    print(bt.value)
    bst.print_infix(bt.right)
  end
end

function bst.print_postfix(bt)
  if bst.is_empty(bt) then
    return
  else
    bst.print_postfix(bt.left)
    bst.print_postfix(bt.right)
    print(bt.value)
  end
end

-- basic usage

print("**** bst of integer ****")
-- bst of integer
local bt = bst.create()
bt = bst.add(11, bt)
bt = bst.add(33, bt)
bt = bst.add(55, bt)
bt = bst.add(22, bt)
bt = bst.add(10, bt)
print("nb nodes", bst.nb_node(bt))
print("nb leafs", bst.nb_leaf(bt))
print("min", bst.min(bt))
print("max", bst.max(bt))
print("-- print_prefix")
bst.print_prefix(bt)
print("-- print_infix")
bst.print_infix(bt)
print("-- print_postfix")
bst.print_postfix(bt)
print("search 42", bst.search(42, bt))
print("search 22", bst.search(22, bt))
print("search 55", bst.search(55, bt))
print("-- delete 55 33 42 10")
bt = bst.delete(55, bt)
bt = bst.delete(33, bt)
bt = bst.delete(42, bt)
bt = bst.delete(10, bt)
print("nb nodes", bst.nb_node(bt))
print("nb leafs", bst.nb_leaf(bt))
print("min", bst.min(bt))
print("max", bst.max(bt))
print("search 55", bst.search(55, bt))

print("**** bst of string 1 ****")
-- bst of string
local bt_s1 = bst.create()
bt_s1 = bst.add("str_1", bt_s1)
bt_s1 = bst.add("str_2", bt_s1)
bt_s1 = bst.add("aa", bt_s1)
bt_s1 = bst.add("aaa", bt_s1)
bt_s1 = bst.add("zzz", bt_s1)
print("min", bst.min(bt_s1))
print("max", bst.max(bt_s1))
print("-- print_infix")
bst.print_infix(bt_s1)

-- advanced usage with specific compare function

print("**** bst of string 2 ****")

local function strlen_compare_func(a, b)
  assert(type(a) == type(b))
  assert(type(a) == "string")
  if #a < #b then
    return -1
  elseif #a == #b then
    return default_compare_func(a, b)
  else
    return 1
  end
end

-- bst of string with strlen_compare_func
local bt_s2 = bst.create()
bt_s2 = bst.add("str_1", bt_s2, strlen_compare_func)
bt_s2 = bst.add("str_2", bt_s2)
bt_s2 = bst.add("aa", bt_s2)
bt_s2 = bst.add("aaa", bt_s2)
bt_s2 = bst.add("zzz", bt_s2)
bt_s2 = bst.add("zzzzzzz", bt_s2)
bt_s2 = bst.delete("zzzzzzz", bt_s2)
bt_s2 = bst.delete("azerty", bt_s2)
print("min", bst.min(bt_s2))
print("max", bst.max(bt_s2))
print("-- print_infix")
bst.print_infix(bt_s2)
