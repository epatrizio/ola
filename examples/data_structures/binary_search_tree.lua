--[[
Binary search tree data structure
(classic example of recursive function use - graph traversal)
  https://en.wikipedia.org/wiki/Binary_search_tree
--]]

local node = {}

function node.create(value)
  return { value = value, left = nil, right = nil }
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

function bst.add(value, bt)
  if bst.is_empty(bt) then
    return node.create(value)
  elseif value < bt.value then
    return { value = bt.value, left = bst.add(value, bt.left), right = bt.right }
  else
    return { value = bt.value, left = bt.left, right = bst.add(value, bt.right) }
  end
end

function bst.delete(value, bt)
  if bst.is_empty(bt) then
    return bt
  elseif value == bt.value and bst.is_empty(bt.left) and bst.is_empty(bt.right) then
    return nil
  elseif value == bt.value and bst.is_empty(bt.left) then
    return bt.right
  elseif value == bt.value and bst.is_empty(bt.right) then
    return bt.left
  elseif value == bt.value then
    local left_max = bst.max(bt.left)
    return { value = left_max, left = bst.delete(left_max, bt.left), right = bt.right }
  elseif value < bt.value then
    return { value = bt.value, left = bst.delete(value, bt.left), right = bt.right }
  else
    return { value = bt.value, left = bt.left, right = bst.delete(value, bt.right) }
  end
end

function bst.search(value, bt)
  if bst.is_empty(bt) then
    return false
  elseif value == bt.value then
    return true
  elseif value < bt.value then
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
