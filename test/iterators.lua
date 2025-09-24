-- Examples from https://www.tutorialspoint.com/lua/lua_iterators.htm

-- Stateless iterators

local function square(iteratorMaxCount, currentNumber)
  if currentNumber < iteratorMaxCount then
    currentNumber = currentNumber + 1
    return currentNumber, currentNumber * currentNumber
  end
end

local function squares(iteratorMaxCount)
  return square, iteratorMaxCount, 0
end

for i, n in square, 10, 0 do
  print(i, n)
end

for i, n in squares(5) do
  print(i, n)
end

local function mul23(iteratorMaxCount, currentNumber)
  if currentNumber < iteratorMaxCount then
    currentNumber = currentNumber + 1
    -- iterator function should return a list of values
    -- first one is always the control variable
    return currentNumber, currentNumber * 2, currentNumber * 3
  end
end

for i, i2, i3 in mul23, 5, 0 do
  print(i, i2, i3)
end

-- Stateful iterators

local array = { "elt1", "elt2", "elt3" }

local function iterator(collection)
  local index = 0
  local count = #collection
  -- The closure function is returned
  return function()
    index = index + 1
    if index <= count then
      -- return the current element(s) of the iterator
      -- same as above, it's possible to return several values,
      -- but it's the closure that manages the current context (here= local index).
      return index, collection[index]
    end
  end
end

for idx, elt in iterator(array) do
  print(idx, elt)
end
