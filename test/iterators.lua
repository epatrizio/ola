-- Examples from https://www.tutorialspoint.com/lua/lua_iterators.htm

-- Stateless iterators

local function square(iteratorMaxCount, currentNumber)
  if currentNumber<iteratorMaxCount then
    currentNumber = currentNumber + 1
    return currentNumber, currentNumber*currentNumber
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

-- Stateful iterators - TODO: to be implemented
-- https://github.com/epatrizio/ola/issues/18 (Issue bug closure impl)

-- local array = {"elt1", "elt2", "elt3"}

-- function iterator(collection)
--   local index = 0
--   local count = #collection
--   -- The closure function is returned
--   return function ()
--     index = index + 1
--     if index <= count then
--       -- return the current element of the iterator
--       return collection[index]
--     end
--   end
-- end

-- for elt in iterator(array) do
--   print(elt)
-- end
