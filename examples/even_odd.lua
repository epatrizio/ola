--[[ 
classic example : mutually recursive even-odd functions
recursivity
--]]

function even(n)
  if n == 0 then
    return true
  elseif n > 0 then
    return odd(n-1)
  else
    return even(-n)
  end
end

function odd(n)
  if n == 0 then
    return false
  elseif n > 0 then
    return even(n-1)
  else
    return odd(-n)
  end
end

function parity(is_even, n)
  local p
  if is_even then
    p = even(n)
  else
    p = odd(n)
  end
  return p
end

print(even(0), odd(0))
print(parity(true, 0), parity(false, 0))
print(even(42), odd(42))
print(parity(true, 42), parity(false, 42))
print(even(-42), odd(-42))
print(parity(true, -42), parity(false, -42))
