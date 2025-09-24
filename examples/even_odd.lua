--[[
classic example : mutually recursive even-odd functions
recursivity
--]]

function Even(n)
  if n == 0 then
    return true
  elseif n > 0 then
    return Odd(n - 1)
  else
    return Even(-n)
  end
end

function Odd(n)
  if n == 0 then
    return false
  elseif n > 0 then
    return Even(n - 1)
  else
    return Odd(-n)
  end
end

function Parity(is_even, n)
  local p
  if is_even then
    p = Even(n)
  else
    p = Odd(n)
  end
  return p
end

print(Even(0), Odd(0))
print(Parity(true, 0), Parity(false, 0))
print(Even(42), Odd(42))
print(Parity(true, 42), Parity(false, 42))
print(Even(-42), Odd(-42))
print(Parity(true, -42), Parity(false, -42))
