-- print(math.abs)          -- ok (function id)
print(math.abs(1.5))
print(math.abs(-1.5))
print(math.abs(-42))
print(math.abs("1.5"))
print(math.abs("-42"))
-- print(math.abs("ko"))    -- ok (no float representation)
-- print(math.abs(nil))     -- todo typing error
