local eucl_div = require("my_mod")

print(eucl_div._NAME)
print(eucl_div._VERSION)

local q, r = eucl_div.euclidian_division(85, 2)
print(q, r)     -- 85 = 42*2 + 1 >> q:42, r:1

q, r = eucl_div.euclidian_division(10, 0)
print(q, r)     -- error: 0 must be positive
