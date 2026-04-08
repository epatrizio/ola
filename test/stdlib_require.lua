local hello, i = require("stdlib_require_mod")

print(type(hello_global))   -- defined in stdlib_require_mod as global var
print(hello_global)         -- hello, world global!

print(type(hello))          -- return from stdlib_require_mod
print(hello)                -- hello, world local!

print(type(i))              -- return from stdlib_require_mod
print(i)                    -- 42
