-- lua module for ocaml

print("Lua module")

local data = require("data")

local function hello_str(name)
    return "Hello, " .. name .. "!"
end

local mod = {}

mod.__version = "1.0"
mod.data = data
mod.hello =
    function(name)
        print( hello_str(name))
    end

return mod
