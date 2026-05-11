-- initialized data from lua for ocaml

local data = {} -- in global env
data.__version = "1.0"
data.data = { 1, 2, 3, 4 }
data.hello =
    function(name)
        print("Hello, " .. name .. "!")
    end

return data
