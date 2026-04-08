--[[
Minimal module example (classic implementation with lua table)
--]]

local mod = {}

mod._NAME = "Euclidean division module"
mod._VERSION = "1.0"

local function is_integer(n)
    return type(n) == "number" and math.floor(n) == n
end

local function floor(dividend, divisor)
    assert(is_integer(dividend), "dividend must be an integer")
    assert(is_integer(divisor), "divisor must be an integer")
    assert(dividend > 0, "dividend must be positive")
    assert(divisor > 0, "divisor must be positive")
    return dividend // divisor
end

local function modulo(dividend, divisor)
    assert(is_integer(dividend), "dividend must be an integer")
    assert(is_integer(divisor), "divisor must be an integer")
    assert(dividend > 0, "dividend must be positive")
    assert(divisor > 0, "divisor must be positive")
    return dividend % divisor
end

function mod.euclidian_division(dividend, divisor)
    local q = floor(dividend, divisor)
    local r = modulo(dividend, divisor)
    return q, r
end

return mod
