--[[
Oriented object programming in Lua, a basic example

Thanks to the power of tables and closures, Lua offers a simple approach to the OOP paradigm.
For an advanced example, see: oop_advanced_*.lua
--]]

-- OOP by prototype (~ Javascript) https://en.wikipedia.org/wiki/Prototype-based_programming
--  https://www.lua.org/pil/16.1.html

A = { key1 = 1, key2 = 2 }
B = { key2 = 22, key3 = 33 }
C = { key3 = 333, key4 = 444 }
D = { key2 = 2222, key5 = 5555 }

-- A is a prototype for B (= B extends A)
B = setmetatable(B, { __index = A })
-- B is a prototype for C (= C extends B extends A)
C = setmetatable(C, { __index = B })

print(A.key1)   -- 1
print(A.key2)   -- 2
print(A.key3)   -- nil
print(A.key4)   -- nil

print(B.key1)   -- 1
print(B.key2)   -- 22
print(B.key3)   -- 33
print(B.key4)   -- nil

print(C.key1)   -- 1
print(C.key2)   -- 22
print(C.key3)   -- 333
print(C.key4)   -- 444

-- Lua is dynamic, the type can change, the prototype can change!
-- D is now the prototype for C (= C extends D)
C = setmetatable(C, { __index = D })

print(C.key1)   -- nil
print(C.key2)   -- 2222
print(C.key3)   -- 333
print(C.key4)   -- 444
print(C.key5)   -- 5555

AbstractGeometric = {}

-- AbstractGeometric.new = function (name)
function AbstractGeometric.new(name)
    local self = {}
    local name = name

    self.getName = function()
        return name
    end

    self.toString = function()
        return "AbstractGeometric:" .. name
    end

    self.perimeter = function()
        print("To be implemented!")
    end

    return self
end

Point = {}

-- Point.new = function (name, x, y)
function Point.new(name, x, y)
    local self = AbstractGeometric.new(name)
    local x = x
    local y = y

    self.perimeter = function()
        return 0
    end

    self.toString = function()
        return "Point:" .. self.getName() .. ";x:" .. x .. ";y:" .. y
    end

    return self
end

Circle = {}

-- Circle.new = function (name, x, y, r)
function Circle.new(name, x, y, r)
    local self = Point.new(name, x, y)
    local radius = r
    local center = Point.new("Center", x, y)

    self.toString = function()
        return "Circle:" .. self.getName() .. ";" .. center.toString() .. ";radius:" .. r
    end

    self.perimeter = function()
        return 2 * 3.14 * radius
    end

    return self
end

local point1 = Point.new("My point 1", 0, 1)
local point2 = Point.new("My point 2", 1, 2)

local circle1 = Circle.new("My circle 1", 1, 1, 10)
local circle2 = Circle.new("My circle 2", 2, 2, 20)

print(point1.getName())
print(point1.toString())
print(point1.perimeter())

print(point2.getName())
print(point2.toString())
print(point2.perimeter())

print(circle1.getName())
print(circle1.toString())
print(circle1.perimeter())

print(circle2.getName())
print(circle2.toString())
print(circle2.perimeter())

-- Minimal example with ':' colon syntax
-- syntactic sugar:
--  Class:new(name) is syntactic sugar for Class.new(self, name)
--  v:name(args) call is syntactic sugar for v.name(v,args)

Class = {}

function Class:new(name)
    self.name = name
    return self
end

-- print(Class["new"])      -- function: ID
-- print(Class.new)         -- function: ID
local cl = Class:new("my_class")

-- print(cl)                -- table: ID
print(cl.name)

-- An oop implementation of classic functional programming Option type

Option = {}

function Option.Some(value)
    local self = {}
    local value = value

    self.getValue = function()
        return value
    end

    self.isSome = function()
        return true
    end

    self.isNone = function()
        return false
    end

    return self
end

function Option.None()
    local self = {}
    local value = nil

    self.getValue = function()
        return value
    end

    self.isSome = function()
        return false
    end

    self.isNone = function()
        return true
    end

    return self
end

-- An Option type is always returned
local function div_o(a, b)
    if b == 0 then
        return Option.None()
    else
        return Option.Some(a / b)
    end
end

local d = div_o(84, 2)
if d.isSome() then print(d.getValue()) else print("error: div by zero") end
d = div_o(42, 0)
if d.isSome() then print(d.getValue()) else print("error: div by zero") end

-- An oop implementation of classic functional programming Result type

Result = {}

function Result.Ok(value)
    local self = {}
    local value = value
    local error = nil

    self.getValue = function()
        return value
    end

    self.getError = function()
        return error
    end

    self.isOk = function()
        return true
    end

    self.isError = function()
        return false
    end

    return self
end

function Result.Error(error)
    local self = {}
    local value = nil
    local error = error

    self.getValue = function()
        return value
    end

    self.getError = function()
        return error
    end

    self.isOk = function()
        return false
    end

    self.isError = function()
        return true
    end

    return self
end

-- A Result type is always returned
local function div_r(a, b)
    if b == 0 then
        return Result.Error("error: div by zero")
    else
        return Result.Ok(a / b)
    end
end

d = div_r(84, 2)
if d.isOk() then print(d.getValue()) else print(d.getError()) end
d = div_r(42, 0)
if d.isOk() then print(d.getValue()) else print(d.getError()) end
