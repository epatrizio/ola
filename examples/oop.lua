--[[
Oriented object programming in Lua, a basic example

Thanks to the power of tables and closures, Lua offers a simple approach to the OOP paradigm.
For more detailed information and examples: (which requires the use of metatable)
https://www.lua.org/pil/16.html
https://www.tutorialspoint.com/lua/lua_object_oriented.htm
--]]

AbstractGeometric = {}

-- AbstractGeometric.new = function (name)
function AbstractGeometric.new(name)
    local self = {}
    local name = name

    self.getName = function()
        return name
    end

    self.toString = function()
        return "AbstractGeometric:"..name
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
        return "Point:"..self.getName()..";x:"..x..";y:"..y
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
        return "Circle:"..self.getName()..";"..center.toString()..";radius:"..r
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
