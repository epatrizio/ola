--[[
Oriented object programming in Lua, a more complete example based on:
https://www.lua.org/pil/16.html

Other interesting resources:
http://lua-users.org/wiki/ObjectOrientedProgramming
https://www.tutorialspoint.com/lua/lua_object_oriented.htm
--]]

-- Class

-- CheckingAccount = { balance = 0 }    -- not required (only default value, init in new function)
CheckingAccount = {}

-- memo: syntactic suger = CheckingAccount.new(self, initial_amount)
-- CheckingAccount["new"] = function(self, initial_amount) ...body... end
function CheckingAccount:new(obj, initial_amount)
    local account = obj or {}
    account.balance = initial_amount
    local toString
    if obj and type(obj.toString) == "function" then
        toString = obj.toString
    else
        toString = function(a)
            return "Balance checking_account: " .. a.balance
        end
    end
    -- setmetatable(account, self)  -- bug: https://github.com/epatrizio/ola/issues/42
    account = setmetatable(account, { __index = self, __tostring = toString })

    return account
end

function CheckingAccount:deposite(amount)
    assert(amount > 0)
    self.balance = self.balance + amount
    return self -- same issue (unnecessary return stmt)
end

function CheckingAccount:withdraw(amount)
    assert(amount <= self.balance)
    self.balance = self.balance - amount
    return self -- same issue (unnecessary return stmt)
end

-- Object: Class instance
local ca1 = CheckingAccount:new(nil, 100)
local ca2 = CheckingAccount:new(nil, 500)
-- ca:deposite(50)                        -- same issue
ca1 = ca1:deposite(50)
ca2 = ca2:deposite(150)
ca2 = ca2:withdraw(200)
ca1 = ca1:withdraw(60)
print(ca1) -- Balance checking_account: 90 (100+50-60)
print(ca2) -- Balance checking_account: 450 (500+150-200)

-- Inheritance

BlockAccount = CheckingAccount:new(nil, 0)

function BlockAccount:toString()
    return "Balance block_account: " .. self.balance .. " - limit: " .. self.limit
end

function BlockAccount:deposite(amount)
    assert(amount > 0 and self.balance + amount <= self.limit)
    self.balance = self.balance + amount
    return self -- same issue (unnecessary return stmt)
end

function BlockAccount:withdraw(amount)
    print("Error block_account: impossible withdrawal!")
    return self -- same issue (unnecessary return stmt)
end

local ba1 = BlockAccount:new({ limit = 10000, toString = BlockAccount.toString }, 1000)
local ba2 = BlockAccount:new({ limit = 5000, toString = BlockAccount.toString }, 1500)

ba2 = ba2:deposite(1250)
ba1 = ba1:deposite(5000)
ba1 = ba1:withdraw(42) -- Impossible withdrawal!
ba2 = ba2:withdraw(42) -- Impossible withdrawal!
print(ba1)             -- Balance block_account: 6000 (1000+5000 (-42 KO)) - limit: 10000
print(ba2)             -- Balance block_account: 2750 (1500+1250 (-42 KO)) - limit: 5000
