--[[
Oriented object programming in Lua, a more complete example based on:
https://www.lua.org/pil/16.html

Other interesting resources:
http://lua-users.org/wiki/ObjectOrientedProgramming
https://www.tutorialspoint.com/lua/lua_object_oriented.htm
--]]

-- Class

CheckingAccount = { balance = 0 }

-- memo: syntactic suger = CheckingAccount.new(self, initial_amount)
-- CheckingAccount["new"] = function(self, initial_amount) ...body... end
function CheckingAccount:new(obj, initial_amount)
  local account = obj or {}
  -- setmetatable(account, self)                      -- bug: https://github.com/epatrizio/ola/issues/35
  self.balance = initial_amount
  self.__index = self
  if obj and type(obj.toString) == "function" then -- same issue
    self.__tostring = obj.toString
  else
    self.__tostring =
      function(a)
        return "Balance checking_account: " .. a.balance
      end
  end
  account = setmetatable(account, self)   -- same issue
  return account
end

function CheckingAccount:deposite(amount)
  assert(amount > 0)
  self.balance = self.balance + amount
  return self                             -- same issue (unnecessary return stmt)
end

function CheckingAccount:withdraw(amount)
  assert(amount <= self.balance)
  self.balance = self.balance - amount
  return self                             -- same issue (unnecessary return stmt)
end

-- Object: Class instance
local ca = CheckingAccount:new(nil, 100)
-- ca:deposite(50)                        -- same issue
ca = ca:deposite(50)
ca = ca:withdraw(60)
print(ca)           -- Balance checking_account: 90 (100+50-60)

-- Inheritance

BlockAccount = { limit = 0 }

function BlockAccount:new(initial_amount, initial_limit)
  local ba = CheckingAccount:new(self, initial_amount)
  self.limit = initial_limit
  ba.limit = initial_limit                -- same issue (unnecessary)
  return ba
end

function BlockAccount:toString()
  return "Balance block_account: " .. self.balance .. " - limit: " .. self.limit
end

function BlockAccount:deposite(amount)
  assert(amount > 0 and self.balance + amount <= self.limit)
  self.balance = self.balance + amount
  return self                             -- same issue (unnecessary return stmt)
end

function BlockAccount:withdraw(amount)
  print("Error block_account: impossible withdrawal!")
  return self                             -- same issue (unnecessary return stmt)
end

local ba = BlockAccount:new(1000, 10000)
ba = ba:deposite(5000)
ba = ba:withdraw(42)     -- Impossible withdrawal!
print(ba)                -- Balance block_account: 6000 (1000+5000 (-42 KO)) - limit: 10000
