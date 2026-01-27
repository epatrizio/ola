--[[
Oriented object programming in Lua, a more complete example based on:
https://www.lua.org/pil/16.html

oop_advanced_1.lua
ca.balance = -42 -- Permitted!
  In practice, all elements in CheckingAccount and BlockAccount are public

https://www.lua.org/pil/16.4.html:
"Many people consider privacy to be an integral part of an object-oriented language"
"The basic idea of this alternative design is to represent each object through two tables:
one for its state; another for its operations, or its interface."
--]]

function NewCheckingAccount(initial_amount)
    local self = { balance = 0 }

    local function getBalance()
        return self.balance
    end

    local function setBalance(amount)
        assert(amount >= 0)
        self.balance = amount
    end

    local function deposite(amount)
        assert(amount > 0)
        setBalance(self.balance + amount)
    end

    local function withdraw(amount)
        assert(amount <= self.balance)
        setBalance(self.balance - amount)
    end

    local function toString()
        return "Balance checking_account: " .. getBalance()
    end

    setBalance(initial_amount)

    -- Nb. self and setBalance function are private because they are not part of the return table below
    return setmetatable({
        getBalance = getBalance,
        deposite = deposite,
        withdraw = withdraw,
    }, { __tostring = toString })
end

local ca_p = NewCheckingAccount(100)
ca_p.deposite(50)
ca_p.withdraw(60)
print(ca_p)              -- Balance checking_account: 90 (100+50-60)
print(ca_p.balance)      -- nil!
ca_p.balance = 0         -- try to modify the balance
print(ca_p.getBalance()) -- 90

-- Inheritance

function NewBlockAccount(initial_amount, initial_limit)
    local limit
    local self = {}
    local check_acc = NewCheckingAccount(initial_amount)    -- BlockAccount extends CheckingAccount

    function self.getLimit()
        return limit
    end

    local function setLimit(l)
        assert(l > 0)
        limit = l
    end

    local function toString()
        return "Balance block_account: " .. check_acc.getBalance() .. " - limit: " .. limit
    end

    function self.deposite(amount)
        assert(amount > 0 and check_acc.getBalance() + amount <= limit)
        check_acc.deposite(amount)      -- parent function call
    end

    function self.withdraw(amount)
        print("Error block_account: impossible withdrawal!")
    end

    setLimit(initial_limit)

    -- BlockAccount extends CheckingAccount
    self = setmetatable(self, { __index = check_acc, __tostring = toString })

    return self
end

local ba_p = NewBlockAccount(1000, 10000)
ba_p.deposite(5000)
ba_p.withdraw(42)         -- Impossible withdrawal!
print(ba_p)               -- Balance block_account: 6000 (1000+5000 (-42 KO)) - limit: 10000
print(ba_p.balance)       -- nil!
print(ba_p.limit)         -- nil!
ba_p.balance = 0          -- try to modify the balance
ba_p.limit = 0            -- try to modify the limit
print(ba_p)
