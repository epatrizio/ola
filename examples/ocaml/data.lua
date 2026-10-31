-- data init

local function process(data)
    for index, value in pairs(data) do
        data[index] = "value_" .. value
    end
    return data
end

local data = process {
    key_1 = 1,
    key_2 = 2,
    key_3 = 3,
    key_4 = 4
}

return data
