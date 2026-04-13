local utils = {}

function utils.is_integer(n)
    return type(n) == "number" and math.floor(n) == n
end

return utils
