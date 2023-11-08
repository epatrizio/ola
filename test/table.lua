table1 = {}
print(table1)

table1 = {1, "2", 1+2, nil, true, {1;2;3}}
print(table1[1])

--table1[2] = 2.0
print(table1[2])

print(table1[6][3])
--table1[6][3] = "3.5"
print(table1[6][3])

--table1["key1"] = "val1"
--table1["key".."2"] = "val".."2"
print(table1["key1"])
print(table1["key2"])

function p(a)
  print("my_print:")
  print(a)
end
p(table1["key1"])
