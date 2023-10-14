print(1)
print(2)
a = 7
goto l1
print(3)
::l2::
print(4)
goto l3
::l1::
do
    print(5)
    goto l2
    print(6)
end
::l3::
print(a)
goto l42

-- TODO: nested function restriction
-- ::lab::
-- print(x)

-- function f()
--     x = 3
--     goto lab     -- error, lab not visible
-- end

-- f()
