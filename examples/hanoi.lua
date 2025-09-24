--[[
classic example : Tower of Hanoi
Standard recursive implementation

https://interstices.info/les-tours-de-hanoi-un-probleme-classique-de-recursion/
--]]

function Hanoi(n, src, dst, inter)
  if n > 0 then
    Hanoi(n - 1, src, inter, dst)
    print("Disk", n, "from", src, "to", dst)
    Hanoi(n - 1, inter, dst, src)
  end
end

Hanoi(4, 1, 2, 3)
