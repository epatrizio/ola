--[[ 
classic example : Conway's Game of Life
--]]

h, w = 35, 25
life_screen = {}

function life_screen_init()
  local rand_val = function(bound)
    local r = math.random(bound)
    return r == 1
  end
  local matrix = {}
  -- create matrix
  for i = 1, h do
    matrix[i] = {}
    for j = 1, w do
      -- matrix[i][j] = false
      matrix[i][j] = rand_val(6)   -- automatic (random) init
    end
  end
  -- active cells - manual init
  -- matrix[11][10] = true
  -- matrix[12][9]  = true
  -- matrix[12][10] = true
  -- matrix[12][11] = true
  return matrix
end

function is_alive(x, y)
  return life_screen[x][y]
end

function life_screen_print()
  local print_cell = function(cell_content)
    if cell_content then io.write("| X ") else io.write("|   ") end
  end
  local print_line = function(line)
    for j = 1, w do
      print_cell(line[j])
    end
    io.write("\n")
  end
  io.write("\027[2J")
  for i = 1, h do
    print_line(life_screen[i])
  end
end

function count_alive(x, y)
  local nb_alive = 0
  local cell_process = function(x, y)
    if (x < 1) or (x >= h) or (y < 1) or (y >= w) then
      return
    elseif is_alive(x, y) then
      nb_alive = nb_alive + 1
    else
      return
    end
  end
  cell_process(x-1, y-1)
  cell_process(x-1, y)
  cell_process(x-1, y+1)
  cell_process(x, y-1)
  cell_process(x, y+1)
  cell_process(x+1, y-1)
  cell_process(x+1, y)
  cell_process(x+1, y+1)
  return nb_alive
end

function life_screen_step()
  local matrix_count_alive = {}
  local matrix_count_alive_init = function()
    local matrix = {}
    for i = 1, h do
      matrix[i] = {}
      for j = 1, w do
        matrix[i][j] = count_alive(i, j)
      end
    end
    return matrix
  end
  local cell_step = function(x, y)
    local nb_alive = matrix_count_alive[x][y]
    if is_alive(x, y) then
      -- first game rule
      life_screen[x][y] = (nb_alive == 2) or (nb_alive == 3)
    else
      -- scd game rule
      life_screen[x][y] = (nb_alive == 3)
    end
  end
  matrix_count_alive = matrix_count_alive_init()
  for i = 1, h do
    for j = 1, w do
      cell_step(i, j)
    end
  end
end

--

life_screen = life_screen_init()

loop = 1
while loop <= 100 do
  life_screen_print()
  os.execute("sleep 0.2")
  life_screen_step()
  loop = loop + 1
end
