-- I can't think in a silly 1-indexed world
myself0 = myself - 1
function colors0(i)
  return colors[i + 1] ~= nil and colors[i + 1] - 1 or nil
end

my_bool_n = 0

for i = 0, N - 2 do
  local iAnd1Ahead = (myself0 + i + 1) % N
  my_bool_n = my_bool_n ~ (1 & (colors0(iAnd1Ahead) >> i))
end

my_bool = my_bool_n == 1

hands = raise(my_bool)

-- More 1-indexing sanitation
function hands0(i)
  return hands[i + 1]
end

answer = 0

for bitPosition = 0, N - 2 do
  local behindByIAnd1 = (N + myself0 - bitPosition - 1) % N
  local x = hands0(behindByIAnd1) and 1 or 0
  for j = 0, N - 2 do
    local jm = ((behindByIAnd1 + j + 1) % N)
    if jm ~= myself0 then
      x = x ~ (1 & (colors0(jm) >> j))
    end
  end
  if x == 1 then
    answer = answer | (1 << bitPosition)
  end
end

answer = answer + 1
