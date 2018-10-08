my_bool = colors[myself % N + 1] == 2

hands = raise(my_bool)

if myself == 1 then behindMe = 13 else behindMe = myself - 1 end

if hands[behindMe] then
  answer = 2
else
  answer = 1
end
