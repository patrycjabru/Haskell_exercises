incAndtriple v = ( v + 1 ) * 3

specialCases 1 = "Hello"
specialCases 2 = " "
specialCases 3 = "World"
specialCases 4 = "!"
specialCases x = "???"

head_a (x:_) = x 


ownLength [] = 0
ownLength (_:xs) = 1 + ownLength xs

taka_a 1 (h:_) = [h]
take_a x (h:t) = h : (take_a (x-1) t)
