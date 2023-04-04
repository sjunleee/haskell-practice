doubleMe x = x + x

doubleUs x y = x * 2 + y * 2

doubleFunc x y = doubleMe x + doubleUs y

doubleSmallNum x = if x > 100 then x else x * 2

length' xs = sum[1|_<-xs]

keepLowerCase st = [ c | c <- st, c 'elem' ['a'..'z']]