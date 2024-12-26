sumGeomProg :: Integer -> Integer -> Integer -> Integer
sumGeomProg a q 0 = a
sumGeomProg a q n = a*q^n + sumGeomProg a q (n-1)