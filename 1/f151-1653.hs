f151 :: Double -> Double
f151 x = if x == (-1)
            then error "Not in codomain"
            else x^2/(1+x)

f152 :: Double -> Double
f152 x = if x >= 2
            then error "Not in codomain"
            else sqrt(3*x-x^3)

f1541 :: Double -> Double
f1541 x = if abs(x)<2
             then error "Not in codomain"
             else logBase 10 (x^2-4)

f1542 :: Double -> Double
f1542 x = if x < 2
             then error "Not in codomain"
             else (logBase 10 (x+2)) + (logBase 10 (x-2))

f1653 :: Double -> Double
f1653 x = if x == 30 || x == 60 || x == 45 || x == 90
             then error "Not in codomain"
             else sqrt(sin(2*x))+sqrt(sin(3*x))

f167 :: Double -> Double
f167 x = if x<60 && x>300
            then error "Not in codomain" 
            else logBase 10 (1-2*cos(x))