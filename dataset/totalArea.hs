data Shape = Circle Float | Rectangle Float Float

area (Circle r) = pi * r * r
area (Rectangle w h) = w * h

shapes = [Rectangle 2.0, Circle 1.0]


--theme: adt
-- goanna results: 2
-- oracle: true
-- intended fix: 2