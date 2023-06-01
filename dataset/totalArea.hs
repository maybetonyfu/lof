data Shape = Circle Float | Rectangle Float Float

area (Circle r) = pi * r * r
area (Rectangle w h) = w * h

shapes = [Rectangle 2.0, Circle 1.0]


-- theme: adt
-- goanna results: 2
-- oracle: true
-- intended fix: 2
-- response time: 0.19248199462890625
-- mus size: 4
-- ghc loc: 0
-- ghc diagnosis: 0
-- ghc fix: 0
-- goanna1: 1
-- goanna2: 2
-- goanna3: 2
-- goanna4: 2
-- goanna5: 2