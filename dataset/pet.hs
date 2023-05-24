
data Pet = Cat String | Dog String | Snake

petIsDog :: Pet -> Bool
petIsDog Dog = True
petIsDog _ = False

-- theme: adt, pattern-matching
-- goanna results: 2
-- oracle: true
-- intended fix: 1