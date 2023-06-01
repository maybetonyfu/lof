email = ["john@fp.com", "bill@test.org"]
phone = [22490012, 2258267]

-- Pick a format of contact info
-- based on given choice
showContact choice = if choice == "email"
    then "Email: " ++ head email
    else  "Phone: " ++ head phone

-- theme: basics
-- goanna results: 4
-- oracle: true
-- intended fix: 3
-- response time: 0.29905176162719727
-- mus size: 6
-- ghc loc: 0
-- ghc diagnosis: 0
-- ghc fix: 0
-- goanna1: 1
-- goanna2: 2
-- goanna3: 3
-- goanna4: 5
-- goanna5: 5