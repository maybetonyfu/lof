email1 = "john@smith.com"
age1 = 24
registered1 = True

email2 = "jane@smith.com"
age2 = 20
registered2 = False

users = [(email1, age1, registered1), (email2, age2, registered2)]

hasUserRegistered (email, registered) = registered

registered :: [Bool]
registered = map hasUserRegistered users

-- theme: tuple, function
-- goanna results: 6
-- oracle: true
-- intended fix: 5