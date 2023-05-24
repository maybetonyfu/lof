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