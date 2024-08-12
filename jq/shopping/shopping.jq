# Task 1: return the "name" element of the shopping list.
.name,

# Task 2: return the count of the required ingredients.
(.ingredients | length),

# Task 3: return the amount of sugar.
(
    .ingredients[]
    | select(.item == "sugar")
    | .amount.quantity
),

# Task 4: return the mapping of ingredient names with their substitutions
(
    .ingredients + ."optional ingredients"
    | map(select(.substitute))
    | map({ (.item): .substitute })
    | add
)
