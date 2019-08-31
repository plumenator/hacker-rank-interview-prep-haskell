rotLeft xs n = take (length xs) . drop n . concat $ repeat xs
