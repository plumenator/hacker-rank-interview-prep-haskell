inf = 999999999999999999
hourglassSum :: [[Int]] -> Int
hourglassSum [] = -inf
hourglassSum [row] = -inf
hourglassSum [row, row2] = -inf
hourglassSum (row:row2:row3:rows) = max (hourglassSum' row row2 row3) (hourglassSum (row2:row3:rows))

hourglassSum' [] _ _ = -inf
hourglassSum' _ [] _ = -inf
hourglassSum' _ _ [] = -inf
hourglassSum' [_] _ _ = -inf
hourglassSum' _ [_] _ = -inf
hourglassSum' _ _ [_] = -inf
hourglassSum' [_, _] _ _ = -inf
hourglassSum' _ [_, _] _ = -inf
hourglassSum' _ _ [_, _] = -inf
hourglassSum' (c:c2:c3:c1s) (_:c4:c8:c2s) (c5:c6:c7:c3s) = max (c + c2 + c3 + c4 + c5 + c6 + c7) (hourglassSum' (c2:c3:c1s) (c4:c8:c2s) (c6:c7:c3s))
