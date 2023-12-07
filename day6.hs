main = do
    -- Part One

    let times = [63, 78, 94, 68]
    let bestDistances = [411, 1274, 2047, 1035]

    let ranges = map (\t -> [0..t]) times

    let distancesTravelled = map (\r -> map (\t -> ((r !! (-1 + length r) - t)) * t) r) ranges
    
    let indexes = [0..length ranges - 1]
    let filtered = map (\i -> filter (\d -> d > (bestDistances !! i)) (distancesTravelled !! i)) indexes

    let sizes = map (\f -> length f) filtered

    print(product sizes)


    -- Part Two
    let time = 63789468
    let bestDistance = 411127420471035

    let range = [0..time]

    let distancesTravelled' = map (\t -> (time - t) * t) range

    let filtered' = filter (\d -> d > bestDistance) distancesTravelled'

    print(length filtered')
