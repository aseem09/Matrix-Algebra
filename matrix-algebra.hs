type Vector = [Float]
type Row = [Float]
type Matrix = [Row]

-- Compute the inverse of an invertible matrix
inverse :: Matrix -> Matrix
inverse [] = []
inverse matrix = scalarMultiply (1/determinant matrix) (transposeMatrix (cofactorMatrix matrix))

-- Multiply the matrix with a scalar
scalarMultiply :: Float -> Matrix -> Matrix
scalarMultiply _ [] = []
scalarMultiply i (r1:rs) = map (i*) r1 : scalarMultiply i rs

-- Calculate the value of determinant using recursion
determinant :: Matrix -> Float
determinant (r1:r2)
    | (length r1 == 1) = r1 !! 0
    | (length r1 == 2) = (r1 !! 0)*(r2 !! 0 !! 1) - (r2 !! 0 !! 0)*(r1 !! 1)
    | otherwise = alternateSum (zipWith (*) (subMatrixDeterminants 0 (r1:r2)) r1)

-- Calculate alternating sum using recursion
-- AlterateSum of list [a1,a2,a3,a4,a5] = a1-a2+a3-a4+a5
alternateSum :: Vector -> Float
alternateSum [] = 0
alternateSum [a1] = a1
alternateSum (a1:a2:as) = a1 - a2 + alternateSum as

-- Compute value of determinants of the sub matrices generated in the process
subMatrixDeterminants :: Int -> Matrix -> Vector
subMatrixDeterminants _ [] = []
subMatrixDeterminants i (r1:r2) = a1
    where
        a1 = map func [0..(fromIntegral (length r1) - 1)]
        func j = determinant $ dropMatrixColumn j (dropMatrixRow i (r1:r2))

-- Remove a complete row from the matrix
dropMatrixRow :: Int -> Matrix -> Matrix
dropMatrixRow _ [] = []
dropMatrixRow i (r1:r2)
    | (i == 0) = r2
    | otherwise = r1 : dropMatrixRow (i-1) r2

-- Remove a complete column from the matrix
dropMatrixColumn :: Int -> Matrix -> Matrix
dropMatrixColumn _ [] = []
dropMatrixColumn j (r1:r2) = (a1:a2)
    where
        a1 = deleteByIndex j r1
        a2 = dropMatrixColumn j r2

-- Delete element at a given index from a given list
deleteByIndex :: Int -> Vector -> Vector
deleteByIndex _ [] = []
deleteByIndex x (a:as)
    | (x == 0) = as
    | otherwise = a : deleteByIndex (x-1) as

-- Compute the minor matrix
minorMatrix :: Matrix -> Matrix
minorMatrix [] = []
minorMatrix (r1:r2) = a1
    where
        a1 = map func [0..(fromIntegral(length r1) - 1)]
        func x = subMatrixDeterminants x (r1:r2)

-- Compute the row vectors of cofactor matrix
cofactorVector :: Int -> Matrix -> Vector
cofactorVector _ [] = []
cofactorVector i (r1:r2) = a1
    where
        a1 = map func [0..(fromIntegral(length r1) - 1)]
        func j = ((-1)^(i+j))* (determinant $ dropMatrixColumn j (dropMatrixRow i (r1:r2)))

-- Compute the cofactor matrix
cofactorMatrix :: Matrix -> Matrix
cofactorMatrix [] = []
cofactorMatrix (r1:r2) = a1
    where
        a1 = map func [0..(fromIntegral(length r1) - 1)]
        func i = cofactorVector i (r1:r2)

-- Compute the row vectors of transpose matrix
transposeMatrixVector :: Int -> Matrix -> Vector
transposeMatrixVector _ [] = []
transposeMatrixVector _ [[a]] = [a]
transposeMatrixVector i (r1:r2) = (r1 !! i) : transposeMatrixVector i r2

-- Compute the transpose of a matrix
transposeMatrix :: Matrix -> Matrix
transposeMatrix [] = []
transposeMatrix [a] = [a]
transposeMatrix (r1:r2) = a1
    where
        a1 = map func [0..(fromIntegral(length r1) - 1)]
        func i = transposeMatrixVector i (r1:r2)
