-- (**) Decode a run-length encoded list.

-- Given a run-length code list generated as specified in problem 11. Construct its uncompressed version.

data CompressedList a = Single a | Multiple (Int, a) deriving (Show)

uncompressValue :: CompressedList a -> [a]
uncompressValue (Single a) = [a]
uncompressValue (Multiple (x, a)) = replicate x a

decodeCompressedList :: [CompressedList a] -> [a]
decodeCompressedList = foldl (\x y -> x ++ uncompressValue y) []
