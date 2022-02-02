newtype ZipList' a = ZipList' { getZipList :: [a] }
newtype CharList = CharList { getCharList :: [Char] } deriving (Eq, Show)
