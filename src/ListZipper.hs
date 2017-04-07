module ListZipper where 

-- Infinite lazy list zipper
data ListZipper a = LZ [a] a [a]
type BFArray = ListZipper Int

instance Show a => Show (ListZipper a) where
    show (LZ ls c rs) = show (reverse $ take 3 ls) ++ " " ++ show c ++ " " ++ show (take 3 rs)

mkZipper :: a -> ListZipper a
mkZipper x = LZ (repeat x) x (repeat x)

-- Moving --
zLeft :: ListZipper a -> ListZipper a
zLeft  (LZ (l:ls) c rs) = LZ ls l (c:rs)

zRight :: ListZipper a -> ListZipper a
zRight  (LZ ls c (r:rs)) = LZ (c:ls) r rs

-- Reading --
getLeft :: ListZipper a -> [a]
getLeft (LZ ls _ _) = ls

getCursor :: ListZipper a -> a
getCursor (LZ _ x _) = x

getRight :: ListZipper a -> [a]
getRight (LZ _ _ rs) = rs

-- Writing --
modify :: (a -> a) -> ListZipper a -> ListZipper a
modify f (LZ ls x rs) = LZ ls (f x) rs

set :: a -> ListZipper a -> ListZipper a
set = modify . const