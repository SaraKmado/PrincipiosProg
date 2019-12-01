import Test.QuickCheck
import qualified Data.List as List

reverse' :: [a] -> [a]
reverse' xs = rev xs [] where
  rev (x:xs) acc = rev xs (x:acc)
  rev [] acc = acc

prop_reverse_length :: [Int] -> Bool
prop_reverse_length xs = length (reverse' xs) == length xs

quickReverseLength = quickCheck prop_reverse_length

sort :: Ord a => [a] -> [a]
sort [] = []
sort (x:xs) = insert x (sort xs)

insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (y:ys)
  | x <= y    = x : y : ys
  | otherwise = y: insert x ys

prop_sort_is_sort :: [Int] -> Bool
prop_sort_is_sort xs = sort xs == List.sort xs

quickSort = quickCheck prop_sort_is_sort

ordered :: Ord a => [a] -> Bool
ordered xs = and $ zipWith (<=) xs (tail xs)

prop_sort_ordered :: [Int] -> Bool
prop_sort_ordered xs = ordered (sort xs)

quickSortOrd = quickCheck prop_sort_ordered

sameElems :: Eq a => [a] -> [a] -> Bool
sameElems xs ys = xs List.\\ ys == ys List.\\ xs

prop_sort_same_elems :: [Int] -> Bool
prop_sort_same_elems xs = sameElems (sort xs) xs

quickElems = quickCheck prop_sort_same_elems

prop_insert_same_elems :: Int -> [Int] -> Bool
prop_insert_same_elems x xs = sameElems (insert x xs) (x:xs)

quickElemsInsert = quickCheck prop_insert_same_elems

prop_insert_ordered :: Int -> [Int] -> Property
prop_insert_ordered x xs =
  ordered xs ==> classify (null xs) "trivial" $ collect (length xs) $ ordered (insert x xs)

quickInsertOrd = quickCheckWith stdArgs {maxSuccess = 500} prop_insert_ordered
