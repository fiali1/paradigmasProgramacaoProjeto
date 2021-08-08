module Tools (
    goLeftNTimes,
    goRightNTimes
) where

import qualified Data.List.Zipper as Z

goRightNTimes :: Z.Zipper a -> Int -> Z.Zipper a
goRightNTimes z 0 = z
goRightNTimes z n
  | Z.endp z  = z
  | otherwise = goRightNTimes (Z.right z) (n - 1)

goLeftNTimes :: Z.Zipper a -> Int -> Z.Zipper a
goLeftNTimes z 0 = z
goLeftNTimes z n
  | Z.beginp z  = z
  | otherwise = goLeftNTimes (Z.left z) (n - 1)