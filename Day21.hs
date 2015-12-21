module Main where

import Data.List
import Data.Ord

data Item = Item { itemName :: String, itemCost, itemDamage, itemArmor :: Int }
  deriving (Show, Read)

main =
  do print $ minimumBy (comparing itemCost)
           $ filter fight
           $ gearOptions
     print $ maximumBy (comparing itemCost)
           $ filter (not . fight)
           $ gearOptions

emptyItem :: String -> Item
emptyItem name = Item name 0 0 0 

--Weapons:    Cost  Damage  Armor
weapons :: [Item]
weapons =
  [ Item "Dagger"        8     4       0
  , Item "Shortsword"   10     5       0
  , Item "Warhammer"    25     6       0
  , Item "Longsword"    40     7       0
  , Item "Greataxe"     74     8       0
  ]

--Armor:      Cost  Damage  Armor
armors :: [Item]
armors =
  [ Item "Leather"      13     0       1
  , Item "Chainmail"    31     0       2
  , Item "Splintmail"   53     0       3
  , Item "Bandedmail"   75     0       4
  , Item "Platemail"   102     0       5
  ]

-- Rings:      Cost  Damage  Armor
rings :: [Item]
rings =
  [ Item "Damage +1"    25     1       0
  , Item "Damage +2"    50     2       0
  , Item "Damage +3"   100     3       0
  , Item "Defense +1"   20     0       1
  , Item "Defense +2"   40     0       2
  , Item "Defense +3"   80     0       3
  ]

combine :: Item -> Item -> Item
combine x y = Item
  { itemName   = itemName   x ++ " and " ++ itemName y
  , itemCost   = itemCost   x + itemCost   y
  , itemDamage = itemDamage x + itemDamage y
  , itemArmor  = itemArmor  x + itemArmor  y
  }

gearOptions :: [Item]
gearOptions =
  do weapon <- weapons
     armor  <- emptyItem "unarmored" : armors
     ring   <- chooseUpTo 2 rings
     return (foldl1 combine (weapon : armor : ring))

chooseUpTo 0 _ = [[]]
chooseUpTo _ [] = [[]]
chooseUpTo n (x:xs) = map (x:) (chooseUpTo (n-1) xs) ++ chooseUpTo n xs

fight gear = outcome 100 (max 1 (8 - itemArmor gear)) 104 (max 1 (itemDamage gear - 1))

outcome ::
  Int -> Int ->
  Int -> Int ->
  Bool
outcome hp1 dec1 hp2 dec2 = (hp1-1)`quot`dec1 >= (hp2-1)`quot`dec2
