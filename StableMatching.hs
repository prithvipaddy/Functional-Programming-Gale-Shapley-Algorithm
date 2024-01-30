import Data.List

type A = String
type B = String 


--0)HELPER FUNCTIONS 

--a)Function that returns the index of a specified element in a given list 
myIndex :: Eq a =>  a -> [a] -> Int 
myIndex x [] = 0
myIndex x (y:ys) | (x==y) =  0
                 | otherwise = 1 + myIndex x ys 


--b)Function that returns the preference list of a proposer. Note preference list is in order or rank. 

lookupPrefs :: A -> [(A, [B])] -> [B]
lookupPrefs x ((y,prefs):ys) | (x == y) = prefs 
                             | otherwise = lookupPrefs x ys

--c)Function that returns the rank of a propose to a proposer given list of proposers and their preference lists. 
numRankA :: [(A,[B])] -> A -> B -> Int 
numRankA (x:xs) y z = (myIndex z (lookupPrefs y (x:xs))) + 1 
 

--d)Function that returns the rank of a proposer to a proposee given list of proposees and their preference lists. 1 is the greatest rank. 
numRankB :: [(B,[A])] -> B -> A -> Int 
numRankB (x:xs) y z = (myIndex z (lookupPrefs y (x:xs))) + 1 


--1)MATCH EVERY PROPOSER TO THEIR HIGHEST RANKED FREE PROPOSEE 
 

--a)Function that checks if a person is present in a list of matchings i.e checks if a person is already matched. 
isPresent :: Eq a => a -> [(a,a)] -> Bool 
isPresent x [] = False 
isPresent x (y:ys) |( (x == fst y) || (x == snd y) ) = True 
                   | otherwise = isPresent x ys 



--b)Function that returns [(A,B)] such that every proposer A is matched to their highest free proposee.

first :: [(A,[B])] -> [(A,B)]
first ((y,prefs):ys) = first' [] ((y,prefs):ys)
  where
    first' :: [(A,B)] -> [(A,[B])] -> [(A,B)]
    first' _ [] = []
    first' list ((y,prefs):ys) | not (isPresent y list) && not (null prefs) && not (isPresent (head prefs) list) = (y, head prefs) : first' ((y, head prefs) : list) ys
                               | otherwise = first' list ((y, tail prefs) : ys)


--Note: null is an inbuilt function that checks whether a list is empty or not. 

--2)CHECKING FOR CROSSES AND FIXING THEM
--We say a cross exists when there are tuples (a,b) and (c,d), but b prefers c over a and c prefers b over d.    

--a)Function that checks if a particular tuple has a cross in a given list of matchings. Also given are lists of preferences of proposers and proposees. 
yesCross :: [(A,[B])] -> [(B,[A])] -> (A,B) -> [(A,B)] -> Bool
yesCross xs ys (a,b) matches = not $ null [(c,d) | (c, d) <- matches, numRankB ys b c < numRankB ys b a, numRankA xs c b < numRankA xs c d]


--b)Function that given a tuple (a,b), returns the tuple with which it crosses in the given list of matches. 
findCross :: [(A,[B])] -> [(B,[A])] -> (A,B) -> [(A,B)] -> (A,B)
findCross xs ys (a,b) matches = head [(c,d) | (c, d) <- matches, numRankB ys b c < numRankB ys b a, numRankA xs c b < numRankA xs c d] 

 
--c)Function that takes in (a,b) and (c,d) and a list of matches and swaps b and d, such that in the output list of matches, (a,d) and (c,b) are the modified tuples.
--x = (a,b) ; y = (c,d); In output (a,d) and (c,b)
mySwap :: (A, B) -> (A, B) -> [(A, B)] -> [(A, B)]
mySwap _ _ [] = []
mySwap x y (m:matches) | (m == x) = (fst m, snd y) : mySwap x y matches
                       | (m == y) = (fst m, snd x) : mySwap x y matches
                       | otherwise = m : mySwap x y matches


--d)Function that handles crosses matches. Swaps partners in the case of a cross.
fixCrosses :: [(A,[B])] -> [(B,[A])] -> [(A,B)] -> [(A,B)]
fixCrosses _ _ [] = []
fixCrosses xs ys (matches) | (yesCross xs ys (head matches) matches) =  mySwap (head matches) (findCross xs ys (head matches) matches) matches
                           | otherwise = (head matches) : fixCrosses xs ys (tail matches) 

--3)FINAL GALE SHAPLEY FUNCTION 
--Function that takes a list of proposers and their preference list, a list of proposes and their preference list and returns the stable matchings 
galeShapley :: [(A,[B])] -> [(B,[A])] -> [(A,B)]
galeShapley [] [] = []
galeShapley xs ys = fixCrosses xs ys (first xs)


--TEST CASE 
proposers :: [(A, [B])]
proposers = [("A", ["1", "2","3"]), ("B", ["2", "1","3"]), ("C",["1","2","3"])]

proposees :: [(B, [A])]
proposees = [("1", ["B","A","C"]), ("2", ["A", "B","C"]), ("3",["A","B","C"])]

result :: [(A, B)]
result = galeShapley proposers proposees

--Output : ghci> result
--[("A","1"),("B","2"),("C","3")]

--Every proposer is matched (i.e A,B and C are matched)
--All matches are stable : 
--("A","1") & ("B","2"): 1 prefers B over A, but B does not reciprocate hence no cross
--("A","1") & ("C","3") : 1 prefers A over C hence no scope of cross 
--("B","2") & ("A","1") : 2 prefers A over B, but A prefers 1 over 2, hence no cross
--("B","2") & ("C","3") :2 prefers B over C, so no cross
--("C","3") & ("A","1"): 3 prefers A over C, but A does not prefer 3 over 1. 
-- ("C","3") & ("B","2") : 3 prefers B over C, but B does not prefer 3 over 2. 





