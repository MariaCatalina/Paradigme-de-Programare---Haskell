module AISimple where

import Board
import Interactive

{-
    *** TODO ***

    Întoarce tabla rezultată din aplicarea acelei mutări care maximizează
    scorul adversarului.
-}

succ board = let s = successors board
				 in  map (\a -> (a,(snd (scores (snd a))))) s

step :: Board -> (House, Board)
step board = let s = successors board; -- lista cu succesorii
				--  perechi de forma (House , scor oponent)
				 lista = map (\a -> ((fst a), (snd (scores (snd a))))) s;
				-- lista doar cu scoruri
				 lsort = map (\a -> (snd (scores (snd a)))) s;
				-- valoarea maxima dintre scoruri
				 max = maximum lsort;
				-- b - pereche de forma (casa , scor)
				 b = head $ filter (\a -> ((snd a) == max )) lista;
				-- lista de forma [(casa , Board)]
				 final = filter (\a -> ((fst a) == (fst b)) ) s
				 in head final
{-
    Urmărește pas cu pas evoluția jocului, conform strategiei implementate.
-}
userMode :: IO ()
userMode = humanVsAI step
