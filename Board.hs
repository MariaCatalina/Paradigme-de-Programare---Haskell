
{-
    Tabla de joc și mutările posibile.

    Modulul exportă numai funcțiile enumerate mai jos, ceea ce înseamnă
    că doar acestea vor fi vizibile din alte module. Justificarea vizează
    prevenirea accesului extern la structura obiectelor 'Board', și a eventualei
    coruperi a consistenței interne a acestora.
-}
module Board
    ( Board
    , Player (..)  -- Exportăm și constructorii de date 'You' și 'Opponent'.
    , House
    , build
    , yourSeeds
    , oppsSeeds
    , who
    , isOver
    , initialBoard
    , move
    , scores
    , successors
    ) where

import Consecutive

{-
    Jucătorul care urmează să mute.
-}
data Player = You | Opponent deriving (Eq, Show)

{-
    Tipul caselor, definit pentru lizibilitate.
-}
type House = Int

{-
    *** TODO ***

    Definiți tipul 'Board', astfel încât să rețină informație despre starea
    jocului, inclusiv tabla de joc.

    Observați că viitorii constructori de date și eventualele câmpuri pe care
    le veți specifica nu vor apărea în lista de funcții exportate de modul
    (vezi explicația de la începutul fișierului).
-}

{-
 	Board =  Player (Tabela,My-Score) (Tabela,Opponent-Score)
-}
data Board = Board Player ([House], Int) ([House],Int) deriving Eq

{-
    *** TODO ***

    Instanțiați clasa 'Show' cu tipul 'Board'. Exemplu de reprezentare,
    unde scorurile sunt aferent jucătorilor 'You', respectiv 'Opponent':

       4  4  4  4  4  4
     0                  0    Next: You, Playing, Score: (0,0)
       4  4  4  4  4  4
-}
--sList :: [House] -> [House]
sList [] = []
sList lista = (show (head lista)) ++ "  " ++ sList (tail lista)

instance Show Board where
    show (Board jucatorCurent jucator1 jucator2) = "  " ++ sList (fst jucator2) ++ "\n" 
    											   	    ++ (show (snd jucator2)) 
    								   	    			++ "                  " 
    								        			++ (show (snd jucator1)) ++ "    " 
    								        			++ "Next: " ++ (show jucatorCurent) ++ ", "
    								        			++ "Playing, " ++ "Score: " ++ "(" 
    								        			++ (show (snd jucator1)) ++ ","
    								        			++ (show (snd jucator2)) ++ ")" ++ "\n"	
    								        			++ "  " ++ sList (fst jucator1)

{-
    *** TODO BONUS ***

    Instanțiați clasa 'Consecutive', pentru a putea determina dacă în două
    configurații ale tablei trebuie să mute același jucător.
-}
instance Consecutive Board where
    b1 >< b2 = undefined

{-
    *** TODO ***

    Construiește tabla de joc.

    Funcția trebuie să determine dacă jocul este deja încheiat, pe baza
    conținutului caselor.
-}
build :: ([Int], Int)  -- Conținutul caselor și al depozitului utilizatorului
      -> ([Int], Int)  -- Conținutul caselor și al depozitului adversarului
      -> Player        -- Jucătorul aflat la rând
      -> Board         -- Tabla construită

build jucator1 jucator2 jucatorCurent = Board jucatorCurent jucator1 jucator2

{-
    *** TODO ***

    Întoarce conținutul caselor și al depozitului utilizatorului.
-}
yourSeeds :: Board -> ([Int], Int)
yourSeeds (Board _ jucator1 _) = jucator1 

{-
    *** TODO ***

    Întoarce conținutul caselor și al depozitului adversarului.
-}

oppsSeeds :: Board -> ([Int], Int)
oppsSeeds (Board _ _ jucator2) = jucator2

{-
    *** TODO ***

    Întoarce jucătorul aflat la rând.
-}
who :: Board -> Player
who (Board jucatorCurent _ _ ) = jucatorCurent

{-
    *** TODO ***

    Întoarce 'True' dacă jocul s-a încheiat.
-}
{-
	Daca lista formata prin aplicare lui filter are aceeasi lungine ca lista initiala 
	=> lista are numai 0 pe fiecare casa din tabela
-}
isOver :: Board -> Bool
isOver (Board jucatorCurent jucator1 jucator2)
	| (length $ filter (\a -> a == 0)  $ fst jucator1 ) == (length $ fst jucator1) = True
	| (length $ filter (\a -> a == 0) $ fst jucator2 ) == (length $ fst jucator2) = True
	| otherwise  = False

{-
    *** TODO ***

    Tabla inițială.
-}

initialBoard :: Board
initialBoard = Board You ([4,4,4,4,4,4],0) ([4,4,4,4,4,4],0)

{-
    *** TODO ***

    Realizează o mutare pornind de la casa furnizată ca parametru, în funcție
    de configurația actuală a tablei și de jucătorul aflat la rând.

    Întoarce aceeași configurație dacă mutarea nu poate fi efectuată
    din diverse motive, precum numărul eronat al casei, sau casa goală.
-}
{-
	Functia pune 0 pe o pozitie data
-}
setOnZero :: House -> [House] -> [House]	
setOnZero a lista = take a lista ++ [0] ++ drop (a + 1) lista
 
{-
	Functia modifica lista jucatorului You si verifica daca ultima saminta pica pe o casa goala.
	In caz afirmativ verifica casa adversarului de la acceasi pozitie si actualizeaza scorul
-}
changeSYou :: House -> House -> House -> Board -> Board
changeSYou a seminte pCurent board
	| pCurent == 6 = board
	-- pune seminte pana la ultima
	| pCurent > a && seminte > 1 = let lista = (fst (yourSeeds board));
														   x = lista !! pCurent;
														   lNew = take pCurent lista ++ [x + 1] ++ drop (pCurent + 1) lista;
														   b = build (lNew, (snd (yourSeeds board))) (oppsSeeds board) Opponent
														   in changeSYou a (seminte - 1) (pCurent + 1) b
	-- verifica pentru ultima seminta valoare din tabela opusa
	| pCurent > a && seminte == 1 = let you = (fst (yourSeeds board));
										opp = (fst (oppsSeeds board));
										x = you !! pCurent;
									    y = opp !! pCurent;
							-- daca ultima saminta pica pe o casuta = 0 se uita cate seminte sunt in casuta adversa
										jucator1 = if x == 0 && y /= 0 then ((setOnZero pCurent you),(snd (yourSeeds board)) + 1 + y) 
													else  (take pCurent you ++ [x + 1] ++ drop (pCurent + 1 ) you,(snd (yourSeeds board))) ;
										jucator2 = if x == 0 && y /= 0 then (setOnZero pCurent opp, (snd (oppsSeeds board))) else (oppsSeeds board) 
											in build jucator1 jucator2 Opponent 
	| otherwise = changeSYou a seminte (pCurent + 1) board														   

{-
	Modifice lista oponentului in functie de numarul de seminte dat
-}
changeOp :: House -> House -> [House] -> [House]
changeOp seminte pCurent lista
	| pCurent < 0 = lista
	| pCurent >= 0 && seminte > 0 = let x = lista !! pCurent;
									   lNew = take pCurent lista ++ [x + 1] ++ drop (pCurent + 1) lista
									   in changeOp (seminte - 1) (pCurent - 1) lNew
	| otherwise = changeOp seminte (pCurent - 1) lista								   

{-
	Functia modifica tabelele jucatorilor in funtie de numarul de seminte dat.
	c = 0 -  modifica lista juctorului You
	c = 1 - modifica lista oponentului
	a - pozitia de la care se pleaca
	seminte - numarul de seminte de la pozitia intiala , modificandu-se apoi in numarul de seminte ramas	
-}
myTurn :: House -> House -> House -> Board -> Board
myTurn c a seminte board
	| c == 0 && (a + seminte) < 6 = changeSYou a seminte 0 board
	| c == 0 && (a + seminte) == 6 = let lista = (fst (yourSeeds board) , (snd (yourSeeds board)) + 1);
										  b = changeSYou a seminte 0 (build lista (oppsSeeds board) Opponent)
										  -- daca ultima samanta pica in depozit se pastreaza randul jucatorului curent
										  in build (yourSeeds b) (oppsSeeds b) You
	-- daca numarul de seminte este mai mare decat lungimea listei se prelucreaza si se apeleaza recursiv pentru lista oponentului
	| c == 0 && (a + seminte) > 6 = let lista = (fst (yourSeeds board) , (snd (yourSeeds board)) + 1);
										b = changeSYou a seminte 0 (build lista (oppsSeeds board) Opponent)
										in myTurn 1 (0 - 1) (seminte - (6 - a ) ) b
	| c == 1 && seminte <= 6 = let lista = changeOp seminte 5 (fst (oppsSeeds board))
									in build (yourSeeds board) (lista ,(snd (oppsSeeds board))) Opponent
	-- daca numarul de seminte este mai mare decat lista curenta se prelucreaza si se apleaza recursiv pentru lista You
	-- cu pozitia -1 pentru a se incepe de la 0
	| c == 1 && seminte > 6 = let lista = changeOp seminte 5 (fst (oppsSeeds board));
								  b = build (yourSeeds board) (lista ,(snd (oppsSeeds board))) Opponent
								  -- (0 - 1) - cand se intoarce la lista jucatorui curent sa completeze cu seminte de la pozitia 0
								  in myTurn 0 (0 - 1) (seminte - 6) b
	| otherwise = board 

{-
		Functia completeaza cu seminte tabela oponentului si verifica daca 
	casa unde ultima seminta este pusa este egala cu 0 
-}
changeSOpp :: House -> House -> House -> Board -> Board
changeSOpp a seminte pCurent board
	| pCurent < 0 = board
	-- daca nu s-a ajuns la ultima seminta 
	| pCurent < a && seminte > 1 = let lista = (fst (oppsSeeds board));
										x = lista !! pCurent;
										lNew = take pCurent lista ++ [x + 1] ++ drop (pCurent + 1) lista
										in changeSOpp a (seminte - 1) (pCurent - 1) (build (yourSeeds board) (lNew , (snd (oppsSeeds board))) You)
	-- daca s-a ajuns la ultima seminta se vedifica casa unde urmaza sa fie pusa si casa adversarului
	| pCurent < a && seminte == 1 = let you = (fst (oppsSeeds board));
										opp = (fst (yourSeeds board));
										x = you !! pCurent ;
										y = opp !! pCurent ;
										-- daca casa curenta e 0 se pune 0 pe pozitia data si se actualizeaza scorul
										jucator1 = if x == 0 && y /= 0 then ((setOnZero pCurent you),(snd (oppsSeeds board)) + 1 + y) 
													else  (take pCurent  you ++ [x + 1] ++ drop (pCurent + 1)  you,(snd (oppsSeeds board))); 
										-- daca casa curenta e 0 se pune 0 pe accesi pozitie in casa adversarului			
										jucator2 = if x == 0 && y /= 0 then (setOnZero pCurent opp, (snd (yourSeeds board))) else (yourSeeds board)
										in build jucator2 jucator1 You 		
	| otherwise = changeSOpp a seminte (pCurent - 1) board									

{-
	Functie recursiva care adauga seminte pe lista jucatorului You in functie de numarul dat
-}
changeYou :: House -> House -> [House] -> [House]
changeYou seminte pCurent lista
	| pCurent == (length lista) = lista
	| seminte > 0 = let x = lista !! pCurent;
					 l = take pCurent lista ++ [x + 1] ++ drop (pCurent + 1) lista
				     in changeYou (seminte - 1) (pCurent + 1) l
	| otherwise = changeYou (seminte - 1) (pCurent + 1) lista

{-
	Functie pentru mutarile oponenului.
	Se verifica numarul de seminte disponibile si se prelucreza tabela 
	c = 0 - partea jucatorului Opponent
	c = 1 - partea jucatorului You 
	a - pozitia de la care se pleaca
	seminte - numarul de seminte de la pozitia intiala , modificandu-se apoi in numarul de seminte ramas	
-}
opTurn :: House -> House -> House -> Board -> Board
opTurn c a seminte board
	| c == 1 && seminte < a + 1 = changeSOpp a seminte 5 board
	-- daca ultima saminta pica in depozit jucatorul curent ramane tot Opponent
	| c == 1  && seminte == (a + 1) = let lista = (fst (oppsSeeds board), (snd (oppsSeeds board)) + 1);
													b = changeSOpp a seminte 5 (build (yourSeeds board) lista Opponent)
													in build (yourSeeds b) (oppsSeeds b) Opponent
	-- daca s-a ajuns in partea jucatorui Opponent dar numarul de seminte este mai mare decat ceea ce incape in tabela 
	-- se prelucreaza si se apeleaza recursiv pentru tabla jucatorului You												
	| c == 1 && seminte > ( a + 1 ) = let lista = (fst (oppsSeeds board), (snd (oppsSeeds board)) + 1);
													b = changeSOpp a seminte 5 (build (yourSeeds board) lista Opponent)
													in opTurn 0 6 (seminte - (a + 1)) b						
	-- daca s-a ajuns in partea jucatorului You se prelucrea tabela						
	| c == 0 && seminte <= 6 = let lista = (changeYou seminte 0 (fst (yourSeeds board)) , (snd (yourSeeds board)) )
									in build lista (oppsSeeds board) You	
	-- daca s-a ajuns in partea jucatorului You dar numarul de seminte este mai mare decat lungimea tabelei se prelureaza si 
	-- se apeleaza recursiv pentru parte jucatoruliu Opponent
	| c == 0 && seminte > 6 = let lista = (changeYou seminte 0 (fst (yourSeeds board)) , (snd (yourSeeds board)) );
								 b = build lista (oppsSeeds board) You	
								 in opTurn 1 6 (seminte - 6) b																													
	| otherwise = board

move :: House -> Board -> Board
move a board 
	-- se verifica daca datele primite sunt corecte
	| a > 6 = board
	| a <= 0 = board
	-- daca e randul jucatorului You
	| a < 6 && a > 0 && who board == You  = let seminte = (fst (yourSeeds board)) !! (a - 1);
												lista = (setOnZero (a - 1) (fst (yourSeeds board)) , (snd (yourSeeds board))) 
				 				 				in myTurn 0 (a - 1) seminte (build lista (oppsSeeds board) You) 
	-- daca e randul jucatorului Opponent
	|a < 6 && a > 0 && who board == Opponent = let seminte = (fst (oppsSeeds board)) !! (a - 1);
												   lista = (setOnZero (a - 1) (fst (oppsSeeds board)), snd (oppsSeeds board))
								  				   in opTurn 1 (a - 1) seminte (build (yourSeeds board) lista Opponent)
	| otherwise  = board

{-
    *** TODO ***

    Întoarce scorurile (utilizator, adversar).

    Calculul trebuie să țină cont de eventuala încheiere a jocului.
-}

{-
	Functia returnaeza numarul se seminte ramase in casute daca jocul se termina
-}
total :: [House] -> House
total [] = 0
total x = (head x) + total (tail x)

{-
	Functia returneaza True sau False daca tabela de joc nu mai are seminte pe ea
-}
check :: [House] -> Bool 
check lista = if (length $ filter (\a -> a == 0)  $ lista ) == (length $ lista) then True else False

scores :: Board -> (Int, Int)
scores (Board jName jucator1 jucator2 ) =
	-- daca jocul s-a terminat se aduna la scor semintele ramase
	if (isOver (Board jName jucator1 jucator2)) == True then 	
		-- se verifica ce jucator a terminat jocul
		if (check (fst jucator1)) == True then ((snd jucator1 ), (snd jucator2) + (total (fst jucator2)))
			else ((snd jucator1 ) + (total (fst jucator1)) , (snd jucator2))
				else ((snd jucator1) , (snd jucator2 ))  

{-
    *** TODO ***

    Întoarce perechile casă-configurație, reprezentând mutările care pot fi
    făcute într-un singur pas, pornind de la configurația actuală.
-}

{-
 	Functia construieste o lista cu pozitiile pe care sunt elemente != 0
-}
nextS :: [House] -> House -> [House]
nextS _ 6 = [] 
nextS lista pCurent = if (lista !! pCurent) /= 0 then (pCurent + 1):(nextS lista (pCurent + 1) )
						else nextS lista (pCurent + 1)

{- 
	Functia creaza liste de forma (casa, configuratie)
-}
nextSucc :: [House] -> Board -> [(House,Board)]
nextSucc lista board = let a = nextS lista 0
					   in map (\b -> (b, (move b board))) a

successors :: Board -> [(House, Board)]
successors board = if (who board) == You then nextSucc (fst (yourSeeds board)) board
					  else nextSucc (fst (oppsSeeds board)) board
