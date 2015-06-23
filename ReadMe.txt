	<Tema 2 - Kalah >

	Informatiile despre starea jocului sunt retinute intr-un constructor de forma:
Player (Tabela,You-scor) (Tabela ,Opponent-scor)
	Functiile yourSeeds, oppsSeeds, initialBoard si who sunt construite astfel
incat sa respecte indicatiile.
	Functia isOver verifica pe rand lista jucatorului curent si lista oponentului
daca are numai 0 si intoarce True sau False.
	Functia move verifica daca datele sunt primite corect si apeleaza functiile 
pentru mutarea seminteleor in functie de jucatorul curent.
-> myTurn - se apeleaza pentru jucatorul You. In functie de numarul de seminte care se
	    afla initial pe casuta din care se muta se modifica cele 2 liste din 
	    tabela. 
	    c = 0 => se modifica lista jucatorului curent
	    c = 1 => se modifica lista oponentului
	  - daca numarul de seminte se incadreaza in lungimea primei liste se apeleaza
	    functia changeSYou
      - daca numarul de seminte se incadreaza in lungimea primei liste + 1 
	    (cea ce se pune in depozit) se modifica lista , se actualizeaza scorul
		si se modica jucatorul curent ca fiind tot You
	  - daca numarul de seminte este mai mare decat lunginea listei se modifica
		acesta si se apleaza recursiv pentru a se modifica lista oponentului
		cu numarul de seminte ramas ( apel cu c = 1 ) 
-> changeSYou - modifica lista jucatorului You prin adugarea semintelor din 
		pozitia data
	      - verifica la ultima samanta daca casa pe care urmeaza sa puna este 
		goala si in caz afirmativ verifica si in lista oponentului daca
	  	casa lui nu este goala pentru a pune in depozit cele 2 cantitati
	  	de seminte.
-> changeOp - modifica lista oponentului de la ultima valoare, cat timp mai are
              seminte de pus.
-> opTurn - se apeleaza pentru jucatorul Opponent se are acelesi mod de functionare
	    ca si fucntia myTurn, In functie de numarul de seminte se modifica cele
	    2 liste.
-> changeSOpp - modifica lista jucatorului Opponent prin adugarea de seminte in
		sens invers ( de la dreapta la stanga) si vefica casa ultimei
		seminte puse.			  
-> changeYou - modifica lista jucatorului You prin adugarea de seminte , de la
	       pozitia 0 (listele sunt numerotate de la 0) atata timp cat numarul lor 
	       este diferit de 0.

	Functia scores verifica daca jocul este terminat si in caz afirmativ aduna la
scor cea ce a ramas pe tabela.
	Functia successors creaza liste de forma (casa, configuratie) prin apelarea
functie move pentru fiecare casa in care mai sunt seminte.
	Functia step returneaza tabla ceruta prin crearea unei lista de perechi de 
forma (casa, scor oponent) , aflarea maximului din aceasta lista si cautarea 
si returnarea valorilor corespunzatoare date de functia successors.
	Functia step retine datele returnate de functia successors, creaza o lista de 
perechi de forma (House, scor oponent) , afla maximul din aceasta lista si 
returneaza valoarea corespunzatoare.
