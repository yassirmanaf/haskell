type Document = String
type Ligne = String
type Mot = String
type NoLigne = Int
type Indexe = [([NoLigne],Mot)]
(>.>) :: ( a -> b) -> ( b -> c) -> (a -> c)
g >.> f = f . g
construireIndex :: Document  -> Indexe
construireIndex = 
			diviserLigne >.>
			numeroterLigne >.>
			diviserMot >.>
			trierMot >.>
			construireListe >.>
			assemblerListe >.>
			filtrerMot
diviserChaine :: [a] -> [a] -> [[a]]					
diviserChaine diviseurs cs = 
			case dropWhile (( flip elem) diviseurs) cs of 
				"" -> []
				cs' -> e : diviserChaine diviseurs cs''
					where (e , cs'') = break ((flip elem) diviseurs) cs'	
					
diviserLigne :: Document -> [Ligne]
diviserLigne = diviserChaine "\n"

numeroterLigne :: [Ligne] -> [(NoLigne,Ligne)]
numeroterLigne Lignes = zip [1..] Lignes

diviserMot :: [(NoLigne, Ligne)] -> [(NoLigne, Mot)]
diviserMot = concat . map numeroterMot
	where
		numeroterMot (Nombre, Ligne) =
			[(Nombre,Mot) | Mot <- diviserChaine " .,:;?!\n\t" Ligne]
			
trierMots :: [(NoLigne , Mot)] -> [(NoLigne , Mot)]
trierMots [] = []
trierMots ( p : ps ) = trierMots petit ++ [p] ++ trierMots grand
	where
		petit = [ r | r <- ps, ordre r p]
		grand = [ r | r<- ps, ordre p r]
		ordre (n1, mot1) (n2,mot2) = 
			mot1 < mot2 || (mot1 == mot2 && n1<n2)
			
