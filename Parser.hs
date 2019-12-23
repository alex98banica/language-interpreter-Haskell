module Parser
where

import Util
import Data.Maybe
import InferenceDataType
import ClassState

data Class= Class{nume::String, nume_par::String, prog::ClassState}deriving Show

type Program = [Class]
type Instruction = [String]

--verifica daca o clasa deja exista in program
existaClasa :: Program -> Instruction -> Bool
existaClasa [] y = False
existaClasa (x:xs) y = if( (head (tail y))== nume(x) ) then True else existaClasa xs y

--verifica daca parintele exista in program
existaClasa2 :: Program -> Instruction -> Bool
existaClasa2 [] y = False
existaClasa2 (x:xs) y = if ( (head (tail(tail y)))== nume(x))  then True else existaClasa2 xs y

--verifica daca la o functie tipul returnat este in program
existaClasa3 :: Program -> Instruction -> Bool
existaClasa3 [] y = False
existaClasa3 (x:xs) y = if ( (head y)== nume(x))  then True else existaClasa3 xs y

--verifica la o functie daca la o functie clasea in care trebuie inserata exista in program
existaClasa4 :: Program -> Instruction -> Bool
existaClasa4 [] y = False
existaClasa4 (x:xs) y = if ( (head (tail y))== nume(x))  then True else existaClasa4 xs y

--verifica la o functie daca toti parametrii sunt valizi
existaClasa5 :: Program -> Instruction -> Bool
existaClasa5 x [] = True
existaClasa5 x (y:ys) = if((existaClasa3 x [y])==False)  then False else existaClasa5 x ys

initEmptyProgram :: Program
initEmptyProgram =[Class{nume="Global",nume_par="Global",prog=initEmptyClass}]

getVars :: Program -> [[String]] 
getVars [] =[]
getVars (x) =getValues2 (prog (head(x)))

getClasses :: Program -> [String]
getClasses ( [])=[]
getClasses x = nume(head(x)):getClasses( (tail (x)))

getParentClass :: String -> Program -> String
getParentClass y ( []) =""
getParentClass y ( (x:xs))=if (y==nume(x)) then nume_par(x) else getParentClass y (xs)

getFuncsForClass :: String -> Program -> [[String]]
getFuncsForClass y ( [])=[]
getFuncsForClass y ((x:xs))= if (y==nume(x)) then ((getValues (prog x) Func)++getFuncsForClass y ( xs)) else getFuncsForClass y (xs)

functiesplit::[String]->[String]
functiesplit [] =[]
functiesplit (x:xs) =x:functiesplit(xs)

--inlocuieste toate aparitiile semnelor = : ( ) , cu caracterul ' ' (spatiu) pentru a putea parsa
repl::[Char]->[Char]
repl [] = []
repl (x:xs) =
    if x == '=' 
    then ' ' : repl xs 
	else if x== ':'
	then ' ' : repl xs
	else if x == '(' 
    then ' ' : repl xs  
	else if x == ')' 
    then ' ' : repl xs
    else if x == ',' 
    then ' ' : repl xs 	 
    else x : repl xs

separa [] =[]
separa (x:xs) = functiesplit (words x):separa(xs)  

--sterge toate listele goale dupa parsare
remv x=(filter (\k->k/=[]) x)

parse :: String -> [Instruction]
parse x = remv (separa (lines (repl x)))

existaParinte :: Program -> Instruction -> Bool
existaParinte [] y = False
existaParinte (x:xs) y = if( (head (tail(tail(tail y))))== nume(x) ) then True else existaParinte xs y

--insereaza in program variabile 
insertIntoClass2 [] y=[]
insertIntoClass2 (x:xs) y= [Class{nume="Global",nume_par="Global",prog=(insertIntoClass (prog x) Var [head(tail y),head(tail(tail y))] )}] ++xs

--insereaza in program funtiile
insertIntoClass3 [] y =[]
insertIntoClass3 (x:xs) y = if( nume(x) == head (tail y)) 
								then [Class{nume=nume(x),nume_par=nume_par(x),prog=(insertIntoClass (prog x) Func ([head(tail(tail y)),(head y)]++tail(tail(tail y))) )}]++xs 
							else [x]++insertIntoClass3 xs y 

--calculeaza numarul parametrilor dintr-o instructiune 
nrPar []= 0
nrPar (x:xs)=1+nrPar xs

interpret :: Instruction -> Program -> Program
interpret x y =if ((head x) == "class") then 
                       ( if((nrPar x)==4 && ((existaClasa y x)==False) && ((existaParinte y x)==True))  
							then ( y++[Class{nume=(head (tail x)),nume_par=head(tail(tail(tail x))),prog=initEmptyClass}] )  
						else if ((nrPar x)==4 && ((existaClasa y x)==False) && ((existaParinte y x)==False)) 
							then ( y++[Class{nume=(head (tail x)),nume_par="Global",prog=initEmptyClass}] )
						else if ((nrPar x)==2 && ((existaClasa y x)==False)) 
							then (y++[Class{nume=(head (tail x)),nume_par="Global",prog=initEmptyClass}] )
						else y)
						
               else if (((head x) == "newvar") && ((existaClasa2 y x)==True)) 
					then insertIntoClass2 y x	
               else if (((existaClasa3 y x)==True) && ((existaClasa4 y x)==True) && ((existaClasa5 y (tail(tail(tail x))))==True)) 
					then insertIntoClass3 y x 
			   else y
			   
infer :: Expr -> Program -> Maybe String
infer x y = Nothing



