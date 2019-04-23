import Data.Char
--------------------------------------------win functions -------------------------------------
-- Check if he  win horizintal 
checkwinHorzintal ::[[Int]]->Int
checkwinHorzintal xss
 |(checkwinHorzintalhelper xss 0 1 0)== 1 = 1
 |(checkwinHorzintalhelper xss 0 2 0)== 2 = 2
 |otherwise =0
 --- Checking if there are three x or o in horizontal
checkwinHorzintalhelper :: [[Int]]->Int->Int->Int->Int
checkwinHorzintalhelper xss col play count 
 |count==3 = play
 |col==3 = 0
 |xss!!count!!col==play =checkwinHorzintalhelper xss col play (count+1) 
 |otherwise = checkwinHorzintalhelper xss (col+1) play 0 
--------------- check if he win vertical --------------------
checkVertical::[[Int]]->Int
checkVertical xxs = checker (spilt xxs) (length (spilt xxs) -1)

checker::[Int]->Int->Int
checker xxs n  |((xxs!!n)==1)&&((xxs!!(n-1))==1)&&((xxs!!(n-2))==1) =1
               |((xxs!!n)==2)&&((xxs!!(n-1))==2)&&((xxs!!(n-2))==2) =2
               |otherwise =0
-------------- check diagonal if he win with left ----------------------
helpercheckdigonalLift::[[Int]]->Int->Int->Int->Int->Int
helpercheckdigonalLift xxs row col count1 count2
 |count1==3=1
 |count2==3=2
 |row==(length (xxs!!0))=0
 |xxs!!row!!col==0=0
 |xxs!!row!!col==1=helpercheckdigonalLift xxs (row+1) (col+1) (count1+1) count2
 |xxs!!row!!col==2=helpercheckdigonalLift xxs (row+1) (col+1) count1 (count2+1)

checkdigonalLift::[[Int]]->Int
checkdigonalLift xxs=helpercheckdigonalLift xxs 0 0 0 0
------------ check if he win with diagonal right ----------------------
helpercheckdigonalRight::[[Int]]->Int->Int->Int->Int->Int
helpercheckdigonalRight xxs row col count1 count2
 |count1==3=1
 |count2==3=2
 |row==(-1)=0
 |xxs!!row!!col==0=0
 |xxs!!row!!col==1=helpercheckdigonalRight xxs (row-1) (col+1) (count1+1) count2
 |xxs!!row!!col==2=helpercheckdigonalRight xxs (row-1) (col+1) count1 (count2+1)
checkdigonalRight::[[Int]]->Int
checkdigonalRight xxs=helpercheckdigonalRight xxs ((length (xxs!!0))-1)  0 0 0


spilt ::[[Int]]->[Int]
spilt xxs = [ x | xs<- xxs , x<- xs  ]

win ::[[Int]]->Int 
win xss
 |((checkwinHorzintal xss)==1 || (checkdigonalLift xss)==1 || (checkdigonalRight xss)==1 || (checkVertical xss) ==1)=1
 |((checkwinHorzintal xss)==2 || (checkdigonalLift xss)==2 || (checkdigonalRight xss)==2 ||(checkVertical xss) ==2)=2
 |otherwise=0
 ----------------------------------------play functions----------------------------------------------------------------
 ------------------- Allow Me to play in the grid and putting the value ---------------------------------------------------
playing ::  [[Int]]->(Int,Int)->Int->[[Int]]
playing xxs (x,y) play=editLofL xxs x 0 (editoneL (xxs!!x) y 0 play)

editoneL ::[Int]-> Int-> Int-> Int-> [Int]
editoneL (x:xs) into big play
 |big==into=play:xs
 |otherwise=x:editoneL xs into (big+1) play

editLofL :: [[Int]] -> Int-> Int-> [Int]-> [[Int]]
editLofL (xs:xxs) into big xplay
 |big==into=xplay:xxs
 |otherwise=xs:editLofL xxs into (big+1) xplay
 ----------------------------------------check zero-------------------------------------------------------------------
checkZero::[[Int]]->Bool
checkZero xxs = checkerZero (spilt xxs) (length (spilt xxs) -1)

checkerZero::[Int]->Int->Bool
checkerZero xxs (-1) =False
checkerZero xxs n |((xxs!!n)==0) =True
                  |otherwise =checkerZero xxs (n-1)
------------------------------------------calc weight------------------------------------------------------------------
------------------------------------------costdigoleft---------------------------------------------------------------
checkweightdiognalleftuphelper:: [[Int]]->(Int,Int)->Int->Int->(Int,Int)
checkweightdiognalleftuphelper xss (row,col) count checker
 |(col==(-1)|| row==(3)) = (count,checker)
 |(xss!!row!!col)==0=checkweightdiognalleftuphelper xss ((row+1),(col-1)) count checker
 |(((xss!!row!!col)==checker)&&(checker/=0)) = checkweightdiognalleftuphelper xss ((row+1),(col-1)) (count+1) checker
 |(((xss!!row!!col)/=0 || (xss!!row!!col)/=checker)&&(checker/=0))= checkweightdiognalleftuphelper xss ((row+1),(col-1)) (count-1) (xss!!row!!col)
 |otherwise = checkweightdiognalleftuphelper xss ((row+1),(col-1)) (count+1) (xss!!row!!col)

checkweightdiognalleftup:: [[Int]]->(Int,Int)->(Int,Int)
checkweightdiognalleftup xss (row,col) = checkweightdiognalleftuphelper xss (row,col) 0 0 

checkweightdiognalleftdownhelper:: [[Int]]->(Int,Int)->Int->Int->(Int,Int)
checkweightdiognalleftdownhelper xss (row,col) count checker
 |(col==(3)|| row==(-1)) = (count,checker)
 |(xss!!row!!col)==0=checkweightdiognalleftdownhelper xss ((row-1),(col+1)) count checker
 |(((xss!!row!!col)==checker)&&(checker/=0)) = checkweightdiognalleftdownhelper xss ((row-1),(col+1)) (count+1) checker 
 |(((xss!!row!!col)/=0 || (xss!!row!!col)/=checker)&&(checker/=0))= checkweightdiognalleftdownhelper xss ((row-1),(col+1)) (count-1) (xss!!row!!col) 
 |otherwise = checkweightdiognalleftdownhelper xss ((row-1),(col+1)) (count+1) (xss!!row!!col)
checkweightdiognalleftdown:: [[Int]]->(Int,Int)->(Int,Int)
checkweightdiognalleftdown xss (row,col) = checkweightdiognalleftdownhelper xss (row,col) 0 0
compareweight :: (Int,Int)->(Int,Int)->Int
compareweight (costup,checkerup) (costdown,checkerdown)
 |(checkerup==0)||(checkerdown==0)= costup + costdown
 |checkerup==checkerdown = costup + costdown
 |checkerup/=checkerdown = costup - costdown

checkweightdiognallefthelper::[[Int]]->(Int,Int)->Int
checkweightdiognallefthelper xss (row,col) = compareweight (checkweightdiognalleftup xss (row,col)) (checkweightdiognalleftdown xss (row,col))

checkweightdiognalleft::[[Int]]->(Int,Int)->(Int,(Int,Int))
checkweightdiognalleft xss (row,col) = ((checkweightdiognallefthelper xss (row,col)),(row,col))

------------------------------------------costdigoright---------------------------------------------------------------
checkweightdiognalrightuphelper:: [[Int]]->(Int,Int)->Int->Int->(Int,Int)
checkweightdiognalrightuphelper xss (row,col) count checker
 |(col==(3)|| row==(3)) = (count,checker)
 |(xss!!row!!col)==0=checkweightdiognalrightuphelper xss ((row+1),(col+1)) count checker
 |(((xss!!row!!col)==checker)&&(checker/=0)) = checkweightdiognalrightuphelper xss ((row+1),(col+1)) (count+1) checker
 |(((xss!!row!!col)/=0 || (xss!!row!!col)/=checker)&&(checker/=0))= checkweightdiognalrightuphelper xss ((row+1),(col+1)) (count-1) (xss!!row!!col)
 |otherwise = checkweightdiognalrightuphelper xss ((row+1),(col+1)) (count+1) (xss!!row!!col)
checkweightdiognalrightup:: [[Int]]->(Int,Int)->(Int,Int)
checkweightdiognalrightup xss (row,col) = checkweightdiognalrightuphelper xss (row,col) 0 0 

checkweightdiognalrightdownhelper:: [[Int]]->(Int,Int)->Int->Int->(Int,Int)
checkweightdiognalrightdownhelper xss (row,col) count checker
 |(col==(-1)|| row==(-1)) = (count,checker)
 |(xss!!row!!col)==0=checkweightdiognalrightdownhelper xss ((row-1),(col-1)) count checker
 |(((xss!!row!!col)==checker)&&(checker/=0)) = checkweightdiognalrightdownhelper xss ((row-1),(col-1)) (count+1) checker 
 |(((xss!!row!!col)/=0 || (xss!!row!!col)/=checker)&&(checker/=0))= checkweightdiognalrightdownhelper xss ((row-1),(col-1)) (count-1) (xss!!row!!col) 
 |otherwise = checkweightdiognalrightdownhelper xss ((row-1),(col-1)) (count+1) (xss!!row!!col)
checkweightdiognalrightdown:: [[Int]]->(Int,Int)->(Int,Int)
checkweightdiognalrightdown xss (row,col) = checkweightdiognalrightdownhelper xss (row,col) 0 0

checkweightdiognalrighthelper::[[Int]]->(Int,Int)->Int
checkweightdiognalrighthelper xss (row,col) = compareweight (checkweightdiognalrightup xss (row,col)) (checkweightdiognalrightdown xss (row,col))

checkweightdiognalright::[[Int]]->(Int,Int)->(Int,(Int,Int))
checkweightdiognalright xss (row,col) = ((checkweightdiognalrighthelper xss (row,col)),(row,col))

-------------------------------------------------costHorzintal---------------------------------------------------------
checkweighthorzentaluphelper:: [[Int]]->(Int,Int)->Int->Int->(Int,Int)
checkweighthorzentaluphelper xss (row,col) count checker
 |col==3 = (count,checker)
 |(xss!!row!!col)==0=checkweighthorzentaluphelper xss ((row),(col+1)) count checker
 |(((xss!!row!!col)==checker)&&(checker/=0)) = checkweighthorzentaluphelper xss ((row),(col+1)) (count+1) checker
 |(((xss!!row!!col)/=0 || (xss!!row!!col)/=checker)&&(checker/=0))= checkweighthorzentaluphelper xss ((row),(col+1)) (count-1) (xss!!row!!col)
 |otherwise = checkweighthorzentaluphelper xss ((row),(col+1)) (count+1) (xss!!row!!col)

checkweighthorzentalup ::[[Int]]->(Int,Int)->(Int,Int)
checkweighthorzentalup xss (row,col) = checkweighthorzentaluphelper xss (row,col) 0 0

checkweighthorzentaldownhelper:: [[Int]]->(Int,Int)->Int->Int->(Int,Int)
checkweighthorzentaldownhelper xss (row,col) count checker
 |col==(-1) = (count,checker)
 |(xss!!row!!col)==0=checkweighthorzentaldownhelper xss ((row),(col-1)) count checker
 |(((xss!!row!!col)==checker)&&(checker/=0)) = checkweighthorzentaldownhelper xss ((row),(col-1)) (count+1) checker
 |(((xss!!row!!col)/=0 || (xss!!row!!col)/=checker)&&(checker/=0))= checkweighthorzentaldownhelper xss ((row),(col-1)) (count-1) (xss!!row!!col)
 |otherwise = checkweighthorzentaldownhelper xss ((row),(col-1)) (count+1) (xss!!row!!col)

checkweighthorzentaldown ::[[Int]]->(Int,Int)->(Int,Int)
checkweighthorzentaldown xss (row,col) = checkweighthorzentaldownhelper xss (row,col) 0 0

checkweighthorzintalhelper::[[Int]]->(Int,Int)->Int
checkweighthorzintalhelper xss (row,col) = compareweight (checkweighthorzentalup xss (row,col)) (checkweighthorzentaldown xss (row,col))

checkweighthorzintal::[[Int]]->(Int,Int)->(Int,(Int,Int))
checkweighthorzintal xss (row,col) = ((checkweighthorzintalhelper xss (row,col)),(row,col))

-----------------------------------------------------costvertix-----------------------------------------------------------
getLeftSide::[[Int]]->(Int,Int)->Int->Int
getLeftSide xxs (x,y) e
 |e==0=0
 |x==0=0
 |(xxs!!(x-1)!!y)==e=1+getLeftSide xxs ((x-1),y) e
 |otherwise=0
getRightSide::[[Int]]->(Int,Int)->Int->Int
getRightSide xxs (x,y) 
 |e==0=0
 |x==2=0
 |(xxs!!(x+1)!!y)==e=1+getLeftSide xxs ((x+1),y) e
 |otherwise=0
virtexwight::[[Int]]->(Int,Int)->(Int,(Int,Int))
virtexwight xxs (x,y)
 |x==2=(getLeftSide xxs (x,y) (xxs!!(x-1)!!y),(x,y))
 |x==0=(getRightSide xxs (x,y) (xxs!!(x+1)!!y),(x,y))
 |otherwise = ((getRightSide xxs (x,y) (xxs!!(x+1)!!y))+(getLeftSide xxs (x,y) (xxs!!(x+1)!!y)),(x,y))

---------------------------------------------------findzerotuples----------------------------------------------------------

findzero::[[Int]]->[(Int,Int)]
findzero xxs = finderzero xxs 0 0 []

finderzero::[[Int]]-> Int -> Int-> [(Int,Int)] ->[(Int,Int)]
finderzero xxs 2 2 xs |((xxs!!2!!2)==0) =(2,2):xs
                      |otherwise        =xs

finderzero xxs row column  xs |(column==2)&&((xxs!!row!!column)==0) =finderzero xxs (row+1) (0) ((row,column):xs)
                              |(column==2)       =finderzero xxs (row+1) (0) (xs)
                              |((xxs!!row!!column)==0) =finderzero xxs (row) (column+1) ((row,column):xs)
                              |otherwise    =finderzero xxs (row) (column+1) xs
------------------------------------------------------valditions and translate---------------------------------------------

validations::String->Bool
validations inp  
 |inp=="1-1"||inp=="1-2"||inp=="1-3"||inp=="2-1"||inp=="2-2"||inp=="2-3"||inp=="3-1"||inp=="3-2"||inp=="3-3"=False
 |otherwise=True
translate::String->(Int,Int)
translate inp =(((ord(inp!!2))- 49),((ord(inp!!0))- 49))
----------------------------------------------------highest cost----------------------------------------------------------

comparseTuple ::(Int,(Int,Int))->(Int,(Int,Int))->(Int,(Int,Int))->(Int,(Int,Int))->(Int,(Int,Int))
comparseTuple (x1,(y1,z1)) (x2,(y2,z2)) (x3,(y3,z3)) (x4,(y4,z4)) |(x1>x2)&&(x1>x3)&&(x1>x4) =(x1,(y1,z1))
                                                                  |(x2>x3)&&(x2>x4)          =(x2,(y2,z2))
                                                                  |(x3>x4)                   =(x3,(y3,z3))
                                                                  |otherwise                 =(x4,(y4,z4))

compareList::[(Int,(Int,Int))]->[(Int,(Int,Int))]->[(Int,(Int,Int))]->[(Int,(Int,Int))]->[(Int,(Int,Int))]
compareList (x:xs) (y:ys) (z:zs) (t:ts) 
 |xs==[]=[(comparseTuple x y z t)]
 |otherwise=(comparseTuple x y z t):compareList xs ys zs ts
------------------------------------------------canplay---------------------------------------------------------------
checkCanPlay::[[Int]]->(Int,Int)->Bool
checkCanPlay xxs (x,y)
 |xxs!!x!!y==0=True
 |otherwise=False
 -----------------------------------------------complayer--------------------------------------------------------------
virtexAI::[[Int]]->[(Int,Int)]->[(Int,(Int,Int))]
virtexAI xxs (x:xs)
 |xs==[]=[(virtexwight xxs x)]
 |otherwise=(virtexwight xxs x):virtexAI xxs xs

horzintalAI::[[Int]]->[(Int,Int)]->[(Int,(Int,Int))]
horzintalAI xxs (x:xs)
 |xs==[]=[(checkweighthorzintal xxs x)]
 |otherwise=(checkweighthorzintal xxs x):horzintalAI xxs xs
diognalrightAI::[[Int]]->[(Int,Int)]->[(Int,(Int,Int))]
diognalrightAI xxs (x:xs)
 |xs==[]=[(checkweightdiognalright xxs x)]
 |otherwise=(checkweightdiognalright xxs x):diognalrightAI xxs xs

diognalleftAI::[[Int]]->[(Int,Int)]->[(Int,(Int,Int))]
diognalleftAI xxs (x:xs)
 |xs==[]=[(checkweightdiognalleft xxs x)]
 |otherwise=(checkweightdiognalleft xxs x):diognalleftAI xxs xs
getListOfMaxCost::[[Int]]->[(Int,(Int,Int))]
getListOfMaxCost xxs =compareList (diognalleftAI xxs (findzero xxs)) (diognalrightAI xxs (findzero xxs)) (horzintalAI xxs (findzero xxs)) (virtexAI xxs (findzero xxs))
getMaxCost ::[(Int,(Int,Int))] ->Int->(Int,Int)->(Int,Int)
getMaxCost ((x,y):xxs) max m
 |xxs==[]&&max<x=y
 |xxs==[]=m
 |max<x=getMaxCost xxs x y
 |otherwise=getMaxCost xxs max m
playcomputer ::[[Int]]->[[Int]]
playcomputer xxs=playing xxs (getMaxCost (getListOfMaxCost xxs) 0 (0,0)) 2
------------------------------------------------------showlist-----------------------------------------------------------
content :: [Int] -> [String]
content (x:xs) |length(xs)==0&&x==0 = [" "]
               |length(xs)==0&&x==1 = ["O"]
               |length(xs)==0&&x==2 = ["X"]
               |x==0 = [" "] ++ content xs 
               |x==1 = ["O"] ++ content xs 
               |x==2 = ["X"] ++ content xs 
   

help :: Int -> String
help n | n==1 = "****"
       | otherwise = "****" ++ help (n-1)              

helpRow :: [String] -> String
helpRow (x:xs) | length(xs)==0=" "++x
               | otherwise = (" "++x++" |")++helpRow(xs)

--row :: [Int] -> Int -> IO()
--row xs count |count==0= do putStrLn("*" ++ help (length(xs)))
--                            row xs (count+1)
--             |otherwise= do putStrLn ("|"++helpRow(content(xs))++" |"++"\n"++"*" ++ help (length(xs))) 
rows :: [Int] -> Int -> IO()
rows xs count |count==0= do putStrLn("*" ++ help (length(xs)))
                            rows (xs) (count+1)
              |otherwise= do putStrLn ("|"++helpRow(content(xs))++" |") 
                             putStrLn("*" ++ help (length(xs)))

--ticTacTio :: [[Int]] -> Int -> IO()
--ticTacTio xs = ticTacio (xs) (0)							 

ticTacio :: [[Int]] ->Int -> IO()
ticTacio (x:xs) count |count==0 =do rows x 0 
                                    ticTacio (xs) (2)
                     |length(xs)==0 =rows x 2
                     |otherwise = do rows x 2
                                     ticTacio xs 2
-------------------------------------------------------------main----------------------------------------------------------
start::Int->IO()
start check
 |check==1 =startOne [[0,0,0],[0,0,0],[0,0,0]]
 |check==2 =playuserone [[0,0,0],[0,0,0],[0,0,0]]
 |otherwise = putStrLn "enter valied number!!!"
startOne:: [[Int]]->IO()
startOne xxs=do ticTacio xxs 0
                if(checkZero xxs==False)
                  then do putStrLn "game over!!!"
                  else   do putStrLn "please Enter colomn"
                            input <- getLine
                            if(validations input)
                                then do putStrLn "please enter valid number !!!"
                                        startOne xxs
                                else if(checkCanPlay xxs (translate input)/=True)
                                        then do putStrLn "you can't play here"
                                                startOne xxs
                                        else do afteruser (playing xxs (translate input) 1)
afteruser::[[Int]]->IO()
afteruser xxs=  if((win xxs)==1)
                       then do ticTacio xxs 0
                               putStrLn "you win"
                       else if(checkZero xxs==False)
                              then putStrLn "game over!!!" 
                              else aftercomp (playcomputer xxs)
aftercomp::[[Int]]->IO()
aftercomp xxs= if((win xxs)==2)
                    then do ticTacio xxs 0
                            putStrLn "computer win"
                    else result xxs
result::[[Int]]->IO()
result xxs= do startOne xxs

playuserone::[[Int]]->IO()
playuserone xxs=do ticTacio xxs 0
                   if(checkZero xxs==False)
                        then do putStrLn "game over!!!"
                        else   do putStrLn "please Enter colomn user 1"
                                  input <- getLine
                                  if(validations input)
                                		then do putStrLn "please enter valid number !!!"
                                        		playuserone xxs
                                		else if(checkCanPlay xxs (translate input)/=True)
                                        		then do putStrLn "you can't play here"
                                            			playuserone xxs
                                        	else do afteruserone (playing xxs (translate input) 1)
afteruserone::[[Int]]->IO()
afteruserone xxs=  if((win xxs)==1)
                       then do ticTacio xxs 0
                               putStrLn "user 1 win"
                       else if(checkZero xxs==False)
                              then putStrLn "game over!!!" 
                              else playusertwo xxs
afterusertwo::[[Int]]->IO()
afterusertwo xxs= if((win xxs)==2)
                    then do ticTacio xxs 0
                            putStrLn "user 2 win"
                    else showPlaytwo xxs
playusertwo::[[Int]]->IO()
playusertwo xxs=do ticTacio xxs 0
                   if(checkZero xxs==False)
                        then do putStrLn "game over!!!"
                        else   do putStrLn "please Enter colomn user 2"
                                  input <- getLine
                                  if(validations input)
                                		then do putStrLn "please enter valid number !!!"
                                        		playusertwo xxs
                                		else if(checkCanPlay xxs (translate input)/=True)
                                        		then do putStrLn "you can't play here"
                                            			playusertwo xxs
                                        	else do afterusertwo (playing xxs (translate input) 2)
showPlaytwo::[[Int]]->IO()
showPlaytwo xxs= do playuserone xxs




