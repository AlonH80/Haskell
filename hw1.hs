-- Alon Hartanu
-- ID: 305143422

-- Question 1 --
isPalindrome::String->Bool
isPalindrome "" = True
isPalindrome str = if (length str)==1
                   then True
                   else if last str==head str
                   then isPalindrome (init (tail str))
                   else False

-- Question 2 --
isPrefix::String->String->Bool
isPrefix "" str= True
isPrefix str ""=False
isPrefix (s1:str1) (s2:str2)=if s1==s2
                             then isPrefix str1 str2
                             else False

-- Question 3 --
square::Int->Int
square n=n*n

squareList::Int->[Int]
squareList 0 = [0]
squareList n = (square n):(squareList (n-1))

-- Question 4 --
listSquare::Int->[Int]
listSquare 0 = [0]
listSquare n = (listSquare (n-1))++[square n]

-- Question 5 --
power2Aux::Int->Int->Int
power2Aux 0 acc=acc
power2Aux n acc=power2Aux (n-1) acc*2

power2::Int->Int
power2 n=power2Aux n 1

-- Peter Luhn algorithm --
-- Section a --
toDigits::Integer->[Integer]
toDigits n=if n<=0
           then []
           else (toDigits (n `div` 10))++[n `mod` 10]

-- Section b --
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther []=[]
doubleEveryOther [x]=[x]
doubleEveryOther xs=(doubleEveryOther (init (init xs)))++[2*(last (init xs)),(last xs)]

-- Section c --
sumNumDigits::Integer->Integer
sumNumDigits 0=0
sumNumDigits n= (sumNumDigits (n `div` 10)) + (n `mod` 10)

sumDigits::[Integer]->Integer
sumDigits []=0
sumDigits (x:xs)=(sumNumDigits x)+(sumDigits xs)

-- Section d --
checkSum::Integer->Integer
checkSum n=sumDigits (doubleEveryOther (toDigits n))

-- Section e --
validate::Integer->Bool
validate n=if ((checkSum n) `mod` 10) ==0
           then True
           else False
