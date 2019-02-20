-- Alon Hartanu
-- ID: 305143422

-- Question 1 --
-- Section a --
setElement::Int->a->[a]->[a]
setElement n x []=[]
setElement 0 x (y:ys)=x:ys
setElement n x (y:ys)=y:(setElement (n-1) x ys)

-- Section b --
setElements::[(Int,a)]->[a]->[a]
setElements [] xs=xs
setElements ((n,x):ns) xs=setElements ns (setElement n x xs)

-- Question 2 --
-- Section a --
addWithKey::(String,a)->[(String,a)]->[(String,a)]
addWithKey (key,item) pairs=(key,item):pairs

-- Section b --
subsetByKey::String->[(String,a)]->[a]
subsetByKey key []=[]
subsetByKey subKey ((key,item):pairs)=if subKey==key
                                      then item:(subsetByKey subKey pairs)
                                      else subsetByKey subKey pairs

-- Section c --
subsetByKeys::[String]->[(String,a)]->[a]
subsetByKeys [] pairs=[]
subsetByKeys (key:keys) pairs=(subsetByKey key pairs)++(subsetByKeys keys pairs)

-- Section d --
getKeys::[(String,a)]->[String]
getKeys []=[]
getKeys ((key,item):pairs)=let keys=(getKeys pairs)
                           in
                           if (elem key keys)
                           then keys
                           else (key:keys)

-- Section e --
groupByKeysHelp::[String]->[(String,a)]->[(String,[a])]
groupByKeysHelp [] database=[]
groupByKeysHelp (key:keys) database=addWithKey (key,(subsetByKey key database)) (groupByKeysHelp keys database)

groupByKeys::[(String,a)]->[(String,[a])]
groupByKeys database=groupByKeysHelp (getKeys database) database

-- Question 3 --
-- Section a --
createMatrix::Int->Int->[a]->[[a]]
createMatrix 0 n xs=[]
createMatrix m n xs=(take n xs):(createMatrix (m-1) n (drop n xs))

-- Section b --
getCell::Int->Int->[[a]]->a
getCell m n xs=xs!!m!!n

-- Section c --
appendH::[[a]]->[[a]]->[[a]]
appendH [] []=[]
appendH (row1:mat1) (row2:mat2)=(row1++row2):(appendH mat1 mat2)

-- Section d --
appendV::[[a]]->[[a]]->[[a]]
appendV mat1 mat2=mat1++mat2

-- Section e --
addRows::[Int]->[Int]->[Int]
addRows [] []=[]
addRows (r1:row1) (r2:row2)=(r1+r2):(addRows row1 row2)

addMatrices::[[Int]]->[[Int]]->[[Int]]
addMatrices [] []=[]
addMatrices (row1:mat1) (row2:mat2)=(addRows row1 row2):(addMatrices mat1 mat2)
