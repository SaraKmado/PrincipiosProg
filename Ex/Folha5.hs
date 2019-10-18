--1
---a duplica um numero
---bdevolve true se um num for negativo
---c devolve o inverso de um numero
---d divide um num em 2
---e soma 1 a um num
---f adiciona um paragrafo a uma String

--2
----a funçao a e igual a b mas a a recebe um tuplo e a b recebe 2 nums
----a funçao de c retorna uma funçao soma

--3 
---a [2,3,4]
---b [True,False,False,False]
---c ["As","artes","dos","alunos"]
---d ["so","saluno","sbem-comportado"]
---e [[1,4],[9,16,25]]
---f [6]
---g [2,4,6,8,10]
---h [9,4,1,1,4,9]
---i [1,4,9]

--4
zipWith1 :: (a->b->c)-> [a] -> [b] -> [c]
zipWith1 f xs [] = []
zipWith1 f [] ys = []
zipWith1 f (x:xs) (y:ys) = f x y : zipWith1 f xs ys

zipWith2 :: (a->b->c)-> [a] -> [b] -> [c]
zipWith2 f xs ys = [f (xs !! x) (ys !! y) | x <- [0..length xs - 1],y <- [0..length ys - 1], x == y]

zip' :: [a] -> [b] -> [(a,b)]
zip' xs ys= zipWith1 f xs ys
  where f a b = (a,b)

--5
