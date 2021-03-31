alunos :: [(Int, String, Float)]
alunos = [(1, "Ana", 3.4), (2, "Bob", 6.7), (3, "Tom", 7.6)]

getNome :: (Int, String, Float) -> String
getNome (a,b,c) = b

getMedia :: (Int, String, Float) -> Float
getMedia (a, b, c) = c

getPrimeiroAluno :: [(Int, String, Float)] -> (Int, String, Float)
getPrimeiroAluno (a:_) = a

gerarPares :: [t] -> [u] -> [(t,u)]
gerarPares l1 l2 = [(a,b) | a <- l1, b <- l2]

aprovados :: [(Int, String, Float)] -> [String]
aprovados alunosNaTurma = do
    let aprovados = filter (\a -> getMedia a >= 6) alunosNaTurma
    map getNome aprovados

aprovados2 :: [(Int, String, Float)] -> [String]
aprovados2 alunosNaTurma = [getNome a | a <- alunosNaTurma, getMedia a >= 6]

main = do
    print (getPrimeiroAluno alunos)
    print (aprovados alunos)
    print (aprovados2 alunos)