type Name = String
type Subject = String
type Grades = [Float]

type Student = (Name, Subject, Grades)

studentAt :: Int -> Student
studentAt 0 = ("Jorge", "Matemática Discreta", [6.5, 7.2, 8.5])
studentAt 1 = ("Camila", "Cálculo 4", [7.6, 8.5, 9.4])

nameOf :: Student -> Name
nameOf (studentName, _, _) = studentName

averageOf :: Student -> Float
averageOf (_, _, grades) = reduce grades / fromIntegral (length grades)

reduce :: [Float] -> Float
reduce [] = 0
reduce (a:b) = a + reduce b

main = do
    let firstStudent = studentAt 0
    print firstStudent
    print (nameOf firstStudent)
    print (averageOf firstStudent)