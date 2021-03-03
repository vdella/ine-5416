type Name = String
type Subject = String
type Grades = [Float]

type Student = (Name, Subject, Grades)

studentAt :: Int -> Student
studentAt 0 = ("Jorge", "Matemática Discreta", [6.5, 7.2, 8.5])
studentAt 1 = ("Camila", "Cálculo 4", [7.6, 8.5, 9.4])
studentAt 2 = ("Romeu", "Fundamentos de Biologia Molecular", [0, 2, 3])

nameOf :: Student -> Name
nameOf (studentName, _, _) = studentName

averageOf :: Int -> Float
averageOf studentId = do
    let student = studentAt studentId
        gradesOf (_, _, grades) = grades
        averageOf grades = sum' grades / fromIntegral (length grades)
        in 
        averageOf (gradesOf student)

    where sum' (a:b) = a + sum b
          sum' [] = 0 

main = do
    let firstStudent = studentAt 0
    print firstStudent
    print (nameOf firstStudent)
    print (averageOf 0)
    print (averageOf 2)