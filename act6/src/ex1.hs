type Name = String
type Subject = String
type Grades = [Float]

type Student = (Name, Subject, Grades)

studentAt :: Int -> Student
studentAt 1 = ("Jorge", "Matemática Discreta", [6.5, 7.2, 8.5])
studentAt 2 = ("Camila", "Cálculo 4", [7.6, 8.5, 9.4])
studentAt 3 = ("Romeu", "Fundamentos de Biologia Molecular", [0, 2, 3])

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

-- Exercise 1.E still needs to be finished. Changes made in studentAt :: Int -> Student were done
-- in order to ease the coding process of said exercise.

main = do
    let firstStudent = studentAt 0
    print firstStudent
    print (nameOf firstStudent)
    print (averageOf 0)
    print (averageOf 2)