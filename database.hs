type Student = (String, Int, [Int])
type DB = [Student]

-- Database 
incons1 :: DB
incons1 = [("Jack", 111, [141, 252, 141])]
incons2 :: DB
incons2 = [("Jane", 112, [141, 252]), ("Jane", 112, [141, 252])]
incons3 :: DB
incons3 = [("Jane", 111, [141]), ("Jack", 111, [141, 252])]
cons1 :: DB
cons1 = [("Jack", 111, [141, 252]), ("Jacko", 113, [141, 253])]
db :: DB
db = [("Jane", 111, [141, 252]), ("Jack", 112, [141, 252]), ("Jacko", 113, [141, 253, 42]), ("Qbert", 123, [23, 253, 42]), ("Albert", 124, [23, 42])]

-- Helper for Student
nameof :: Student -> String
nameof (x,_,_) = x

idof :: Student -> Int
idof (_,x,_) = x

coursesof :: Student -> [Int]
coursesof (_,_,x) = x

-- Task 0
duplicate [] = False
duplicate (x:xs)
       | null xs     = False
       | x `elem` xs = True
       | otherwise   = duplicate xs

duplicateInLists :: [[Int]] -> Bool
duplicateInLists (x:xs)
    | null xs             = False
    | duplicate x == True = True
    | otherwise           = duplicateInLists xs

validStudentIds :: DB -> Bool
validStudentIds db = not (duplicate [ id | (_, id, _) <- db])

validStudentCourse :: DB -> Bool
validStudentCourse db = not (duplicateInLists [ csId | (_, _, csId) <- db])

valid :: DB -> Bool
valid db = True

-- Task 1
-- Given a database and a student id, we're looking for the list of courses of this particular student
query1 :: DB -> Int -> [Int]
query1 db student = [ x | (_,i,cs) <- db, x <- cs, i == student]

-- Task 2
-- Given a database and a course, find all students taking this course.
query2 :: DB -> Int -> [String]
query2 db course = [ x | (x,_,cs) <- db, course `elem` cs]

-- Task 3
-- Given a database, sort the database (in non-decreasing order) according to the name of students.
sortDB :: DB -> DB
sortDB [] = []
sortDB (s:students) = (sortDB lhs) ++ [s] ++ (sortDB rhs)
	where
		cmp c x = length (coursesof s) `c` length (coursesof x)
		lhs = [ x | x <- students, cmp (>) x]
		rhs = [ x | x <- students, cmp (<=) x]

-- Extension1: Provide a function sortDB' which sorts the database according to the number of courses a student is taking
sortDB' :: DB -> DB
sortDB' [] = []
sortDB' (s:students) = (sortDB' lhs) ++ [s] ++ (sortDB' rhs)
	where
		cmp c x = length (coursesof s) `c` length (coursesof x)
		lhs = [ x | x <- students, cmp (>) x]
		rhs = [ x | x <- students, cmp (<=) x]

-- Extension2: Provide a function sortDB'' which is parameterized in the actual comparing relation which determines the sorting order For example: Given
-- todo
sortDB'' :: DB -> DB
sortDB'' _ = []

-- Task 4
-- Given two databases, merge them to obtain one (consistent) database
mergeItem :: DB -> Student -> DB
mergeItem db newStudent
	| null student = db ++ [newStudent]
	| id == idof newStudent = (filter (\n -> id /= (idof n)) db) ++ [(name, id, cs ++ [ x | x <- coursesof newStudent, not (x `elem` cs)])]
	| otherwise = []
	where 
		student = [ (name,id,cs) | (name,id,cs) <- db,  name `elem` [name | (name,_,_) <- [newStudent]]]
		name = nameof (head student)
		id = idof (head student)
		cs = coursesof (head student)

merge :: DB -> DB -> Maybe DB
merge db [] = Just db
merge db (x:xs) 
	| null newDb = Nothing
	| otherwise = merge newDb (xs)
	where newDb = mergeItem db x


-- Test ...
mergeDb = [("Jane", 112, [141, 353])] 
mergeCons = [("Jane", 112, [141, 252])]
mergeCons2 = [("Jack", 114, [141, 252])]
mergeIncons = [("Jane", 113, [141, 252])]

m1 = merge mergeDb mergeCons
m2 = merge mergeDb mergeCons2
m3 = merge mergeDb mergeIncons