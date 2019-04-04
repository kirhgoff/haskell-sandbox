func [True, False, True] = False
func [True, False, False] = False
func [True, True, True] = True
func [True, True, False] = False
func [False, False, False] = False
func [False, False, True] = True
func [False, True, False] = False
func [False, True, True] = True

combinations = [ [x1,x2,x3] | x1 <- [True,False], x2 <- [True,False], x3 <- [True,False] ]

func2 [x, y, z] = (x && y && z) || (not x && not y && z) || (not x && y && z)
--func3 [x, y, z] = z && ((x && y) || (not x && not y) || (not x && y))
--func3 [x, y, z] = z && ((x && y) || (not x))
-- func3 [x, y, z] = z && (y || not x)
func3 [x, y, z] = (not x || y) && z

match = (map func combinations) == (map func3 combinations)
