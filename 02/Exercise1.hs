-- 2.1
suffixes :: [a] -> [[a]]
suffixes [] = [[]]
suffixes xs = xs : (suffixes (Prelude.tail xs))
