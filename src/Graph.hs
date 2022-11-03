module Graph where

import qualified Data.Array as A

--Each space in Array contains some data and the neighboring vertices
type Graph i e = A.Array i (e, [i])

neighborVertices :: Graph i e -> i -> [i]
neighborVertices = undefined

--A list of edges and data to store on each node
fromList :: [(i, i, e)]
fromList = undefined

--The graph, start, end, a func to accumulate result for each node
dfs :: Graph i e -> i -> ((i, e) -> Bool) -> ((i, e) -> x) -> x
dfs g s isDone accF = undefined
