module Main where

-- Note that, in both dirtying and propagation, we only traverse an edge if it is clean or dirty, respectively.
-- We can do so because the above procedures maintain an invariant that:
--
-- At the end of a dirtying or propagation phase:
--
-- If an edge is dirty
--   then all edges transitively reachable by traversing incoming edges beginning from the source node will also be dirty;
-- If an edge is clean:
--   then all edges transitively reachable by traversing outgoing edges beginning from the target node will also be clean.

