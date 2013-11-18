import Data.Graph (Graph, Edge, buildG, components)
import Data.Attoparsec.Char8 (decimal, sepBy, char, space, parseOnly)
import qualified Data.ByteString.Char8 as C8

edge = do
    from <- decimal
    space
    to <- decimal
    return (from, to)

graph = do
    vertCount <- decimal
    char '\n'
    adjList <- edge `sepBy` char '\n'
    return (vertCount, adjList)

parseGraph :: C8.ByteString -> Graph
parseGraph raw = case parseOnly graph raw of
    Left x -> error x
    Right (verts, edges) -> buildG (1, verts) edges

tree = (subtract 1) . length . components . parseGraph
