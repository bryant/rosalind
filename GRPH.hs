import System.Environment (getArgs)
import Data.List (intercalate)
import Common (Fasta(..), parseMultiFasta)

edgeFromTo s t k = lastN k s == take k t
    where lastN n = reverse . take n . reverse

overlap xs k = [snam ++ " " ++ tnam | Fasta snam s <- xs, Fasta tnam t <- xs,
                tnam /= snam, edgeFromTo s t k]

grph raw k = intercalate "\n" $ overlap fastas k
    where Right fastas = parseMultiFasta raw
