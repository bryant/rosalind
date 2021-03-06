module Common (Fasta(..), parseMultiFasta) where

import Text.Parsec (Parsec, parse, many, many1, noneOf, char, newline)
import Control.Applicative ((<$>), (<*), (<*>), (*>))

data Fasta = Fasta String String deriving Show

multiFasta :: Parsec String u [Fasta]
multiFasta = many1 fastaUnit

fastaUnit = Fasta <$> headerLine <*> seqLines

seqLines = concat <$> many1 seqLine

headerLine = char '>' *> many (noneOf "\n") <* newline

seqLine = (:) <$> noneOf ">" <*> many (noneOf "\n") <* newline

parseMultiFasta = parse multiFasta "multifasta"
