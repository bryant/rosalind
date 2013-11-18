import Common (Fasta(..), parseMultiFasta)

gcPercentage xs = 100 * (fromIntegral $ countGC xs) /
                        (fromIntegral $ length xs)
    where countGC = length . filter (\x -> x == 'G' || x == 'C')

gc fasta = case parseMultiFasta fasta of
    Left msg -> error "parse failed"
    Right seqs -> map calcGCs seqs
    where calcGCs (Fasta name seq) = (name, gcPercentage seq)
