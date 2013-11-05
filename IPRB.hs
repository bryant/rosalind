iprb popAA popAa popaa = 1 - homozygrec
    where
    homozygrec = popaa/n * (popaa-1)/(n-1) +
                 0.5 * 2 * popaa/n * popAa/(n-1) +
                 0.25 * popAa/n * (popAa-1)/(n-1)
    n = popAA + popAa + popaa
