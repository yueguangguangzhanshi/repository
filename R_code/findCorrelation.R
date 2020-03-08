x = cor(TraitsSample_all, TraitsSample_all, use = "p")
findCorrelation(x, cutoff = 0.9, verbose = FALSE, names = TRUE,
                exact = ncol(x) < 100)
