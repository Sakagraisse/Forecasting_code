rm(list = ls())
##Not run
    data(Wilshire)
    #Rebuilding model1 from ecm example
    trn <- Wilshire[Wilshire$date<='2015-12-01',]
    xeq <- xtr <- trn[c('CorpProfits', 'FedFundsRate', 'UnempRate')]
    model1 <- ecm(trn$Wilshire5000, xeq, xtr)
    model2 <- ecm(trn$Wilshire5000, xeq, xtr, linearFitter='earth')
    #Use 2015-12-01 and onwards data as test data to predict
    tst <- Wilshire[Wilshire$date>='2015-12-01',]
    #predict on tst using model1 and initial FedFundsRate
    tst <- ecmpredict(model1, tst, tst$Wilshire5000[1])
    tst$model2Pred <- ecmpredict(model2, tst, tst$Wilshire5000[1])
