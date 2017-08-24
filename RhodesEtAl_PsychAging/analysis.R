# Analysis of Pr, Br, d', and c for Rhodes, S., Parra, M. A., Cowan, N., & Logie, R. H. (2017). Healthy Aging and Visual Working Memory: The Role of Mixing Feature and Conjunction Changes. Psychology & Aging, 32 (4), 354-366.

setwd('data/')

library(BayesFactor)
iter = 5e+04
addIter = 1e+04

# EXPERIMENT 1 ------
exp1 <- read.table('Exp1_data')
exp1$ppt_no <- as.factor(exp1$ppt_no)
exp1$SetSize <- as.factor(exp1$SetSize)

# Pr
BF.1.Pr <- anovaBF(Pr ~ Condition*MemCondition*SetSize*Age_Group + ppt_no, data = exp1, 
                    whichModels = 'top', whichRandom = 'ppt_no', iterations = iter)

while (any(extractBF(BF.1.Pr)$error > 0.05)){
  BF.1.Pr = recompute(BF.1.Pr, iterations = addIter)
}

# Br
BF.1.Br <- anovaBF(Br ~ Condition*MemCondition*SetSize*Age_Group + ppt_no, data = exp1, 
                   whichModels = 'top', whichRandom = 'ppt_no', iterations = iter)

while (any(extractBF(BF.1.Br)$error > 0.05)){
  BF.1.Br = recompute(BF.1.Br, iterations = addIter)
}

# d'
BF.1.d <- anovaBF(dprime ~ Condition*MemCondition*SetSize*Age_Group + ppt_no, data = exp1, 
                   whichModels = 'top', whichRandom = 'ppt_no', iterations = iter)

while (any(extractBF(BF.1.d)$error > 0.05)){
  BF.1.d = recompute(BF.1.d, iterations = addIter)
}

# c
BF.1.c <- anovaBF(criterion ~ Condition*MemCondition*SetSize*Age_Group + ppt_no, data = exp1, 
                  whichModels = 'top', whichRandom = 'ppt_no', iterations = iter)

while (any(extractBF(BF.1.c)$error > 0.05)){
  BF.1.c = recompute(BF.1.c, iterations = addIter)
}

# EXPERIMENT 2 -----
exp2 <- read.table('Exp2_data')
exp2$ppt_no <- as.factor(exp2$ppt_no)
exp2$SetSize <- as.factor(exp2$SetSize)

# Pr
BF.2.Pr <- anovaBF(Pr ~ Condition*MemCondition*SetSize*Age_Group + ppt_no, data = exp2, 
                   whichModels = 'top', whichRandom = 'ppt_no', iterations = iter)

while (any(extractBF(BF.2.Pr)$error > 0.05)){
  BF.2.Pr = recompute(BF.2.Pr, iterations = addIter)
}

# Br
BF.2.Br <- anovaBF(Br ~ Condition*MemCondition*SetSize*Age_Group + ppt_no, data = exp2, 
                   whichModels = 'top', whichRandom = 'ppt_no', iterations = iter)

while (any(extractBF(BF.2.Br)$error > 0.05)){
  BF.2.Br = recompute(BF.2.Br, iterations = addIter)
}

# d'
BF.2.d <- anovaBF(dprime ~ Condition*MemCondition*SetSize*Age_Group + ppt_no, data = exp2, 
                  whichModels = 'top', whichRandom = 'ppt_no', iterations = iter)

while (any(extractBF(BF.2.d)$error > 0.05)){
  BF.2.d = recompute(BF.2.d, iterations = addIter)
}

# c
BF.2.c <- anovaBF(criterion ~ Condition*MemCondition*SetSize*Age_Group + ppt_no, data = exp2, 
                  whichModels = 'top', whichRandom = 'ppt_no', iterations = iter)

while (any(extractBF(BF.2.c)$error > 0.05)){
  BF.2.c = recompute(BF.2.c, iterations = addIter)
}

# EXPERIMENT 2 FOLLOW UP ------
exp2b <- read.table('Exp2b_data')
exp2b$ppt_no <- as.factor(exp2b$ppt_no)
exp2b$SetSize <- as.factor(exp2b$SetSize)

# Pr
BF.2b.Pr <- anovaBF(Pr ~ MemCondition*SetSize*Age_Group + ppt_no, data = exp2b, 
                   whichModels = 'top', whichRandom = 'ppt_no', iterations = iter)

while (any(extractBF(BF.2b.Pr)$error > 0.05)){
  BF.2b.Pr = recompute(BF.2b.Pr, iterations = addIter)
}

# Br
BF.2b.Br <- anovaBF(Br ~ MemCondition*SetSize*Age_Group + ppt_no, data = exp2b, 
                   whichModels = 'top', whichRandom = 'ppt_no', iterations = iter)

while (any(extractBF(BF.2b.Br)$error > 0.05)){
  BF.2b.Br = recompute(BF.2b.Br, iterations = addIter)
}

# d'
BF.2b.d <- anovaBF(dprime ~ MemCondition*SetSize*Age_Group + ppt_no, data = exp2b, 
                  whichModels = 'top', whichRandom = 'ppt_no', iterations = iter)

while (any(extractBF(BF.2b.d)$error > 0.05)){
  BF.2b.d = recompute(BF.2b.d, iterations = addIter)
}

# c
BF.2b.c <- anovaBF(criterion ~ MemCondition*SetSize*Age_Group + ppt_no, data = exp2b, 
                  whichModels = 'top', whichRandom = 'ppt_no', iterations = iter)

while (any(extractBF(BF.2b.c)$error > 0.05)){
  BF.2b.c = recompute(BF.2b.c, iterations = addIter)
}

save.image('BF_results.RData')
