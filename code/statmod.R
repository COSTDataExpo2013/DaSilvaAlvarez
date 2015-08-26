
# Cluster and RF with SOTC data
source(file='code/data_chk.R')
library(reshape2)
library(stats)
library(randomForest)
library(fpc)
library(car)
library(plyr)
library(survey)
library(reldist)

# we choose 4 clusters
set.seed(1234)
km <- kmeans(soul.red2[, c(3, 9:45)], centers = 4)

# relation with attach, one cluster attach, one cluster not attach, two
# clusters in the middle
soul.red2$cl.id <- factor(km$cluster)

# Characterize the clusters with the numerical variables
pr <- soul.red2$PROJWT/3
des.pro <- svydesign(ids = ~1, weights = ~pr, data = soul.red2)
frm <- formula(paste("~", paste(names(soul.red2[, 3:45]), collapse = "+")))
means <- svyby(frm, ~cl.id + YEAR, data = soul.red2, design = des.pro, FUN = svymean, keep.var = TRUE, na.rm = T)
means <- means[, -grep("se.", colnames(means))]
des.cl1 <- melt(means, id.vars = c("cl.id", "YEAR"))
cv <- function(x) min(x) - max(x)
des.cl1$variable <- reorder(des.cl1$variable, des.cl1$value, cv)

save(km, des.cl1, file='data/res_cluster.Rdta')
###############################

fr <- as.formula("CCEGRP2~. -CCE-cl.id-QS3_02-Q3A-Q3B-Q3C-QCE1-QCE2-QS3-WEIGHT-PROJWT")
bosque <- dlply(soul.red2, .(QSB), function(x) randomForest(fr, importance = T, data = x))
xx <- ldply(bosque, function(x) t(importance(x)[, "MeanDecreaseGini"]))
xx <- data.frame(QSB = names(bosque), xx)

important <- melt(xx, id.vars = "QSB")
colnames(important) <- c("QSB", "dim", "mdg")
important <- ddply(important, .(QSB), mutate, mdg.pr = 100 * mdg/sum(mdg), 
                   rnk = rank(-mdg))
mdg.mm = ddply(important, .(dim), summarise, mdg.mm = mean(mdg.pr))
important <- merge(important, mdg.mm)
important$rmdg = with(important, (mdg.pr - mdg.mm) * (mdg.pr > mdg.mm))

rankings <- ddply(subset(important, rnk < 6), .(dim, rnk), function(x) length(x$dim))
rankings$rnk <- as.factor(rankings$rnk)
levels(rankings$rnk) <- paste("R", 1:5, sep = "")

# reorder QSB levels again
important <- merge(important, att.pr[, 1:2], by = "QSB")
important$QSB <- with(important, reorder(QSB, CCEGRP2Attached))
important$CCEGRP2Attached <- NULL

save(bosque, important,rankings, file='data/res_rf.Rdta')
