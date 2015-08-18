# Fit RF on each community, to explain CCE with the constructed
# variables
fr <- as.formula("CCEGRP2~. -CCE-cl.id-QS3_02-Q3A-Q3B-Q3C-QCE1-QCE2-QS3-WEIGHT-PROJWT")
bosque <- dlply(soul.red2, .(QSB), function(x) randomForest(fr, importance = T, 
                                                            data = x))
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