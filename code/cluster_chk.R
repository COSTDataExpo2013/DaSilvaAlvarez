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
means <- svyby(frm, ~cl.id + YEAR, data = soul.red2, design = des.pro, 
               FUN = svymean, keep.var = TRUE, na.rm = T)
means <- means[, -grep("se.", colnames(means))]
des.cl1 <- melt(means, id.vars = c("cl.id", "YEAR"))
cv <- function(x) min(x) - max(x)
des.cl1$variable <- reorder(des.cl1$variable, des.cl1$value, cv)
