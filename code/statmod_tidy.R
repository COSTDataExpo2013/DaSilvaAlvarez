# Cluster and RF with SOTC data
source(file='code/data_chk.R')
library(tidyverse)
library(randomForest)
library(survey)

# we choose 4 clusters
set.seed(1234)
km <- stats::kmeans(soul.red2[, c(3, 9:45)], centers = 4)
soul.red2$cl.id <- factor(km$cluster)

# Characterize the clusters with the numerical variables
des.cl1 <- svyby(formula = formula(paste("~", paste(names(soul.red2[, 3:45]), collapse = "+"))),
      by = ~cl.id + YEAR, 
      data = soul.red2, 
      design = svydesign(ids = ~1, weights = soul.red2$PROJWT/3, data = soul.red2), 
      FUN = svymean, keep.var = TRUE, na.rm = T) %>%
  select(-starts_with('se') ) %>%
  gather( vars, value, -cl.id, -YEAR ) %>% 
  mutate(vars = stats::reorder(vars, value, FUN=function(x) max(x)-min(x))) 

list(des.cl1=des.cl1, km = km) %>%
  saveRDS( file = 'data/res_cluster.rds')
  
###############################

fr <- as.formula("CCEGRP2~. -CCE-cl.id-QS3_02-Q3A-Q3B-Q3C-QCE1-QCE2-QS3-WEIGHT-PROJWT")

rf_model <- function(df) {
  randomForest(fr, importance=TRUE, data=df)
}

imp_fn <- function(rf) {
  importance(rf) %>% data.frame() %>% 
    rownames_to_column(var = 'dim') %>% select(dim, MeanDecreaseGini) 
}

bosque <- soul.red2 %>%
  group_by(QSB) %>% nest() %>%
  mutate(bosque = map(data, rf_model) )

important <- bosque %>% 
  mutate(importance = map(bosque, imp_fn)) %>%
  unnest(importance) %>% set_names(nm = c("QSB", "dim", "mdg")) %>%
  group_by(QSB) %>%
  mutate(mdg.pr = 100 * mdg/sum(mdg), rnk = rank(-mdg) ) %>%
  group_by(dim) %>%
  mutate(mdg.mm=mean(mdg.pr)) %>%
  ungroup() %>%
  mutate(rmdg = (mdg.pr - mdg.mm) * (mdg.pr > mdg.mm) )

rankings <- important %>% 
  subset(rnk < 6) %>% group_by(dim, rnk) %>%
  summarise(freq.rnk = n() ) %>%
  ungroup() %>%
  mutate(rnk2 = factor(rnk, labels = paste("R", 1:5, sep = "") ))

# save one list object
list(bosque=bosque, important=important, rankings=rankings) %>%
  saveRDS( file = 'data/res_rf.rds')

