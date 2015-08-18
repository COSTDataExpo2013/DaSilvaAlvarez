# 1) read data tables from census
file <- c("agesex.csv", "education.csv", "income.csv", "owner.csv", "race.csv", 
          "workers.csv", "yearentry.csv")
for (i in 1:length(file)) {
  aux <- read.csv(file[i], header = T)
  aux <- subset(aux, X == "Estimate")
  for (j in 4:dim(aux)[2]) aux[, j] <- as.numeric(as.character(aux[, 
                                                                   j]))
  aux1 <- subset(aux, select = -c(1, 3), subset = !community %in% c("Gary", 
                                                                    "Philadelphia"))
  aux2 <- apply(subset(aux, community == "Gary", select = -c(1, 2, 3)), 
                2, sum)
  aux2 <- data.frame(community = "Gary", t(aux2))
  aux3 <- apply(subset(aux, community == "Philadelphia", select = -c(1, 
                                                                     2, 3)), 2, sum)
  aux3 <- data.frame(community = "Philadelphia", t(aux3))
  aux <- rbind(aux1, aux2, aux3)
  aux$community <- factor(aux$community)
  nam <- sub(pattern = ".csv", x = file[i], replacement = "")
  assign(nam, aux)
}
# for incomePC is slightly different ...
aux <- read.csv("incomePC.csv", header = T)
aux <- subset(aux, X == "Estimate")
aux[, 4] <- as.numeric(as.character(aux[, 4]))
aux1 <- subset(aux, select = -c(1, 3), subset = !community %in% c("Gary", 
                                                                  "Philadelphia"))
aux2 <- apply(subset(aux, community == "Gary", select = -c(1, 2, 3)), 2, 
              mean)
aux2 <- data.frame(community = "Gary", t(aux2))
aux3 <- apply(subset(aux, community == "Philadelphia", select = -c(1, 2, 
                                                                   3)), 2, mean)
aux3 <- data.frame(community = "Philadelphia", t(aux3))
aux <- rbind(aux1, aux2, aux3)
aux$community <- factor(aux$community)
assign("incomePC", aux)
remove(aux, aux1, aux2, aux3, i, j, nam)
# 2) create a data set with variables from every, topic. We compute
# some variables per topic and save it in census.data
census.data <- agesex[, 1]
# agesex has population by sex and age, the columns ending with 'F' are
# for female.  for now : agesex[,c(2,4,5,29)],
x <- which(colnames(agesex) == "Female.")
colnames(agesex)[-c(1:x)] <- paste(colnames(agesex)[-c(1:x)], "F", sep = "")
p <- c(2.5, 7, 12, 16, 18.5, 20, 21, 23, 27, 32, 37, 42, 47, 52, 57, 60.5, 
       63, 65.5, 68, 72, 77, 82, 90)
pp <- matrix(p, nrow = 26, ncol = 23, byrow = T)
agesex <- mutate(agesex, aveageM = apply(agesex[, 4:26] * pp, 1, sum)/agesex$Male, 
                 aveageF = apply(agesex[, 28:50] * pp, 1, sum)/agesex$Female, prop.male = agesex$Male/agesex$Total, 
                 prop.female = agesex$Female/agesex$Total)
agesex <- mutate(agesex, prop.m.f = agesex$aveageF/agesex$aveageM, pop100 = agesex[, 
                                                                                   2]/1e+05, prop1821 = apply(agesex[, c(8:10, 32:34)], 1, sum)/agesex$Total)
census.data <- cbind(census.data, agesex[, c(51:57)])
# ----------------------------------------- education has info, about
# education level and working status, unemployment rate can be computed
# from this. Universe: Pop between 25-64 years un rate
education <- mutate(education, unemployed = apply(education[, c("Unemployed", 
                                                                "Unemployed.1", "Unemployed.2", "Unemployed.3")], 1, sum), employed = apply(education[, 
                                                                                                                                                      c("Employed", "Employed.1", "Employed.2", "Employed.3")], 1, sum), 
                    militar = apply(education[, c("In.Armed.Forces", "In.Armed.Forces.1", 
                                                  "In.Armed.Forces.2", "In.Armed.Forces.3")], 1, sum), not.labor = apply(education[, 
                                                                                                                                   c("Not.in.labor.force", "Not.in.labor.force.1", "Not.in.labor.force.2", 
                                                                                                                                     "Not.in.labor.force.3")], 1, sum))
aux2 <- apply(education[, c("In.labor.force.", "In.labor.force..1", "In.labor.force..2", 
                            "In.labor.force..3")], 1, sum)
education <- mutate(education, un.rate = unemployed/aux2, pr.rate = aux2/education$Total.)
pet <- apply(agesex[, c(12:20, 36:44)], 1, sum)
census.data <- cbind(census.data, education[, c(3, 10, 17, 24, 31:34)]/education$Total., 
                     education[, c(35, 36)])
# ------------------------------------- income has info about income
# level, we should pooled some categories, or some meassure of
# inequality can be computed here and compute average income per
# community.
# mean point for income levels in miles (5=5000), we are not sure about
# the upper limit, range is 'over 200'.
p2 <- c(5, 12.5, 17, 22.5, 27, 32.5, 37.5, 42.5, 47.5, 55, 67.5, 87.5, 
        112.5, 137.5, 175, 200)
pp2 <- matrix(p2, nrow = 26, ncol = 16, byrow = T)
income <- mutate(income, av.income = (apply(income[, -c(1, 2)] * pp2, 1, 
                                            sum)/income$Total.) * 1000, income.pc = (apply(income[, -c(1, 2)] * 
                                                                                             pp2, 1, sum)/agesex$Total.) * 1000)
income$gini <- 0
for (i in 1:26) {
  d <- rbind(mid = 1000 * p2, cont = as.vector(income[i, 3:18]))
  d <- data.frame(t(d))
  income$gini[i] <- with(d, gini(mid, weights = cont))
}
census.data <- cbind(census.data, income[, 19:21])
# ------------------------------------------------------- owner: People
# living in housholds, renting vs owning and how long are living in the
# same place.
colnames(owner)[2] <- "Total"
p3 <- c(5, 10, 15, 25, 35, 45)
pp <- matrix(rep(p3, 2), nrow = 26, ncol = 6, byrow = T)
owner <- mutate(owner, own.rate = Owner.occupied./Renter.occupied., ow.prop10 = apply(owner[, 
                                                                                            6:9], 1, sum)/owner$Owner.occupied., ren.prop10 = apply(owner[, 13:16], 
                                                                                                                                                    1, sum)/owner$Renter.occupied., av.rent = apply(owner[, c(11:16)] * 
                                                                                                                                                                                                      pp, 1, sum)/owner$Renter.occupied., av.own = apply(owner[, c(4:9)] * 
                                                                                                                                                                                                                                                           pp, 1, sum)/owner$Owner.occupied.)
census.data <- cbind(census.data, owner[, 17:21])
# --------------------------------------------------------- race, we
# can use proportion os white people, and also entropy measure with the
# races distribution in each community, sum(p*log(p))
race <- mutate(race, white.prop = White/Total., afri.prop = Black/Total., 
               asian.prop = Asian/Total.)
aux <- apply(race[, -c(1, 2, 3, 4, 6)], 1, sum)
race$other.prop <- aux/race$Total.
census.data <- cbind(census.data, race[, 12:15])
# ----------------------------------------------------------- workers
# Proportion of household with 1,2 or more than 3 workers
# (prop.1worker,prop.2worker,prop.3worker) Proportion of houses with
# one two ando so on persons
# (prop.1.per,prop.2.per,prop.3.per,prop.3.per) Proportion of no
# workers and one worker in households with only one person
# (prop1.no.workers, prop1.worker) proportion of no workers, one worker
# and two workers in households with two person (prop2.no.workers,
# prop2.1worker,prop.2worker ) same than previous but for households
# with Three person househole (prop3.no.workers,prop3.1worker,
# prop3.2worker,prop3.3worker ) same than previous but for households
# with four person or more househole
# (prop3.no.workers,prop3.1worker,prop3.2worker,prop3.3worker)
workers <- mutate(workers, prop.no.workers = workers$No.workers/workers$Total., 
                  prop.1worker = workers$X1.worker/workers$Total., prop.2worker = workers$X2.workers/workers$Total., 
                  prop.3worker = workers$X3.or.more.workers/workers$Total., prop.1.per = workers$X1.person.household./workers$Total., 
                  prop.2.per = workers$X2.person.household./workers$Total., prop.3.per = workers$X3.person.household./workers$Total., 
                  prop.4.per = workers$X4.or.more.person.household./workers$Total, prop1.no.workers = workers$No.workers.1/workers$X1.person.household., 
                  prop1.worker = workers$X1.worker.1/workers$X1.person.household., prop2.no.workers = workers$No.workers.2/workers$X2.person.household., 
                  prop2.1worker = workers$X1.worker.2/workers$X2.person.household., prop2.2worker = workers$X2.workers.1/workers$X2.person.household., 
                  prop3.no.workers = workers$No.workers.2/workers$X3.person.household., 
                  prop3.1worker = workers$X1.worker.3/workers$X3.person.household., prop3.2worker = workers$X2.workers.2/workers$X3.person.household., 
                  prop3.3worker = workers$X3.workers/workers$X3.person.household., prop3.no.workers = workers$No.workers.2/workers$X3.person.household., 
                  prop3.1worker = workers$X1.worker.3/workers$X3.person.household., prop3.2worker = workers$X2.workers.2/workers$X3.person.household., 
                  prop3.3worker = workers$X3.workers/workers$X3.person.household.)
census.data <- cbind(census.data, workers[, 24:40])
# -----------------------------------------------------------------------
# yearentry proportion of person entry 2000 or later
# (prop.new.entry,prop.entry.90s,prop.entry.80s,prop.entry.prev.80s)
# proportiono of native and foreign for different entries
# (prop.nat.new.entry,prop.for.new.entry,prop.nat.entry.90s,prop.for.entry.90s)
# proportion of persons not original from the place (entred,entred.nat,
# entred.for) we can separate foreign born in naturalized us citizen
# and not us citizen (entred.for.nat.us, entred.for.not.us)
yearentry <- mutate(yearentry, prop.new.entry = yearentry$Entered.2000.or.later/yearentry$Total., 
                    prop.entry.90s = yearentry$Entered.1990.to.1999./yearentry$Total., 
                    prop.entry.80s = yearentry$Entered.1980.to.1989./yearentry$Total., 
                    prop.entry.prev.80s = yearentry$Entered.before.1980./yearentry$Total., 
                    prop.nat.new.entry = yearentry$Native/yearentry$Entered.2000.or.later, 
                    prop.for.new.entry = yearentry$Foreign.born./yearentry$Entered.2000.or.later, 
                    prop.nat.entry.90s = yearentry$Native.1/yearentry$Entered.1990.to.1999., 
                    prop.for.entry.90s = yearentry$Foreign.born..1/yearentry$Entered.1990.to.1999., 
                    prop.nat.entry.80s = yearentry$Native.2/yearentry$Entered.1980.to.1989., 
                    prop.for.entry.80s = yearentry$Foreign.born..2/yearentry$Entered.1980.to.1989., 
                    prop.nat.entry.prev.80s = yearentry$Native.3/yearentry$Entered.before.1980., 
                    prop.for.entry.prev.80s = yearentry$Foreign.born..3/yearentry$Entered.before.1980., 
                    entred = yearentry$Total./agesex$Total., entred.nat = yearentry$Native/agesex$Total., 
                    entred.for = yearentry$Foreign.born./agesex$Total., entred.for.nat.us = yearentry$Naturalized.U.S..citizen/agesex$Total., 
                    entred.for.not.us = yearentry$Not.a.U.S..citizen/agesex$Total.)
census.data <- cbind(census.data, yearentry[, 23:38])
census.data$income.pc <- incomePC$income.pc
colnames(census.data)[1] <- "QSB"
# Compute comunity proportion of attached people, carefull casuse
# comunities are in different order in soul data and census data.
aux <- with(soul, table(QSB, CCEGRP2))
tot <- apply(aux, 1, sum)
prop.atach <- data.frame(aux[, 1]/tot, aux[, 2]/tot, aux[, 3]/tot)
colnames(prop.atach) <- colnames(aux)
prop.atach <- prop.atach[order(rownames(prop.atach)), ]
census.data <- census.data[order(census.data[, 1]), ]
census.data <- data.frame(QSB = rownames(prop.atach), census.data[, -1], 
                          prop.atach)
census.data$QSB <- with(census.data, reorder(QSB, Attached))
# -----------------------------
corrs <- cor(census.data[, -1])
aux1 <- sort(corrs[, "Attached"])
dat.cor <- rbind(data.frame(vars = names(corrs[1:61, "Attached"]), cor = corrs[1:61, 
                                                                               "Attached"], positive = corrs[1:61, "Attached"] > 0, type = "Attached"), 
                 data.frame(vars = names(corrs[1:61, "Not.Attached"]), cor = corrs[1:61, 
                                                                                   "Not.Attached"], positive = corrs[1:61, "Not.Attached"] > 0, type = "Not.Attached"))
rownames(dat.cor) <- NULL
dat.cor$vars <- reorder(dat.cor$vars, rep(dat.cor$cor[dat.cor$type == "Attached"], 
                                          2))
dat.cor2 <- subset(dat.cor, vars %in% names(aux1[c(2:11, 51:61)]))
dat.cor2$vars <- factor(dat.cor2$vars)