
library(dplyr)
library(tidyr)
library(reldist)

# 1) read data tables from census
ll <- list.files('data')
file <- ll[-c(grep('sotc',ll),grep('PC',ll))]

for (i in 1:length(file)) {
  aux <- read.csv(paste('data/',file[i],sep=''), header = T)
  aux <- aux %>% filter(X=='Estimate') %>% 
    mutate_each(funs(as.character), vars=-c(1:3) )  %>% 
    mutate_each(funs(as.numeric), vars=-c(1:3) ) %>% 
    group_by(community)  %>%
    summarise_each( funs(sum),vars=-c(1:3) )

  aux$community <- factor(aux$community)  
  nam <- sub(pattern = ".csv", x = file[i], replacement = "")
  assign(nam, aux)
}

# for incomePC is slightly different ...
aux <- read.csv("data/incomePC.csv", header = T)

aux <- aux %>% filter(X=='Estimate') %>% 
  mutate_each(funs(as.character), vars=-c(1:3) )  %>% 
  mutate_each(funs(as.numeric), vars=-c(1:3) ) %>% 
  group_by(community)  %>%
  summarise_each( funs(mean),vars=-c(1:3) )
assign("incomePC", aux)

remove(aux, nam, i, file, ll)


# 2) create a data set with variables from every, topic. We compute
# some variables per topic and save it in census.data
census.data <- agesex[, 1]

# agesex has population by sex and age, the columns ending with 'F' are
# for female.  for now : agesex[,c(2,4,5,29)],
x <- which(colnames(agesex) == "Female.")
colnames(agesex)[-c(1:x)] <- paste(colnames(agesex)[-c(1:x)], "F", sep = "")
p <- c(2.5, 7, 12, 16, 18.5, 20, 21, 23, 27, 32, 37, 42, 47, 52, 57, 60.5, 63, 65.5, 68, 72, 77, 82, 90)
pp <- matrix(p, nrow = 26, ncol = 23, byrow = T)

agesex <- agesex %>% 
    mutate(aveageM = apply(agesex[, 4:26] * pp, 1, sum)/Male. ,
           aveageF = apply(agesex[, 28:50] * pp, 1, sum)/Female. , 
           prop.male = Male./Total., 
           prop.female = Female./Total. , 
           prop.m.f = aveageF/aveageM,
           pop100 =  Total. /1e+05, 
           prop1821 = apply(agesex[, c(8:10, 32:34)], 1, sum)/Total.)

census.data <- inner_join(census.data, agesex[, c(1,51:57)], 'community')

# ----------------------------------------- 
# education has info, about education level and working status, unemployment rate can be computed
# from this. Universe: Pop between 25-64 years un rate

education <- education %>% mutate( 
              unemployed = apply(education[, grep('Unemployed', colnames(education))], 1, sum), 
              employed = apply(education[, grep('Employed', colnames(education))], 1, sum),                         
              militar = apply(education[, grep('In.Armed.Forces', colnames(education))], 1, sum), 
              not.labor = apply(education[, grep('Not.in.labor.force', colnames(education))], 1, sum),
              labor = apply(education[, grep('In.labor.force', colnames(education))], 1, sum),
              un.rate =  unemployed/labor, 
              pr.rate = labor/Total.
              )
pet <- apply(agesex[, c(12:20, 36:44)], 1, sum)

census.data <- inner_join(census.data, cbind(education[, c(3, 10, 17, 24, 31:34)]/education$Total., education[, c(1,36, 37)]), 'community')

# ------------------------------------- 
# income has info about income level, we should pooled some categories, or some meassure of
# inequality can be computed here and compute average income per community.
# mean point for income levels in miles (5=5000), we are not sure about
# the upper limit, range is 'over 200'.
p2 <- c(5, 12.5, 17, 22.5, 27, 32.5, 37.5, 42.5, 47.5, 55, 67.5, 87.5, 112.5, 137.5, 175, 200)
pp2 <- matrix(p2, nrow = 26, ncol = 16, byrow = T)

income <- income %>% 
  mutate( av.income = (apply(income[, -c(1, 2)] * pp2, 1, sum)/income$Total.) * 1000, 
          income.pc = (apply(income[, -c(1, 2)] * pp2, 1, sum)/agesex$Total.) * 1000)

xx <- income %>% gather('grp', 'n', 3:18) %>%
  mutate(mid = rep(1000*p2, each=26)) %>% 
  group_by(community) %>% 
  summarise( gini = gini(mid, n)) 

income <- inner_join(income, xx, 'community' )
census.data <-inner_join(census.data, income[, c(1,19:21)],'community' )


# -------------------------------------------------------
# owner: People living in housholds, renting vs owning and how long are living in the
# same place.
colnames(owner)[2] <- "Total"
p3 <- c(5, 10, 15, 25, 35, 45)
pp <- matrix(rep(p3, 2), nrow = 26, ncol = 6, byrow = T)
owner <- owner %>% 
  mutate(own.rate = Owner.occupied./Renter.occupied., 
         ow.prop10 = apply(owner[,6:9], 1, sum)/Owner.occupied., 
         ren.prop10 = apply(owner[, 13:16], 1, sum)/Renter.occupied., 
         av.rent = apply(owner[, c(11:16)] * pp, 1, sum)/Renter.occupied., 
         av.own = apply(owner[, c(4:9)] * pp, 1, sum)/Owner.occupied.
         )

census.data <- inner_join(census.data, owner[, c(1,17:21)], 'community')

# --------------------------------------------------------- 
# race, we can use proportion os white people, and also entropy measure with the
# races distribution in each community, sum(p*log(p))

aux <- apply(race[, -c(1, 2, 3, 4, 6)], 1, sum)
race <- race %>% 
       mutate(white.prop = White/Total., afri.prop = Black/Total., 
              asian.prop = Asian/Total., 
              other.prop <- aux/race$Total.
              )

census.data <- cbind(census.data, race[, 12:15])

# ----------------------------------------------------------- 
# workers: Proportion of household with 1,2 or more than 3 workers
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

house.props <- workers %>% 
  select( -grep('worker', colnames(workers)) ) %>% 
  gather('house', 'cnt', -c(1:2)) %>% 
  mutate(prop = cnt / Total.) %>%
  select(community, house, prop) %>%
  spread(house,prop)

colnames(house.props)[-1] <-  c('prop.1.per', 'prop.2.per', 'prop.3.per', 'prop.4.per')   

xx1 <- workers %>% select(1:6) %>% gather('worker', 'cnt.w', -c(1:2)) %>% mutate(house='Total') %>% rename(cnt.h = Total.)
xx2 <- workers %>% select(c(1,7:9)) %>% gather('worker', 'cnt.w',-c(1:2)) %>% mutate(house='1per') %>% rename(cnt.h = X1.person.household.)
xx3 <- workers %>% select(c(1,10:13)) %>% gather('worker', 'cnt.w', -c(1:2)) %>% mutate(house='2per') %>% rename(cnt.h = X2.person.household.)                    
xx4 <- workers %>% select(c(1,14:18)) %>% gather('worker', 'cnt.w', -c(1:2)) %>% mutate(house='3per') %>% rename(cnt.h = X3.person.household.)
xx <- rbind(xx1,xx2,xx3,xx4)

work.prop <- xx %>% 
  unite(ho_wk, house,worker) %>%
  mutate(prop = cnt.w / cnt.h) %>%
  select(community, ho_wk, prop) %>%
  spread(ho_wk,prop)

colnames(work.prop)[-1] <- c("prop1.no.workers","prop1.worker", "prop2.no.workers", "prop2.1worker",    "prop2.2worker",  "prop3.no.workers", "prop3.1worker",    "prop3.2worker" ,   "prop3.3worker","prop.no.workers" ,"prop.1worker" ,    "prop.2worker"   ,  "prop.3worker")  
workers <- cbind(workers, house.props[,-1], work.prop[,-1])

census.data <- inner_join(census.data, workers[, c(1,24:40)], 'community')  

# -----------------------------------------------------------------------
# yearentry proportion of person entry 2000 or later
# (prop.new.entry,prop.entry.90s,prop.entry.80s,prop.entry.prev.80s)
# proportiono of native and foreign for different entries
# (prop.nat.new.entry,prop.for.new.entry,prop.nat.entry.90s,prop.for.entry.90s)
# proportion of persons not original from the place (entred,entred.nat,
# entred.for) we can separate foreign born in naturalized us citizen
# and not us citizen (entred.for.nat.us, entred.for.not.us)

prop.year <- yearentry %>% 
  select(c(1:3, 8, 13, 18)) %>%  
  gather('year', 'cnt', -c(1:2)) %>% 
  mutate(prop = cnt / Total.) %>%
  select(community, year, prop) %>%
  spread(year,prop)
colnames(prop.year)[-1] <- c('prop.new.entry','prop.entry.90s','prop.entry.80s', 'prop.entry.prev.80s' )

prop.origin <- yearentry %>% 
  select(c(1:2,4:7)) %>%  
  inner_join(agesex[, 1:2], by='community') %>% 
  gather('from', 'cnt', -c(community, Total..y))  %>% 
  mutate(prop = cnt / Total..y) %>%
  select(community, from, prop) %>%
  spread(from,prop)
colnames(prop.origin)[-1] <- c('entered', 'entered.nat', 'entered.for', 'entered.for.nat.us', 'entered.for.not.us')

xx1 <- yearentry %>% select(1,3:5) %>% gather('orig', 'cnt', -c(1:2)) %>% mutate(date='ent2000') %>% rename(cnt.h = Entered.2000.or.later.)
xx2 <- yearentry %>% select(1,8:10) %>% gather('orig', 'cnt', -c(1:2)) %>% mutate(date='ent90') %>% rename(cnt.h = Entered.1990.to.1999.)
xx3 <- yearentry %>% select(1,13:15) %>% gather('orig', 'cnt', -c(1:2)) %>% mutate(date='ent80') %>% rename(cnt.h = Entered.1980.to.1989.)
xx4 <- yearentry %>% select(1,18:20) %>% gather('orig', 'cnt', -c(1:2)) %>% mutate(date='ent_bef80') %>% rename(cnt.h = Entered.before.1980.)
xx <- rbind(xx1,xx2,xx3,xx4)

prop.enter <- xx %>% 
  unite(org_dt, orig ,date) %>%
  mutate(prop = cnt / cnt.h) %>%
  select(community,org_dt, prop) %>%
  spread(org_dt,prop)

colnames(prop.enter)[-1] <- c(paste('prop.for', c('new.entry', paste('entry',c('90s', '80s', 'prev.80s'), sep='.' ) )  ,sep='.' ),  paste('prop.nat', c('new.entry', paste('entry',c('90s', '80s', 'prev.80s'), sep='.' ) )  ,sep='.' ) ) 
yearentry <- cbind(yearentry, prop.origin[,-1], prop.year[,-1], prop.enter[,-1])

census.data <- inner_join(census.data, yearentry[, c(1,23:38)], 'community')

# --------------------------------------------------------------

colnames(census.data)[1] <- "QSB"
census.data$income.pc <- incomePC$income.pc



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
