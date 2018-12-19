

library(tidyverse)
library(reldist)

# 1) read data tables from census
ll <- list.files('data', pattern='.csv')
file <- ll[-c(grep('sotc',ll),grep('PC',ll))]

dt.list <- 
  lapply( file, function(dd) {
    read.csv(paste('data/', dd ,sep=''), header = T) %>% 
      filter(X=='Estimate') %>% 
      mutate_at(vars(-c(1:3)), funs(as.character) )  %>% 
      mutate_at(vars(-c(1:3)), funs(as.numeric) ) %>% 
      group_by(community)  %>%
      summarise_at( vars(-c(1:3) ), funs(sum) ) %>%
      mutate(community = factor(community) )
  })
names(dt.list) <- sub(pattern = ".csv", x = file, replacement = "")
  
# for incomePC is slightly different ...
dt.list$incomePC <- read.csv("data/incomePC.csv", header = T) %>% 
  filter(X=='Estimate') %>% 
  mutate_at(vars(-c(1:3)), funs(as.character) )  %>% 
  mutate_at(vars(-c(1:3)), funs(as.numeric) ) %>% 
  group_by(community)  %>%
  summarise_at( vars(-c(1:3) ), funs(mean) )

############################################################
# 2) create a data set with variables from every, topic. 
# We compute some variables per topic and save it in census.data
# census.data <- agesex[, 1]

# 2.1 agesex has population by sex and age, 
# the columns ending with '1' correspond to female population
p <- c(2.5, 7, 12, 16, 18.5, 20, 21, 23, 27, 32, 37, 42, 47, 52, 57, 60.5, 63, 65.5, 68, 72, 77, 82, 90)
pp <- matrix(p, nrow = 26, ncol = 23, byrow = T)

agesex <- dt.list$agesex %>% 
  mutate(aveageM = apply(.[, 4:26] * pp, 1, sum)/Male.,  
         aveageF = apply(.[, 28:50] * pp, 1, sum)/Female. , 
         prop.male = Male./Total., 
         prop.female = Female./Total. , 
         prop.m.f = aveageF/aveageM,
         pop100 =  Total. /1e+05, 
         prop1821 = apply(dt.list$agesex[, c(8:10, 32:34)], 1, sum)/Total.) %>%
  select(community, aveageM:prop1821) %>%
  arrange(community)

# ----------------------------------------- 
# 2.2) Education 
# education level and working status. unemployment rate can be computed from this.
# Universe: Pop between 25-64 years un rate
education <- dt.list$education %>% 
  mutate( 
  unemployed = rowSums( select(., contains('Unemployed', ignore.case = FALSE)) ),
  employed = rowSums(select(. , contains('Employed', ignore.case = FALSE) ) ),
  militar = rowSums(select(. , contains('In.Armed.Forces', ignore.case = FALSE)) ), 
  not.labor = rowSums(select(. , contains('Not.in.labor.force', ignore.case = FALSE)) ),
  labor = rowSums(select(. , contains('In.labor.force', ignore.case = FALSE)) ),
  un.rate =  unemployed/labor, 
  pr.rate = labor/Total.
  ) %>%
  select(community, Total., contains('school'), contains('degree'), unemployed:pr.rate ) %>% 
  gather(var, value, -community, -Total., -un.rate, -pr.rate) %>%
  mutate(prop = value/Total.) %>%
  select(-value, -Total.) %>%
  spread(var, prop) %>%
  arrange(community)
rm(p, pp)

# ------------------------------------- 
# 2.3) income
p2 <- c(5, 12.5, 17, 22.5, 27, 32.5, 37.5, 42.5, 47.5, 55, 67.5, 87.5, 112.5, 137.5, 175, 200)
pp2 <- matrix(p2, nrow = 26, ncol = 16, byrow = T)

income <- dt.list$income %>% gather('grp', 'n', 3:18) %>%
  mutate(mid = rep(1000*p2, each=26)) %>% 
  group_by(community) %>% 
  summarise( gini = gini(mid, n)) %>% 
  mutate( av.income = (apply(dt.list$income[, -c(1, 2)] * pp2, 1, sum)/dt.list$income$Total.) * 1000)
  
rm(p2, pp2)

# -------------------------------------------------------
# 2.4 owner: 
# People living in housholds, renting vs owning and how long are living in current place.
colnames(dt.list$owner)[2] <- "Total"
p3 <- c(5, 10, 15, 25, 35, 45)
pp <- matrix(rep(p3, 2), nrow = 26, ncol = 6, byrow = T)

owner <- dt.list$owner %>% 
  mutate(own.rate = Owner.occupied./Renter.occupied.,
         ow.prop10 = apply(.[,6:9], 1, sum)/Owner.occupied., 
         ren.prop10 = apply(.[, 13:16], 1, sum)/Renter.occupied., 
         av.rent = apply( .[, c(11:16)] * pp, 1, sum)/Renter.occupied., 
         av.own = apply( .[, c(4:9)] * pp, 1, sum)/Owner.occupied.
  ) %>%
  select(c(1,17:21))
rm(p3, pp)

# --------------------------------------------------------- 
#2.5) race, 
# proportion of white, african-american, asian and other
race <- dt.list$race %>% 
  mutate(
    Other = apply(.[, -c(1, 2, 3, 4, 6)], 1, sum),
    white.prop = White/Total., 
    afri.prop = Black/Total., 
    asian.prop = Asian/Total., 
    other.prop = Other/Total.
  ) %>% 
  select(community, ends_with('.prop'))


# ----------------------------------------------------------- 
# 2.6) workers: 
# Total of working people in household, dividing by household size. 

# household size proportion
house.props <- dt.list$workers %>% 
  select( community, Total., contains('household') ) %>% 
  gather('house', 'cnt', -community, -Total. ) %>% 
  mutate(prop = cnt / Total.) %>%
  select(community, house, prop) %>%
  spread(house,prop) %>% 
  setNames( nm = c('community','prop.1.per', 'prop.2.per', 'prop.3.per', 'prop.4.per'))

# working people proportions
work.prop <- list(
  Total = dt.list$workers %>% select(1:6) %>% gather('worker', 'cnt.w', -c(1:2)) %>% rename(cnt.h = Total.),
  per1 = dt.list$workers %>% select(c(1,7:9)) %>% gather('worker', 'cnt.w',-c(1:2)) %>% rename(cnt.h = X1.person.household.),
  per2 = dt.list$workers %>% select(c(1,10:13)) %>% gather('worker', 'cnt.w', -c(1:2)) %>% rename(cnt.h = X2.person.household.),                    
  per3 = dt.list$workers %>% select(c(1,14:18)) %>% gather('worker', 'cnt.w', -c(1:2)) %>% rename(cnt.h = X3.person.household.)
) %>% bind_rows(.id = 'house')  %>% 
  unite(ho_wk, house,worker) %>%
  mutate(prop = cnt.w / cnt.h) %>%
  select(community, ho_wk, prop) %>%
  spread(ho_wk,prop) %>% 
  setNames(nm = c("community" ,"prop1.no.workers","prop1.worker", "prop2.no.workers", "prop2.1worker",    "prop2.2worker",  "prop3.no.workers", "prop3.1worker",    "prop3.2worker" ,   "prop3.3worker","prop.no.workers" ,"prop.1worker" ,    "prop.2worker"   ,  "prop.3worker")  )

# -----------------------------------------------------------------------
# 2.7) yearentry:
# year of etnry in the community, dividing by us origin or foreign. 

# year of entry proportions
prop.year <- dt.list$yearentry %>% 
  select(community, Total., contains('Entered') )  %>%  
  gather('year', 'cnt', contains('Entered') ) %>% 
  mutate(prop = cnt / Total.) %>%
  select(community, year, prop) %>%
  spread(year,prop) %>%
  set_names(nm = c('community','prop.new.entry','prop.entry.90s','prop.entry.80s', 'prop.entry.prev.80s' ))


# origin proportions by year of entry
prop.origin <- dt.list$yearentry %>% 
  select( -matches( '[[:digit:]]' ) ) %>%  
  inner_join( select(dt.list$agesex, community, Total.), by='community' ) %>% 
  gather('from', 'cnt', -c(community, Total..y))  %>% 
  mutate(prop = cnt / Total..y) %>%
  select(community, from, prop) %>%
  spread(from,prop) %>% 
  set_names(nm = c('community','entered', 'entered.nat', 'entered.for', 'entered.for.nat.us', 'entered.for.not.us') )



# origin proportions

prop.enter <- list( 
  ent2000 =  dt.list$yearentry %>% select(1,3:5) %>% gather('orig', 'cnt', -c(1:2)) %>% rename(cnt.h = Entered.2000.or.later.),  
  ent90 = dt.list$yearentry %>% select(1,8:10) %>% gather('orig', 'cnt', -c(1:2)) %>% rename(cnt.h = Entered.1990.to.1999.),
  ent80 = dt.list$yearentry  %>% select(1,13:15) %>% gather('orig', 'cnt', -c(1:2)) %>% rename(cnt.h = Entered.1980.to.1989.),
  ent_bef80 = dt.list$yearentry %>%  select(1,18:20) %>% gather('orig', 'cnt', -c(1:2))  %>% rename(cnt.h = Entered.before.1980.)
) %>% bind_rows(.id = 'date') %>% 
  unite(org_dt, orig ,date) %>%
  mutate(prop = cnt / cnt.h) %>%
  select(community,org_dt, prop) %>%
  spread(org_dt,prop) %>%
  set_names(nm = c('community', paste('prop.for', c('new.entry', paste('entry',c('90s', '80s', 'prev.80s'), sep='.' ) )  ,sep='.' ),  paste('prop.nat', c('new.entry', paste('entry',c('90s', '80s', 'prev.80s'), sep='.' ) )  ,sep='.' ) ) 
              )
#################################################

# 3) Put everything together in a dataset
census.data <- list(
  agesex, education, income, house.props, 
  owner, prop.enter, prop.origin, 
  prop.year, race, work.prop, 
  dt.list$incomePC)  %>% 
  reduce(inner_join, by = "community") %>% 
  rename( QSB = community)

