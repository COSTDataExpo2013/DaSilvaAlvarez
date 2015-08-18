# Data contain on 'sotc.csv' file were pre-processed by Xiaoque Cheng
# with mergeGUI.
soul <- read.csv("data/sotc.csv", header = T)

# Variable sets
orig <- c(4:6, 8:61, 148:152, 165:175, 181:206, 210:213)
demo <- c(7, 62:79, 153:154, 158:160, 206:209, 216, 229)
reco <- c(80:120, 141:147, 161, 177:179, 217:228)
cons <- c(121:135, 155:157, 162:164, 176, 180)

# recode CCEGRP
soul$CCEGRP2 <- recode(soul$CCEGRP, "'Engaged'='Attached';'Not Engaged'='Not Attached'")

# Compute attachment proportion for ordering QSB levels
wt <- soul %>%
        filter(!is.na(WEIGHT)) %>%
        transmute(wt = WEIGHT/3)  

des.wt <- svydesign(ids = ~1, strata = ~QSB, weights = ~as.numeric(wt2[,1]),
      data = subset(soul,  !is.na(WEIGHT))
    )

att.pr <- svyby(~CCEGRP2, ~QSB, des.wt, svymean, keep.var = TRUE, na.rm = T)

soul$QSB2 <- soul$QSB

soul <- soul%>%
        left_join(att.pr[, 1:2], by = "QSB")

soul$QSB <- with(soul, reorder(QSB2, CCEGRP2Attached))
soul$CCEGRP2Attached <- NULL

# Reduction of the data set to avoid missing value imputation.
miss <- is.na(soul)
aux <- apply(miss, 2, mean)

# select variables with less than 25 % of missing values
var.red <- names(aux[aux < 0.25])
aux <- apply(miss[, var.red], 1, sum)
aux1 <- data.frame(table(aux))
soul.red <- soul[aux < 3, var.red]
orig.red <- colnames(soul.red)[c(4, 5, 7, 8:50)]

# age and proportion of age in place where live.
soul <- mutate(soul, 
  QD1gr = cut(QD1, breaks = c(0, 30, 40, 50, 60, 70, 80, 99), include.lowest = T),
  QD2gr = cut(QD2, breaks = c(0, 10, 20, 50, 99), include.lowest = T), prop.res = QD2/QD1,
  prop.gr = cut(prop.res, breaks = c(0, 0.25, 0.5, 1, 5.5), include.lowest = T)
)

soul$prop.res[soul$QD1 == 0] <- NA
demo.red <- c(colnames(soul.red)[c(6, 51:62)], "QD1gr", "QD2gr", "prop.res", "prop.gr")

# Main reduction of data: Only variables with less than 25% of missing,
# and complete cases withion those vars.
aux <- apply(is.na(soul[, c(orig.red, demo.red, "CCE", "CCEGRP2")]), 1, sum)
condi1 <- aux == 0
soul.red <- soul[condi1, 
    c(orig.red, demo.red, "CCE", "CCEGRP2", "QSB", "YEAR", "WEIGHT", "PROJWT")]

# Recodification of several variables in the data, collapsing some
# levels, and considering NA some of the responses (like, DK, not
# aplicable, non-response, etc).
reco.fun <- function(x) {
  n <- length(x)
  y <- numeric(n)
  if ("2" %in% levels(x)) {
    y[x %in% c("(DK)", "(Refused)", "(Not applicable)", "(Have not lived in area for five years)")] <- NA
    y[x %in% c("Very bad", "Strongly disagree", "Extremely low", "Not at all safe", 
               "Not at all satisfied", "Not at all likely", "Much worse", 
               "Will be much worse")] <- 1
    y[x %in% c("Very good", "Strongly agree", "Extremely high", "Completely safe", 
               "Extremely satisfied", "Extremely likely", "Much better", "Will be much better")] <- 5
    y[x == "2"] <- 2
    y[x == "3"] <- 3
    y[x == "4"] <- 4
  }
  if ("Yes" %in% levels(x)) {
    y[x %in% c("(DK)", "(Refused)", "(Not applicable)")] <- NA
    y[x == "Yes"] <- 1
    y[x == "No"] <- 0
  }
  if ("A few" %in% levels(x)) {
    y[x %in% c("(DK)", "(Refused)", "(Not applicable)")] <- NA
    y[x == "None"] <- 1
    y[x == "A few"] <- 2
    y[x == "Some"] <- 3
    y[x == "About half"] <- 4
    y[x %in% c("Most", "Most, OR")] <- 5
    y[x == "All or nearly all"] <- 6
  }
  if ("Never" %in% levels(x)) {
    y[x %in% c("(DK)", "(Refused)", "(Not applicable)")] <- NA
    y[x %in% c("Never", "NeverNunca")] <- 1
    y[x == "Once a year or less"] <- 2
    y[x %in% c("Once a month", "Several times a year")] <- 3
    y[x == "Once a month"] <- 4
    y[x %in% c("Several times a week", "About every day")] <- 6
    y[x == "Several times a month"] <- 5
  }
  if ("Decreased" %in% levels(x)) {
    y[x %in% c("(DK)", "(Refused)", "(Not applicable)")] <- NA
    y[x == "Decreased"] <- 1
    y[x == "Stayed the same"] <- 2
    y[x == "Increased"] <- 3
  }
  if ("Getting better" %in% levels(x)) {
    y[x %in% c("(DK)", "(Refused)", "(Not applicable)")] <- NA
    y[x == "Getting worse"] <- 1
    y[x == "Getting better"] <- 3
    y[x == "(Same)"] <- 2
  }
  if (is.numeric(x)) 
    y <- x
  return(y)
}

reco.aux <- NULL
for (i in 1:length(orig.red)) reco.aux <- rbind(reco.aux, reco.fun(soul.red[, i]))
reco.aux <- t(reco.aux)[, -32]
colnames(reco.aux) <- orig.red[-32]

# More recodification, now for demographic variables in the data.
reco.demo <- soul.red[, c(demo.red, "Q11")]
reco.demo <- mutate(reco.demo, Q11 = factor(recode(Q11, "'(Disabled/unable to work)' = 'Other '; '(Other) (do not list)'='Other '\n; 'Disabled/unable to work'='Other '; 'Temporarily laid off'='Other '\n; 'Unemployed and not looking for work'='Other '; '(DK)'='Non Response';'\n (Refused)'='Non Response' ")), 
     QD4 = factor(recode(QD4, "'(DK)' = 'Non Response' ;'(Refused)' = 'Non Response'")),
     QD6 = factor(recode(QD6, "c('Separated', 'Widowed', 'Separated, OR') = 'Divorced'; 'Never been married'\n = 'Single'; 'Living in a partnered relationship' = 'Married'; 'Now married' = 'Married'\n; '(DK)' = 'Non Response'; '(Refused)' = 'Non Response'")), 
     QD7 = factor(recode(QD7, "'Grade school or less' = 'Less HS'; 'Some high school' = 'Less HS';'High school \n graduate' = 'HS'; '(DK)' = 'Non Response'; '(Refused)' = 'Non Response'; c('Some college or \n technical school', 'College graduate', 'College graduate, OR') = 'Collegue';'Post\n-graduate work or degree'='MS'")), 
     QD8 = factor(recode(QD8, "'Other (rent a room, live as a lodger, squatter, etc.)' = 'Other';'(DK)'='Non \n Response';'(Refused)' = 'Non Response'")), 
     QD9 = factor(recode(QD9, "c('Under $15,000','$15,000 to $24,999','$25,000 to $34,999') = 'Under 35k'; c\n ('$35,000 to $44,999','$45,000 to $54,999', '$55,000 to $74,999') = '35k to 75k'; c('$75\n , 000 to $99,999','$100,000 or over') = 'Over 75k'"))
  )

# QD10 ask for hispanic or not hispanic, QD111 ask for race, we combine
# both in QD111
aux <- reco.demo$QD111
levels(aux)[c(4, 6:10, 15:17, 19, 20)] <- "Other"
levels(aux)[c(1, 2, 5, 7, 8, 10)] <- "Non Response"
levels(aux)[2] <- "Hispanic"
aux[reco.demo$QD10 == "Yes"] <- "Hispanic"
reco.demo$QD111 <- factor(aux)

# new soul.red data with recode variables and no missing
soul.red2 <- data.frame(reco.aux,
      reco.demo, soul.red[, c("CCE", "YEAR", "CCEGRP2", "QSB", "WEIGHT", "PROJWT")])
soul.red2$Q23[soul.red2$Q23 == 9] <- NA

# the recodification creates a few new missing values that we don't
# want.
aux <- apply(is.na(soul.red2), 1, sum)
condi2 <- aux == 0
soul.red2 <- soul.red2[condi2, ]