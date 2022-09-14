#####################################
### load packages, function, data ###
#####################################
{
require(piecewiseSEM) #only needed for R^2-values
require(DHARMa)

pvals = function(t, df) return(2*pt(abs(t), df = df, lower.tail = FALSE))
scale3 = function(x, N) {x = x - median(x, na.rm = TRUE); x = x / sd(x, na.rm = TRUE); if(length(x) < N) x = NA; return(x)};
median_n = function(x, N) { if(length(na.omit(x)) >= N) return(median(x, na.rm = TRUE)) else return(NA)}
test_modelDHARMa = function(m) {
  simulationOutput <- simulateResiduals(fittedModel = m, plot = F)
  plot(simulationOutput)
}

data(dataset) #name of raw dataset: x
}




#####################################################################################
### summarize raw data for modeling #################################################
### the raw dataset (x) contains each day where a male was recorded sleeping ########
### the final dataset (x2) contains 1 datapoint per male and year ###################
### DATA SCALED FOR MODELS!!! #######################################################
#####################################################################################
{
#subset data to the month before the first egg
x = subset(x, rel_day %in% -30 : -1)

#define minimum group size
N = 3
#define which variables to scale by: "day to first egg" and "date"
scaleBy = c("rel_day", "date_")


#MEDIAN-CENTER AND SCALE ACROSS MALES
#calculate start of activity relative to other birds recorded on the same date and days-to-first-egg on those day where a datapoint exists (subset for evening, because some individuals have a morning emergence without an evening entry)
x[, z_time_to_sunrise_min := as.numeric(scale3(as.ITime(out_),N)), by = scaleBy]
x[as.IDate(in_) == as.IDate(out_)-1, z_time_to_sunset_min := as.numeric(scale3(as.ITime(in_),N)), by = scaleBy]
x[as.IDate(in_) == as.IDate(out_)-1, z_active := as.numeric(scale3(as.ITime(in_)-as.ITime(out_),N)), by = scaleBy]

#MEDIAN-CENTER AND SCALE WITHIN MALE
x[, start := as.numeric(median_n(z_time_to_sunrise_min, N)), by = .(season, ID)]
x[, end := as.numeric(median_n(z_time_to_sunset_min, N)), by = .(season, ID)]
x[, active := as.numeric(median_n(z_active, N)), by = .(season, ID)]
x[, season_age := paste(season, age, sep = "_")]


#create summarized dataset
x2 = copy(x)
x2[, ":=" (date_ = NULL, in_ = NULL, out_ = NULL, in_duration = NULL, out_duration = NULL, rel_day = NULL, z_time_to_sunset_min = NULL, z_time_to_sunrise_min = NULL, z_active = NULL)]
x2 = unique(x2)


#SCALE BY DEVIDING BY 2 TIMES THE STANDARD DEVIATION TO ALLOW EFFECT SIZE COMPARISON BETWEEN CONTINUOUS AND FACTOR, following Gelman 2008
x2[, start := start/(2*sd(start, na.rm = TRUE))]
x2[, end := end/(2*sd(end, na.rm = TRUE))]
x2[, active := active/(2*sd(active, na.rm = TRUE))]

#Sample sizes#####
#number of final data points
nrow(x2[!is.na(start),])
nrow(x2[!is.na(end), ])

#number of data points per individual within season
tmp = x[, .(ID, season)]
tmp[, N := .N, by = .(ID, season)]
tmp = unique(tmp)
min(tmp[,N]); max(tmp[,N]); mean(tmp[,N]); median(tmp[,N]) #3-28, 11.63, 10

#number of individuals in overall data set
length(unique(x[, ID]))
length(unique(x[!is.na(end), ID]))

#age classes
table(unique(x2[, .(ID, age)])[, .(age)])
table(unique(x2[!is.na(end), .(ID, age)])[, .(age)])

#number of seasons
table(x2[, season])

#number of individuals per season
table(unique(x2[, .(ID, season)])$season)
length(unique(x2[, ID]))
length(unique(x2[, season]))

#proportion of yearling males
table(unique(x2[, .(ID, season, age)])[, .(season, age)])

}




#########################################################
### run models ##########################################
### after selecting the relevant variable ###############

### ABBREVIATIONS: ######################################
### VAR1: extra-pair paternity gains: "gain"; VAR1 ######
### VAR1: extra-pair paternity losses: "loss"; VAR1 #####

### VAR2: start of activity: "sr" (=sunrise) ############
### VAR2: end of activity: "ss" (=sunset) ###############
### VAR2: time spent active: "act" ######################

### COMMENTS: ############################################
### In case of convergence problems compare estimates ###
### to models without random slope "season" #############
#########################################################
{
x2[, VAR1 := EPP_gainYN];x2[, VAR2 := start]; VAR = "gain_sr"
x2[, VAR1 := EPP_gainYN];x2[, VAR2 := end]; VAR = "gain_ss"
x2[, VAR1 := EPP_gainYN];x2[, VAR2 := active]; VAR = "gain_act"

x2[, VAR1 := EPP_lossYN];x2[, VAR2 := start]; VAR = "loss_sr"
x2[, VAR1 := EPP_lossYN];x2[, VAR2 := end]; VAR = "loss_ss"
x2[, VAR1 := EPP_lossYN];x2[, VAR2 := active]; VAR = "loss_act"

#model 1: age * timing (interaction)
m = glmer(VAR1 ~ factor(age, levels = c(2,1))*(VAR2) + (1|ID) + (1|season), data = x2, family = "binomial")
test_modelDHARMa(m)
summary(m)
AIC(m)

#model 2: age + timing (no interaction)
m = glmer(VAR1 ~ factor(age, levels = c(2,1))+VAR2 + (1|ID) + (1|season), data = x2, family = "binomial")
test_modelDHARMa(m)
summary(m)
nrow(m@frame)
rsquared(m)
AIC(m)

#model 3.0: timing (no age)
m = glmer(VAR1 ~ VAR2 + (1|ID) + (1|season), data = subset(x2, !is.na(age)), family = "binomial")
test_modelDHARMa(m)
summary(m)
rsquared(m)
AIC(m)

#model 3.1: timing within age class
m = glm(VAR1 ~ VAR2, data = subset(x2, age == 1), family = "binomial")
test_modelDHARMa(m)
summary(m)
m = glmer(VAR1 ~ VAR2 + (1|season), data = subset(x2, age == 2), family = "binomial")
test_modelDHARMa(m)
summary(m)

#model 4.1: timing and EPP females
m = lmer(VAR2 ~ EP_females + (1|ID), data = subset(x2, EP_females > 0 & age == 2))
test_modelDHARMa(m)
summary(m)
pvals(-1.64, 89)

#model 4.2: timing and number of EPY
m = lmer(VAR2 ~ EPP_gain + (1|ID), data = subset(x2, age == 2 & EPP_gain > 0))
test_modelDHARMa(m)
summary(m)
pvals(-1.06, 89)

#model 5: EPP predicted by age
m = glmer(VAR1 ~ factor(age, levels = c(2,1)) + (1|ID) + (1|season), data = subset(x2, !is.na(VAR2)), family = "binomial")
test_modelDHARMa(m)
summary(m)
AIC(m)
rsquared(m)

#model 6: timing predicted by age
m = lmer(VAR2 ~ factor(age, levels = c(2,1)) + (1|ID), data = x2)
test_modelDHARMa(m)
summary(m)
pvals(3.34, 205)

#model 7: start predicted by end (because the data are collected such that the evening entry takes place before the corresponding morning exit)
m = lmer(start ~ end + (1|ID), data = x2)
test_modelDHARMa(m)
summary(m)
pvals(-0.12, 165)
}


