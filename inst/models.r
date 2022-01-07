require(scales)
require(piecewiseSEM)
library(readr)
pvals = function(t, df) return(2*pt(abs(t), df = df, lower.tail = FALSE))

#0. load data #####
data(dataset)
x
full_data <- copy(x)

#models main text #####
#create dataset #####
#subset to the month before the first egg
x = subset(x, rel_day %in% -30 : -1) #-30 : -1

#use minimum group size of 3
N = 3
{ #create final dataset that has one row per individual per year

#use scaleby rel_day and date, because the data gets a lot more reliable
scaleBy = c("rel_day", "date_")

#calculate start of activity relative to other birds recorded on the same date and days-to-first-egg
x[, z_time_to_sunrise_min := as.numeric(scale3(as.ITime(out_),N)), by = scaleBy]
x[as.IDate(in_) == as.IDate(out_)-1, z_time_to_sunset_min := as.numeric(scale3(as.ITime(in_),N)), by = scaleBy] #only calculate for those data points where the evening entry was actually recorded.
x[as.IDate(in_) == as.IDate(out_)-1, z_active := as.numeric(scale3(as.ITime(in_)-as.ITime(out_),N)), by = scaleBy] #only calculate for those data points where the evening entry was actually recorded.

#it doesn't matter to use the in_time instead of time to sunrise, because it is scaled within date anyway.
#remove all data points where the previous could not be calculated
x = subset(x, !is.na(z_time_to_sunrise_min))

#create individual measurements by taking the median over at least 3 data points for all individuals within one season
x[, start := as.numeric(median_n(z_time_to_sunrise_min, N)), by = .(season, ID)]
x[, end := as.numeric(median_n(z_time_to_sunset_min, N)), by = .(season, ID)]
x[, active := as.numeric(median_n(z_active, N)), by = .(season, ID)]
x[, season_age := paste(season, age, sep = "_")]

#remove missing data
x = subset(x, !is.na(start))

#keep full data set for calculations lateron. Make new dataset where unnecessary and duplicated information is removed.
x2 = copy(x)
x2[, ":=" (date_ = NULL, in_ = NULL, out_ = NULL, in_duration = NULL, out_duration = NULL, rel_day = NULL, z_time_to_sunset_min = NULL, z_time_to_sunrise_min = NULL, z_active = NULL)]
x2 = unique(x2)

x2[, start := start/(2*sd(start, na.rm = TRUE))] #following Gelman 2008
x2[, end := end/(2*sd(end, na.rm = TRUE))] #following Gelman 2008
x2[, active := active/(2*sd(active, na.rm = TRUE))] #following Gelman 2008


#if you remove data from 2010-2012 because there are less than 11 data points per year (2013 onwards: more than 20 data points each year), the results don't change; results not shown.
#x2 = subset(x2, season > 2014)
#x = subset(x, season > 2014)
#Sample sizes#####
#number of final data points
nrow(x2) #296
nrow(x2[!is.na(end), ]) #235

#number of data points per individual within season
tmp = x[, .(ID, season)]
tmp[, N := .N, by = .(ID, season)]
tmp = unique(tmp)
min(tmp[,N]); max(tmp[,N]); mean(tmp[,N]); median(tmp[,N]) #3-28, 11.63, 10

#number of individuals in overall data set
length(unique(x[, ID])) #205
length(unique(x[!is.na(end), ID])) #205

#age classes
table(unique(x2[, .(ID, age)])[, .(age)])
table(unique(x2[!is.na(end), .(ID, age)])[, .(age)])

#number of seasons
table(x2[, season])


#number of individuals per season
table(unique(x[, .(ID, season)])$season) #2, 5, 13, 15, 39, 43, 33, 66, 41, 39
nrow(x2)
length(unique(x2[, ID]))
length(unique(x2[, season]))
table(unique(x2[, .(ID, season)])$season)

#proportion of yearling males
table(unique(x2[, .(ID, season, age)])[, .(season, age)])
#2011-2014: 5 yealings, 30 adults
5/35
#>2014:
16+20+10+17+14+10 #87 yearlings
23+23+23+49+27+29 #174 adults
87/(174+87) #0.33
}

#models #####
#the next lines set the variables: VAR1 is extra-pair gain or loss, VAR2 is start, end or total time of activity. Run for each combination of VAR1 and VAR2 by selecting one of the rows below and then running the models in the upcoming paragraph
x2[, VAR1 := EPP_gainYN];x2[, VAR2 := start]; VAR = "gain_sr"
x2[, VAR1 := EPP_lossYN];x2[, VAR2 := start]; VAR = "loss_sr"

x2[, VAR1 := EPP_gainYN];x2[, VAR2 := active]; VAR = "gain_act"
x2[, VAR1 := EPP_lossYN];x2[, VAR2 := active]; VAR = "loss_act"

x2[, VAR1 := EPP_gainYN];x2[, VAR2 := end]; VAR = "gain_ss"
x2[, VAR1 := EPP_lossYN];x2[, VAR2 := end]; VAR = "loss_ss"


#all three with interaction
m = glmer(VAR1 ~ factor(age, levels = c(2,1))*(VAR2) + (1|ID) + (1|season), data = x2, family = "binomial")
#m = glmer(VAR1 ~ factor(age, levels = c(2,1))*VAR2 + (1|ID), data = x2, family = "binomial") #run to verify estimates in case of convergence problems.
summary(m)

#all three without interaction
m = glmer(VAR1 ~ factor(age, levels = c(2,1))+VAR2 + (1|ID) + (1|season), data = x2, family = "binomial")
#m = glmer(VAR1 ~ factor(age, levels = c(2,1))+VAR2 + (1|ID), data = x2, family = "binomial") #run to verify in case of convergence problems.
summary(m)
nrow(m@frame)
rsquared(m)
##

#timing of activity and EPP
m = glmer(VAR1 ~ VAR2 + (1|ID) + (1|season), data = x2, family = "binomial")
#m = glmer(VAR1 ~ VAR2 + (1|ID), data = x2, family = "binomial")  #run to verify in case of convergence problems.
summary(m)
rsquared(m)

#start and EPP within age class
m = glm(VAR1 ~ VAR2, data = subset(x2, age == 1), family = "binomial")
m = glmer(VAR1 ~ VAR2 + (1|season), data = subset(x2, age == 2), family = "binomial")
summary(m)

#start and EPP females
m = lmer(VAR2 ~ EP_females + (1|ID), data = subset(x2, EP_females > 0 & age == 2))
summary(m)

#age and EPP
m = glmer(VAR1 ~ factor(age, levels = c(2,1)) + (1|ID) + (1|season), data = x2, family = "binomial")
#m = glmer(VAR1 ~ factor(age, levels = c(2,1)) + (1|ID), data = x2, family = "binomial")
summary(m)
rsquared(m)

#age and timing
m = lmer(VAR2 ~ factor(age, levels = c(2,1)) + (1|ID), data = x2)
hist(resid(m))
qqnorm(resid(m))
summary(m)

#start and end
m = lmer(start ~ end + (1|ID), data = x2)
hist(resid(m))
qqnorm(resid(m))
summary(m)
pvals(-0.12, 165)

#Figures #####
#data for figure #####
for(i in c("gain_sr", "gain_ss", "gain_act")) {
  print(i)
  if(i == "gain_sr") { x2[, VAR1 := EPP_gainYN];x2[, VAR2 := start] }
  if(i == "gain_ss") { x2[, VAR1 := EPP_gainYN];x2[, VAR2 := end] }
  if(i == "gain_act") { x2[, VAR1 := EPP_gainYN];x2[, VAR2 := active] }


require(boot)
x2N = subset(x2, !is.na(VAR2) & !is.na(VAR1))
newdata <- data.table(with(x2N, expand.grid(VAR2=seq(-3, 3, length.out=200), ID=unique(ID), season = unique(season))))

# ala Knowles and Frederick 2016, see https://stats.stackexchange.com/questions/344012/confidence-intervals-from-bootmer-in-r-and-pros-cons-of-different-interval-type
#CI.lower = apply(merBoot$t, 2, function(x) as.numeric(quantile(x, probs=.025, na.rm=TRUE)))
#CI.upper = apply(merBoot$t, 2, function(x) as.numeric(quantile(x, probs=.975, na.rm=TRUE)))
#

N = 1000
#no need for ID for yearlings, because there are no duplicates by necessity
gm1 = glmer(VAR1 ~ VAR2 + (1|season), data = subset(x2N, age == 1), family = "binomial") #for loss there is a warning, but the model output is ok compared with the corresponding glm
gm2 = glmer(VAR1 ~ VAR2 + (1|ID) + (1|season), data = subset(x2N, age == 2), family = "binomial")
newdata[,pred1:=predict(gm1,newdata=newdata,re.form=~0)]
newdata[,pred2:=predict(gm2,newdata=newdata,re.form=~0)]

merBoot<-bootMer(gm1,FUN=function(.) predict(.,newdata=newdata,re.form=~0),nsim=N)
newdata[, CI.lower1 := apply(merBoot$t, 2, function(x) as.numeric(quantile(x, probs=.025, na.rm=TRUE)))]
newdata[, CI.upper1 := apply(merBoot$t, 2, function(x) as.numeric(quantile(x, probs=.975, na.rm=TRUE)))]

merBoot<-bootMer(gm2,FUN=function(.) predict(.,newdata=newdata,re.form=~0),nsim=N)
newdata[, CI.lower2 := apply(merBoot$t, 2, function(x) as.numeric(quantile(x, probs=.025, na.rm=TRUE)))]
newdata[, CI.upper2 := apply(merBoot$t, 2, function(x) as.numeric(quantile(x, probs=.975, na.rm=TRUE)))]

if(i == "gain_sr") copy(newdata) -> dat_gain_sr
if(i == "gain_ss") copy(newdata) -> dat_gain_ss
if(i == "gain_act") copy(newdata) -> dat_gain_act
}

#make figure
#AD_COL = "#5000FFFF"
#Y_COL = "#FF758AFF"
#alternative colour pair:
AD_COL = "blue"
Y_COL = "red"

dev.off()
{
jpeg("Figure1.jpg", width = 600*2, height = 600*6, quality = 100)
par(mar = c(4.1, 4.1, 0.1, 0.1))
par(mfrow = c(3,1))
par(mgp = c(2,0.8,0))
par(cex = 5)


for(VAR in c("gain_sr", "gain_ss","gain_act")) {
  rm(newdata)
  if(VAR == "gain_sr") {
    x2[, VAR1 := EPP_gainYN]
    x2[, VAR2 := start]
    newdata = copy(dat_gain_sr)
    XLIM = c(min(x2[, VAR2], na.rm = TRUE)-0.1, max(x2[, VAR2], na.rm = TRUE)+0.1)
    XLIM = c(-2, 2)
    par(las = 0)
    plot(c(-4.6, 4.2), c(-0.2, 1.0), xlim = XLIM, type = "n", xlab = "Start of activity", ylab = "Probability of siring EPO", yaxt = 'n', xaxt = "n")
    axis(1, at = -4:4, labels = -4:4)
    axis(2, at = c(0, 0.2, 0.4, 0.6, 0.8, 1.0), labels = c("0.0", "0.2", "0.4", "0.6", "0.8", "1.0"))
    par(las = 0)
    mtext("(a)", cex = 6, side = 3, line = -0.8, adj = -0.3)
  }
  if(VAR == "gain_ss") {
    x2[, VAR1 := EPP_gainYN]
    x2[, VAR2 := end]
    newdata = copy(dat_gain_ss)
    XLIM = c(min(x2[, VAR2], na.rm = TRUE)-0.1, max(x2[, VAR2], na.rm = TRUE)+0.1)
    XLIM = c(-2, 2)
    par(las = 0)
    plot(c(-4.6, 4.2), c(-0.2, 1.0), xlim = XLIM, type = "n", xlab = "End of activity", ylab = "Probability of siring EPO", yaxt = 'n', xaxt = "n")
    axis(1, at = -4:4, labels = -4:4)
    axis(2, at = c(0, 0.2, 0.4, 0.6, 0.8, 1.0), labels = c("0.0", "0.2", "0.4", "0.6", "0.8", "1.0"))
    par(las = 0)
    mtext("(b)", cex = 6, side = 3, line = -0.8, adj = -0.3)

  }
  if(VAR == "gain_act") {
    x2[, VAR1 := EPP_gainYN]
    x2[, VAR2 := active]
    newdata = copy(dat_gain_act)
    XLIM = c(min(x2[, VAR2], na.rm = TRUE)-0.1, max(x2[, VAR2], na.rm = TRUE)+0.1)
    XLIM = c(-2, 2)
    par(las = 0)
    plot(c(-4.6, 4.2), c(-0.2, 1.0), xlim = XLIM, type = "n", xlab = "Active period", ylab = "Probability of siring EPO", yaxt = 'n', xaxt = "n")
    axis(1, at = -4:4, labels = -4:4)
    axis(2, at = c(0, 0.2, 0.4, 0.6, 0.8, 1.0), labels = c("0.0", "0.2", "0.4", "0.6", "0.8", "1.0"))
   par(las = 0)
    mtext("(c)", cex = 6, side = 3, line = -0.8, adj = -0.3)

  }
  setorder(newdata, VAR2)

{
  #histograms for barplot part of figure: calculate points
  #BREAKS = c(1:80/5 - 1.8)

  BREAKS = c(1:80/5 - 3)
  a = hist(x2[age == 1 & VAR1 == 0, VAR2], plot = FALSE, breaks = BREAKS)
  w = (a$breaks[2] -a$breaks[1])/2
  a2 = hist(x2[age == 2 & VAR1 == 0, VAR2], plot = FALSE, breaks = BREAKS)
  w2 = (a2$breaks[2] -a2$breaks[1])/2
  a3 = hist(x2[age == 1 & VAR1 == 1, VAR2], plot = FALSE, breaks = BREAKS)
  w3 = (a3$breaks[2] -a3$breaks[1])/2
  a4 = hist(x2[age == 2 & VAR1 == 1, VAR2], plot = FALSE, breaks = BREAKS)
  w4 = (a4$breaks[2] -a4$breaks[1])/2

  #table for barplot part for figure: rectangles
  dev_by = 150 #sets height of barplot
  FIG = rbind(data.table(xleft = a2$mids-w2, ybottom = -0.2, xright = a2$mids+w2, ytop = -0.2+a2$counts/dev_by, COL = AD_COL),
      data.table(xleft = a2$mids-w, ybottom = -0.2+a2$counts/dev_by, xright = a2$mids+w, ytop = -0.2+a2$counts/dev_by+a$counts/dev_by, COL = Y_COL),
       data.table(xleft = a4$mids-w4, ybottom = 1, xright = a4$mids+w4, ytop = 1-a4$counts/dev_by, COL = AD_COL),
       data.table(xleft = a4$mids-w3, ybottom = 1-a4$counts/dev_by, xright = a4$mids+w3, ytop = 1-a4$counts/dev_by-a3$counts/dev_by, COL = Y_COL))
  FIG = FIG[ytop != ybottom,] #remove boxes of height zero (no data)

  #draw rectangles
  rect(xleft = FIG[, xleft], ybottom = FIG[, ybottom], xright = FIG[, xright], ytop = FIG[, ytop], col = alpha(FIG[, COL], 0.5))
  #axis(4, at = (-2:2)/10, labels = (0:4)/10*dev_by, line = -1.5, las = 0, cex.axis = 0.8, mgp = c(0, 0.4, 0))
  #axis(4, at = (6:10)/10, labels = (4:0)/10*dev_by, line = -1.5, las = 0, cex.axis = 0.8, mgp = c(0, 0.4, 0))

  SUBSET = c(min(FIG[, xleft], na.rm = TRUE), max(FIG[, xright], na.rm = TRUE))
  polygon(c(newdata[VAR2 >= SUBSET[1] & VAR2 <= SUBSET[2], VAR2], rev(newdata[VAR2 >= SUBSET[1] & VAR2 <= SUBSET[2], VAR2])), c(binomial()$linkinv(newdata[VAR2 >= SUBSET[1] & VAR2 <= SUBSET[2], CI.lower1]), rev(binomial()$linkinv(newdata[VAR2 >= SUBSET[1] & VAR2 <= SUBSET[2], CI.upper1]))), col = alpha(Y_COL, 0.25), border = NA)
  points(newdata[VAR2 >= SUBSET[1] & VAR2 <= SUBSET[2], VAR2], binomial()$linkinv(newdata[VAR2 >= SUBSET[1] & VAR2 <= SUBSET[2], pred1]), type = "l", lwd = 2, col = Y_COL)

  polygon(c(newdata[VAR2 >= SUBSET[1] & VAR2 <= SUBSET[2], VAR2], rev(newdata[VAR2 >= SUBSET[1] & VAR2 <= SUBSET[2], VAR2])), c(binomial()$linkinv(newdata[VAR2 >= SUBSET[1] & VAR2 <= SUBSET[2], CI.lower2]), rev(binomial()$linkinv(newdata[VAR2 >= SUBSET[1] & VAR2 <= SUBSET[2], CI.upper2]))), col = alpha(AD_COL, 0.25), border = NA)
  points(newdata[VAR2 >= SUBSET[1] & VAR2 <= SUBSET[2], VAR2], binomial()$linkinv(newdata[VAR2 >= SUBSET[1] & VAR2 <= SUBSET[2], pred2]), type = "l", lwd = 2, col = AD_COL)
}
}
dev.off()
}
