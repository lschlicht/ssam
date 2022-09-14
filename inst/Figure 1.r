#####################################
### load packages, function, data ###
#####################################
{
  require(scales) #needed for transparency, function "alpha"


  pvals = function(t, df) return(2*pt(abs(t), df = df, lower.tail = FALSE))
  scale4 = function(x, N) {x = x - median(x, na.rm = TRUE); if(length(x) < N) x = NA; return(x)}
  median_n = function(x, N) { if(length(na.omit(x)) >= N) return(median(x, na.rm = TRUE)) else return(NA)}
  test_modelDHARMa = function(m) {
    simulationOutput <- simulateResiduals(fittedModel = m, plot = F)
    plot(simulationOutput)
  }

  data(dataset) #name of raw dataset: x
}




########################################################################################
### summarize raw data for modeling ####################################################
### the raw dataset (x) contains each day where a male was recorded sleeping ###########
### the final dataset (x_fig) contains 1 datapoint per male and year ###################
### DATA NOT SCALED FOR THE FIGURE!!! ##################################################
########################################################################################
{
  #subset data to the month before the first egg
  x = subset(x, rel_day %in% -30 : -1)

  #define minimum group size
  N = 3
  #define which variables to scale by: "day to first egg" and "date"
  scaleBy = c("rel_day", "date_")

  #remove evening in for those datapoints where the entry was defined by the previous event (Note: date of "in" equals date of "out", this event is the "morning out" only, the "in" is not correct. This was checked during dataset extraction.) Also remove datapoints where the "in_" is too early (to be on the safe side, using outliers from boxplot)
  #don't forget to add to stats script
  x[as.IDate(in_) == as.IDate(out_), in_ := NA]
  errors = boxplot(as.numeric(as.ITime(as.POSIXct(x$in_))), plot = FALSE)$stats
  x[as.ITime(as.POSIXct(in_)) <= errors[1] | as.ITime(as.POSIXct(in_)) >= errors[5], in_ := NA]

  #MEDIAN-CENTER AND SCALE ACROSS MALES
  #calculate start of activity relative to other birds recorded on the same date and days-to-first-egg on those day where a datapoint exists (subset for evening, because some individuals have a morning emergence without an evening entry)
  x[, z_time_to_sunrise_min := as.numeric(scale4(as.ITime(out_),N)), by = scaleBy]
  x[as.IDate(in_) == as.IDate(out_)-1, z_time_to_sunset_min := as.numeric(scale4(as.ITime(in_),N)), by = scaleBy]
  x[as.IDate(in_) == as.IDate(out_)-1, z_active := as.numeric(scale4(as.ITime(in_)-as.ITime(out_),N)), by = scaleBy]

  #MEDIAN-CENTER AND SCALE WITHIN MALE
  x[, start := as.numeric(median_n(z_time_to_sunrise_min, N)), by = .(season, ID)]
  x[, end := as.numeric(median_n(z_time_to_sunset_min, N)), by = .(season, ID)]
  x[, active := as.numeric(median_n(z_active, N)), by = .(season, ID)]
  x[, season_age := paste(season, age, sep = "_")]


  #create summarized dataset
  x_fig = copy(x)
  x_fig[, ":=" (date_ = NULL, in_ = NULL, out_ = NULL, in_duration = NULL, out_duration = NULL, rel_day = NULL, z_time_to_sunset_min = NULL, z_time_to_sunrise_min = NULL, z_active = NULL)]
  x_fig = unique(x_fig)
}


########################################################
### Create dataset for figure: #########################
### !!!COMPUTING TIME: 30 minutes on our machine !!! ###
########################################################

for(i in c("gain_sr", "gain_ss", "gain_act")) {
  print(i)
  if(i == "gain_sr") { x_fig[, VAR1 := EPP_gainYN];x_fig[, VAR2 := start] }
  if(i == "gain_ss") { x_fig[, VAR1 := EPP_gainYN];x_fig[, VAR2 := end] }
  if(i == "gain_act") { x_fig[, VAR1 := EPP_gainYN];x_fig[, VAR2 := active] }


  x_figN = subset(x_fig, !is.na(VAR2) & !is.na(VAR1))
  x_figN[, VAR2 := VAR2 / 60]
  newdata <- data.table(with(x_figN, expand.grid(VAR2=seq(-30, 30, length.out=300), ID=unique(ID), season = unique(season))))

  N = 1000
  #no need for ID for yearlings, because there are no duplicates by necessity
  gm1 = glmer(VAR1 ~ VAR2 + (1|season), data = subset(x_figN, age == 1), family = "binomial") #for loss there is a warning, but the model output is ok compared with the corresponding glm
  gm2 = glmer(VAR1 ~ VAR2 + (1|ID) + (1|season), data = subset(x_figN, age == 2), family = "binomial")
  newdata[,pred1:=predict(gm1,newdata=newdata,re.form=~0)]
  newdata[,pred2:=predict(gm2,newdata=newdata,re.form=~0)]

  merBoot<-bootMer(gm1,FUN=function(.) predict(.,newdata=newdata,re.form=~0),nsim=N)
  newdata[, CI.lower1 := apply(merBoot$t, 2, function(x) as.numeric(quantile(x, probs=.025, na.rm=TRUE)))]
  newdata[, CI.upper1 := apply(merBoot$t, 2, function(x) as.numeric(quantile(x, probs=.975, na.rm=TRUE)))]

  merBoot<-bootMer(gm2,FUN=function(.) predict(.,newdata=newdata,re.form=~0),nsim=N)
  newdata[, CI.lower2 := apply(merBoot$t, 2, function(x) as.numeric(quantile(x, probs=.025, na.rm=TRUE)))]
  newdata[, CI.upper2 := apply(merBoot$t, 2, function(x) as.numeric(quantile(x, probs=.975, na.rm=TRUE)))]

  #save objects
  if(i == "gain_sr") copy(newdata) -> dat_gain_sr
  if(i == "gain_ss") copy(newdata) -> dat_gain_ss
  if(i == "gain_act") copy(newdata) -> dat_gain_act
}


########################################################
### Create figure: #####################################
########################################################
#define colours for yearlings ("Y_COL") and adults ("AD_COL")
AD_COL = "blue"
Y_COL = "red"

{
  jpeg(paste0(Sys.Date(), "_Figure1.jpg"), width = 600*6, height = 600*2, quality = 100)
  par(mar = c(4.1, 4.1, 0.2, 0.1))
  par(mfrow = c(1,3))
  par(mgp = c(2.2,0.8,0))
  par(cex = 5)

  #run loop to create three panels
  for(VAR in c("gain_sr", "gain_ss", "gain_act")) {
    print(VAR)
    rm(newdata)

    #define parameters for the three panels
    if(VAR == "gain_sr") {
      x_fig[, VAR1 := EPP_gainYN]
      x_fig[, VAR2 := start]
      newdata = copy(dat_gain_sr)
      #XLIM = c(min(x_fig[, VAR2], na.rm = TRUE)-0.1, max(x_fig[, VAR2], na.rm = TRUE)+0.1)
      XLIM = c(-20, 20)
      par(las = 1)
      plot(c(-4.6, 4.2), c(-0.2, 1.0), xlim = XLIM, type = "n", xlab = "Start of activity", ylab = "Probability of siring EPO", yaxt = 'n', xaxt = "n")
      axis(1, at = (-3:8)*10, labels = (-3:8)*10)
      #axis(1, at = -4:4, labels = -4:4)
      axis(2, at = c(0, 0.2, 0.4, 0.6, 0.8, 1.0), labels = c("0.0", "0.2", "0.4", "0.6", "0.8", "1.0"))
      par(las = 0)
      mtext("(a)", cex = 6, side = 3, line = -0.8, adj = -0.3)
      BREAKS = c(-17:50)*100/60

    }
    if(VAR == "gain_ss") {
      x_fig[, VAR1 := EPP_gainYN]
      x_fig[, VAR2 := end]
      newdata = copy(dat_gain_ss)
      XLIM = c(min(x_fig[, VAR2], na.rm = TRUE)-0.1, max(x_fig[, VAR2], na.rm = TRUE)+0.1)
      XLIM = c(-20, 20)
      par(las = 1)
      plot(c(-4.6, 4.2), c(-0.2, 1.0), xlim = XLIM, type = "n", xlab = "End of activity", ylab = "", yaxt = 'n', xaxt = "n")
      #axis(1, at = -4:4, labels = -4:4)
      axis(1, at = (-30:80)*10, labels = (-30:80)*10)
      #axis(2, at = c(0, 0.2, 0.4, 0.6, 0.8, 1.0), labels = c("0.0", "0.2", "0.4", "0.6", "0.8", "1.0"))
      axis(2, at = c(0, 0.2, 0.4, 0.6, 0.8, 1.0), labels = c("", "", "", "", "", ""))
      par(las = 1)
      mtext("(b)", cex = 6, side = 3, line = -0.8, adj = -0.3)
      BREAKS = (-162:900)*100/60

    }
    if(VAR == "gain_act") {
      x_fig[, VAR1 := EPP_gainYN]
      x_fig[, VAR2 := active]
      newdata = copy(dat_gain_act)
      XLIM = c(min(x_fig[, VAR2], na.rm = TRUE)-0.1, max(x_fig[, VAR2], na.rm = TRUE)+0.1)
      XLIM = c(-20, 20)
      par(las = 1)
      plot(c(-4.6, 4.2), c(-0.2, 1.0), xlim = XLIM, type = "n", xlab = "Active period", ylab = "", yaxt = 'n', xaxt = "n")
      #axis(1, at = -4:4, labels = -4:4)
      axis(1, at = (-3:8)*10, labels = (-3:8)*10)
      #axis(2, at = c(0, 0.2, 0.4, 0.6, 0.8, 1.0), labels = c("0.0", "0.2", "0.4", "0.6", "0.8", "1.0"))
      axis(2, at = c(0, 0.2, 0.4, 0.6, 0.8, 1.0), labels = c("", "", "", "", "", ""))
      par(las = 0)
      mtext("(c)", cex = 6, side = 3, line = -0.8, adj = -0.3)
      BREAKS = (-162:11)*100/60

    }
    setorder(newdata, VAR2)

    #calculate data for histograms
    dev_by = 150 #sets height of barplot

    a = hist(x_fig[age == 1 & VAR1 == 0, VAR2/60], plot = FALSE, breaks = BREAKS)
    w = (a$breaks[2] -a$breaks[1])/2
    a2 = hist(x_fig[age == 2 & VAR1 == 0, VAR2/60], plot = FALSE, breaks = BREAKS)
    w2 = (a2$breaks[2] -a2$breaks[1])/2
    a3 = hist(x_fig[age == 1 & VAR1 == 1, VAR2/60], plot = FALSE, breaks = BREAKS)
    w3 = (a3$breaks[2] -a3$breaks[1])/2
    a4 = hist(x_fig[age == 2 & VAR1 == 1, VAR2/60], plot = FALSE, breaks = BREAKS)
    w4 = (a4$breaks[2] -a4$breaks[1])/2

    FIG = rbind(data.table(xleft = a2$mids-w2, ybottom = -0.2, xright = a2$mids+w2, ytop = -0.2+a2$counts/dev_by, COL = AD_COL),
                  data.table(xleft = a2$mids-w, ybottom = -0.2+a2$counts/dev_by, xright = a2$mids+w, ytop = -0.2+a2$counts/dev_by+a$counts/dev_by, COL = Y_COL),
                  data.table(xleft = a4$mids-w4, ybottom = 1, xright = a4$mids+w4, ytop = 1-a4$counts/dev_by, COL = AD_COL),
                  data.table(xleft = a4$mids-w3, ybottom = 1-a4$counts/dev_by, xright = a4$mids+w3, ytop = 1-a4$counts/dev_by-a3$counts/dev_by, COL = Y_COL))
      FIG = FIG[ytop != ybottom,] #remove boxes of height zero (no data)

      #draw histogram
      rect(xleft = FIG[, xleft], ybottom = FIG[, ybottom], xright = FIG[, xright], ytop = FIG[, ytop], col = alpha(FIG[, COL], 0.5))



      #create and draw model output: line and confidence interval
      SUBSET = c(min(FIG[xleft >= min(XLIM)-0.1, xleft], na.rm = TRUE), max(FIG[xright <= max(XLIM)+0.1, xright], na.rm = TRUE)) #0.1 is a security because of rounding issues.

      #yearlings
      polygon(c(newdata[VAR2 >= SUBSET[1] & VAR2 <= SUBSET[2], VAR2], rev(newdata[VAR2 >= SUBSET[1] & VAR2 <= SUBSET[2], VAR2])), c(binomial()$linkinv(newdata[VAR2 >= SUBSET[1] & VAR2 <= SUBSET[2], CI.lower1]), rev(binomial()$linkinv(newdata[VAR2 >= SUBSET[1] & VAR2 <= SUBSET[2], CI.upper1]))), col = alpha(Y_COL, 0.25), border = NA)
      points(newdata[VAR2 >= SUBSET[1] & VAR2 <= SUBSET[2], VAR2], binomial()$linkinv(newdata[VAR2 >= SUBSET[1] & VAR2 <= SUBSET[2], pred1]), type = "l", lwd = 2, col = Y_COL)

      #adults
      polygon(c(newdata[VAR2 >= SUBSET[1] & VAR2 <= SUBSET[2], VAR2], rev(newdata[VAR2 >= SUBSET[1] & VAR2 <= SUBSET[2], VAR2])), c(binomial()$linkinv(newdata[VAR2 >= SUBSET[1] & VAR2 <= SUBSET[2], CI.lower2]), rev(binomial()$linkinv(newdata[VAR2 >= SUBSET[1] & VAR2 <= SUBSET[2], CI.upper2]))), col = alpha(AD_COL, 0.25), border = NA)
      points(newdata[VAR2 >= SUBSET[1] & VAR2 <= SUBSET[2], VAR2], binomial()$linkinv(newdata[VAR2 >= SUBSET[1] & VAR2 <= SUBSET[2], pred2]), type = "l", lwd = 2, col = AD_COL)

  }
  dev.off()
}
