#sensitivity analysis
#subset to the month before the first egg
start_range = c(-60 : -24)

L = list()
for(i in start_range) {
    print(i)
    rm(x)
    data(dataset)
    x = subset(x, rel_day %in% i : -1)
    {

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

    } #create x2; final dataset containing 1 row per individual per year

    for(VAR in c("gain_sr", "loss_sr", "gain_ss", "loss_ss", "gain_act", "loss_act")) {
    if(VAR == "gain_sr") { x2[, VAR1 := EPP_gainYN];x2[, VAR2 := start] }
    if(VAR == "loss_sr") { x2[, VAR1 := EPP_lossYN];x2[, VAR2 := start] }

    if(VAR == "gain_ss") { x2[, VAR1 := EPP_gainYN];x2[, VAR2 := end] }
    if(VAR == "loss_ss") { x2[, VAR1 := EPP_lossYN];x2[, VAR2 := end] }

    if(VAR == "gain_act") { x2[, VAR1 := EPP_gainYN];x2[, VAR2 := active] }
    if(VAR == "loss_act") { x2[, VAR1 := EPP_lossYN];x2[, VAR2 := active] }

    m = glmer(VAR1 ~ factor(age, levels = c(2,1))+VAR2 + (1|ID), data = x2, family = "binomial")
    m1 = data.table(summary(m)$coefficients[2:3,])
    m1[, var := c("age+", "VAR+")]; m1[, VAR := VAR]; m1[, i := i]; m1[, N := nrow(m@frame)]
    L[[length(L)+1]] = copy(m1)

    m = glmer(VAR1 ~ VAR2 + (1|ID), data = x2, family = "binomial")
    m1 = data.table(summary(m)$coefficients[1:2,])[2,]
    m1[, var := c("VAR")]; m1[, VAR := VAR]; m1[, i := i]; m1[, N := nrow(m@frame)]
    L[[length(L)+1]] = copy(m1)

    m = glmer(VAR1 ~ factor(age, levels = c(2,1)) + (1|ID), data = x2, family = "binomial")
    m1 = data.table(summary(m)$coefficients[1:2,])[2,]
    m1[, var := c("age")]; m1[, VAR := VAR]; m1[, i := i]; m1[, N := nrow(m@frame)]
    L[[length(L)+1]] = copy(m1)

        }
}
LL = rbindlist(L)

setnames(LL, names(LL)[1:4], c("Est", "SE", "z", "P"))

####make figure
{ jpeg(paste0(Sys.Date(), "_Sensitivity analysis.jpg"), width = 900, height = 900, quality = 100)
par(mfrow = c(3,3))
par(mgp = c(2, 0.6, 0))
par(mar = c(3, 3, 1.2, 0.2))
par(cex = 1.5)
par(las = 1)
COL_age = "red"
COL_time = "blue"
LL_sep = subset(LL, nchar(var) == 4)
which.fig = 0

  for(useVAR in c("gain_sr", "gain_ss", "gain_act", "loss_sr", "loss_ss", "loss_act")) {
  which.fig = which.fig + 1

    plot(c(range(start_range)), c(-4, 1.5), type = "n", xlab = "", ylab = ifelse(useVAR == "gain_sr", "EPP gain", ifelse(useVAR == "loss_sr", "EPP loss", "")))
    abline(h = 0, lty = 2)
    abline(v = -30, col = "red")
    tmp = subset(LL_sep, var %like% "age" & VAR == useVAR)
    lines(x = tmp[, i], y = tmp[, Est], lwd = 2, col = COL_age)
      polygon(c(tmp[, i], rev(tmp[, i])), c(tmp[, Est]+1.96*tmp[,SE], rev(tmp[, Est]-1.96*tmp[,SE])), col = alpha(COL_age, alpha = 0.5), border = NA)
    tmp = subset(LL_sep, var %like% "VAR" & VAR == useVAR)
    lines(x = tmp[, i], y = tmp[, Est], lwd = 2, col = COL_time)
    polygon(c(tmp[, i], rev(tmp[, i])), c(tmp[, Est]+1.96*tmp[,SE], rev(tmp[, Est]-1.96*tmp[,SE])), col = alpha(COL_time, alpha = 0.5), border = NA)


    if(useVAR %in% c("gain_sr", "loss_sr")) legend("bottomleft", col = c(COL_age, COL_time), lty = 1, legend = c("Age", "Activity start"), bty = "n")
    if(useVAR %in% c("gain_ss", "loss_ss")) legend("bottomleft", col = c(COL_age, COL_time), lty = 1, legend = c("Age", "Activity end"), bty = "n")
    if(useVAR %in% c("gain_act", "loss_act")) legend("bottomleft", col = c(COL_age, COL_time), lty = 1, legend = c("Age", "Active period"), bty = "n")

  mtext(paste0("(", letters[which.fig], ")"), side = 3, line = -0.7, adj = -0.17, cex = 2)
  }
which.fig = which.fig+1
plot(LL[VAR == "gain_sr" & var == "VAR", N] ~ LL[VAR == "gain_sr" & var == "VAR", i], type = "l", ylim = c(0, 450), xlab = "Days to first egg", ylab = "Sample size")
abline(v=-30, col = "red")
mtext(paste0("(", letters[which.fig], ")"), side = 3, line = -0.7, adj = -0.17, cex = 2)

which.fig = which.fig+1
plot(LL[VAR == "gain_ss" & var == "VAR", N] ~ LL[VAR == "gain_ss" & var == "VAR", i], type = "l", ylim = c(0, 450), xlab = "Days to first egg", ylab = "")
abline(v=-30, col = "red")
mtext(paste0("(", letters[which.fig], ")"), side = 3, line = -0.7, adj = -0.17, cex = 2)

which.fig = which.fig+1
plot(LL[VAR == "gain_act" & var == "VAR", N] ~ LL[VAR == "gain_act" & var == "VAR", i], type = "l", ylim = c(0, 450), xlab = "Days to first egg", ylab = "")
abline(v=-30, col = "red")
mtext(paste0("(", letters[which.fig], ")"), side = 3, line = -0.7, adj = -0.17, cex = 2)

dev.off()
}

