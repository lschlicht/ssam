###############################################################
### supplementary figures #####################################
### run start of figure script first (create x2_fig from x) ###
###############################################################

#1. roosting behaviour of males
desc1 = subset(x, rel_day >= -30 & rel_day <= -1)
desc1[, N := length(unique(ID)), by = .(season)]
desc1[, N_day := length(unique(ID)), by = .(season, rel_day)]
desc1 = unique(desc1[, .(season, rel_day, N, N_day)])
desc1[, prop := N_day/N]

jpeg(file = "Suppl.Fig.ProportionRoosting.jpg", width = 300, height = 300, quality = 100)
par(las = 2)
par(mar = c(4.1, 4.1, 0.2, 0.2))
par(cex.axis = 1.3)
par(cex.lab = 1.5)
boxplot(desc1$prop ~ as.numeric(desc1$rel_day), xlab = list("Days to first egg (0 = first egg date)", cex = 1.3), ylab = list("% males roosting", cex = 1.3), las= 1, ylim = c(0, 1.1))
dev.off()




#2. start of activity across days to first egg
desc2 = subset(x, rel_day >= -30 & rel_day <= -1)
#add sunrise and sunset
crds <- matrix(c(10.88, 48.13), nrow=1)
dates <- as.POSIXct("2009-03-01", tz="Etc/GMT-2")
crds_seq <- seq(from=dates, length.out=90000, by="days")

#sunrise
  up <- as.data.table(sunriset(crds, crds_seq, direction="sunrise", POSIXct.out=TRUE))
  up[, date_ := as.Date(as.POSIXct(up$time))]
  up = up[, c('time', 'date_'), with = FALSE]
  names(up) = c('sunrise', 'date_')
  #some out-durations are too long, set all that are longer than 500 seconds to 500 seconds (N = 697, 0.7%)
  desc2[out_duration > 500, out_duration := 500]
  desc2 = merge(desc2, up, by = 'date_')
  desc2[, time_to_sunrise := (as.numeric(as.ITime(out_))+out_duration)-as.numeric(as.ITime(sunrise))]
  desc2[, time_to_sunrise_min := as.numeric(time_to_sunrise)/60]

#sunset
  down <- as.data.table(sunriset(crds, crds_seq, direction="sunset", POSIXct.out=TRUE))
  down[, date_ := as.Date(as.POSIXct(down$time))]
  down = down[, c('time', 'date_'), with = FALSE]
  names(down) = c('sunset', 'date_')
  desc2 = merge(desc2, down, by = 'date_')
  desc2[, time_to_sunset := (as.numeric(as.ITime(in_)))-as.numeric(as.ITime(sunset))]
  desc2[, time_to_sunset_min := as.numeric(time_to_sunset)/60]
  desc2[, time_activity := 24 - as.numeric((as.ITime(in_) - as.ITime(out_)))/60/60]

#plot
jpeg(file = "Suppl.Fig.timingAndSeason.jpg", width = 600, height = 900, quality = 100)
par(mfrow = c(3,1))
par(las = 2)
par(cex = 1)
par(cex.axis = 1.3)
par(cex.lab = 1.5)
par(mar = c(4.1, 6, 0.5, 0.1))
par(las = 1)
par(cex.axis = 1.5)
#sunrise
boxplot(time_to_sunrise_min ~ rel_day, data = desc2, outline = FALSE, xlab = "", ylab = "")
mtext("Time to sunrise (min)", side = 2, line = 4.5, cex = 1.5, las = 0)
#sunset
boxplot(time_to_sunset_min ~ rel_day, data = desc2, outline = FALSE, xlab = "", ylab = "")
mtext("Time to sunset (min)", side = 2, line = 4.5, cex = 1.5, las = 0)

#active
boxplot(time_activity ~ rel_day, data = desc2, outline = FALSE, xlab = "", ylab = "")
mtext("Activity (hours)", side = 2, line = 4.5, cex = 1.5, las = 0)

mtext("Days to first egg (0 = first egg date)", side = 1, line = 2.5, cex = 1.5)
dev.off()
