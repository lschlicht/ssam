
#### 1. subfunctions ####
{
  addID = function(con, x) {
    copy(x) -> X
    IDs = unique(dbq(con, "SELECT ID, transponder FROM BTatWESTERHOLZ.ADULTS WHERE transponder IS NOT NULL UNION
SELECT ID, transponder FROM BTatWESTERHOLZ.CHICKS WHERE transponder IS NOT NULL;"))
    X = merge(X, IDs, by.x = "transp", by.y = "transponder", all.x = TRUE)
    return(X)
  }
  addBreeding = function(con, x) {
    copy(x) -> X
    males = dbq(con, "SELECT year_, box, IDmale as ID, DATE(firstEgg) as firstEgg, DATE(hatchDate) as hatchDate, DATE(lastHatchDate) as lastHatchDate, DATE(fledgeDate) as fledgeDate, clutch, hatched, fledged, laying_gap FROM BTatWESTERHOLZ.BREEDING where IDmale is not NULL")
    females = dbq(con, "SELECT year_, box, IDfemale as ID, DATE(firstEgg) as firstEgg, DATE(hatchDate) as hatchDate, DATE(lastHatchDate) as lastHatchDate, DATE(fledgeDate) as fledgeDate, clutch, hatched, fledged, laying_gap FROM BTatWESTERHOLZ.BREEDING where IDfemale is not NULL")
    br = as.data.table(rbind(males, females))
    br[, noSocialNests := ifelse(!is.na(ID), length(.I), as.integer(NA)), by = list(year_, ID)]
    br[noSocialNests > 1, ':=' (firstEgg = NA, hatchDate = NA, lastHatchDate = NA, fledgeDate = NA, clutch = NA, hatched = NA, fledged = NA, laying_gap = NA)]
    br[noSocialNests > 1, box := as.integer(NA), by = list(year_, ID)]
    br[, noSocialNests := NULL]
    br = unique(br, by = names(br))
    X[, season := ifelse(yday(date_) > 200 | year(date_) != year_, year_ + 1, year_)]

    X = merge(X, br, by.x = c('season', 'ID'), by.y = c('year_', 'ID'), all.x = TRUE, suffixes = c('.sleep', '.breeding'))
    X[, rel_day := as.numeric(yday(date_) - yday(as.IDate(firstEgg)))]
    X[, rel_day_dusk := as.numeric(yday(date_) - yday(as.IDate(firstEgg)))-1]

    X = subset(X, rel_day > -300 | is.na(rel_day))
    return(X)
  } #this function also removes true replacement broods!

  addWeather = function(con, x) {
    copy(x) -> X

    #fetch weather data
    w = dbq(con, "SELECT HOUR(datetime_) as hour_, DATE(datetime_) as date_, temperature, precipitation, humidity, wind, radiation FROM LOGGERSatWESTERHOLZ.ENVIRONMENTAL")
    w[, date_ := as.IDate(date_)]

    X[, hour_ := ifelse(!is.na(out_), hour(as.ITime(out_)), hour(as.ITime(in_)))]
    X = merge(X, w, by = c('date_', 'hour_'), all.x = TRUE, all.y = FALSE)
    X[, hour_ := NULL]

    #dusk
    w[, date_ := as.IDate(date_)+1] #so that the dusk variables are for the previous night
    X[, hour_ := ifelse(!is.na(in_), hour(as.ITime(in_)), hour(as.ITime(out_)))]
    X = merge(X, w, by = c('date_', 'hour_'), all.x = TRUE, all.y = FALSE, suffixes = c('_dawn', '_dusk'))
    X[, hour_ := NULL]

    #daylength (daily averages of the day of entry/the day before emergence)
    w[, avgRainfall := mean(precipitation, na.rm = TRUE), by = date_]
    w[, avgTemperature := mean(temperature, na.rm = TRUE), by = date_]
    w[, avgHumidity := mean(humidity, na.rm = TRUE), by = date_]
    w = subset(w, select = c('avgRainfall', 'avgTemperature', 'avgHumidity', 'date_'))
    w = unique(w, by = names(w))
    X = merge(X, w, by = 'date_', all.x = TRUE, all.y = FALSE)




    return(X)
  }

  addIndVar = function(con, x) {
    copy(x) -> X
    ind1 = dbq(con,  "SELECT season as capture_season, ID, tarsus, age FROM BTatWESTERHOLZ.ADULTS")
    ind2 = dbq(con,  "SELECT year_ as capture_season, ID, tarsus FROM BTatWESTERHOLZ.CHICKS")
    ind2[, age := 0]

    ind = rbind(ind1, ind2)
    ind[ , ageAtFirstCapture := min(age, na.rm = TRUE), by = ID]
    ind[, seasonOfFirstCapture := capture_season[which.min(age)[1]], by = ID]
    ind[ageAtFirstCapture == 'Inf', ':=' (seasonOfFirstCapture = min(capture_season), ageAtFirstCapture = NA)]

    ind[,tarsus := mean(tarsus, na.rm = TRUE), by = ID]
    ind[, capture_season := NULL]
    ind[, age := NULL]
    ind = unique(ind, by = names(ind))
    X = merge(X, ind, by = 'ID', all.x  = TRUE)
    X[, minAge := year_ - seasonOfFirstCapture + ageAtFirstCapture + ifelse(month(date_) > 6, 1, 0)]
    X[, age := ifelse(minAge > 1, 2, 1)]
    X[, ageAtFirstCapture := NULL]
    X[, seasonOfFirstCapture := NULL]
    return(X)
  }

  addSunriseSunset = function(x) {
    copy(x) -> X

    crds <- matrix(c(10.88, 48.13), nrow=1)
    dates <- as.POSIXct("2009-03-01", tz="Etc/GMT-2")
    crds_seq <- seq(from=dates, length.out=90000, by="days")

    #sunrise
    up <- as.data.table(sunriset(crds, crds_seq, direction="sunrise", POSIXct.out=TRUE))
    up[, date_ := as.Date(as.POSIXct(up$time))]
    up = up[, c('time', 'date_'), with = FALSE]
    names(up) = c('sunrise', 'date_')

    #some out-durations are too long, set all that are longer than 500 seconds to 500 seconds (N = 697, 0.7%)
    X[out_duration > 500, out_duration := 500]

    X = merge(X, up, by = 'date_')
    X[, time_to_sunrise := (as.numeric(out_time)*60*60+out_duration)-as.numeric(as.ITime(sunrise))]
    X[, time_to_sunrise_min := as.numeric(time_to_sunrise)/60]


    return(X)
  }

  addNestStage = function(con, x) {
    copy(x) -> X
    nests = dbq(con, "SELECT * FROM BTatWESTERHOLZ.NESTS")
    nests[, nest_start := as.IDate(min(c(date_B, date_C, date_LIN), na.rm = TRUE)), by = npk]
    nests[, nest_completed := as.IDate(date_LIN)]
    nests = subset(nests, select = c('box', 'year_', 'nest_start', 'nest_completed'))
    X = merge(X, nests, by.x = c('season', 'box.breeding'), by.y = c('year_', 'box'), all.x = TRUE)
    X[is.na(nest_start) & is.na(nest_completed), ':=' (nest_start = as.IDate(firstEgg), nest_completed = as.IDate(firstEgg))]
    X[!is.na(nest_start) & is.na(nest_completed), ':=' (nest_completed = as.IDate(firstEgg))]
    return(X)
  }

  addSleep = function(x, threshold_sleep = 180) {
    copy(x) -> X
    X[, sleep := 0]
    X[!is.na(out_) & (as.Date(in_)+1) == as.Date(out_) , sleep := 0.5]
    X[abs(time_to_sunset_min) < threshold_sleep & abs(time_to_sunrise_min) < threshold_sleep & !is.na(in_) & !is.na(out_) & (as.Date(in_)+1) == as.Date(out_) , sleep := 1]
    return(X)
  }

  addSex = function(con, x) {
    copy(x) -> X
    sex = dbq(con, "SELECT ID, sex FROM BTatWESTERHOLZ.SEX where SEX is not NULL")
    X = merge(X, sex, by = "ID", all.x = TRUE)
    return(X)
  }

  addEPP = function(con, x) {

    eppGain = dbq(con, " select * from (
                (SELECT DISTINCT year_, sum(epy) AS EPP_gain, father AS IDmale
                FROM
                (SELECT year_, epy, father FROM BTatWESTERHOLZ.PATERNITY P
                WHERE father IS NOT NULL) AS p1

                GROUP BY year_, father HAVING  sum(epy) IS NOT NULL) AS Y
                LEFT OUTER JOIN

                --  NO OF EP FEMALES
                ( SELECT DISTINCT year_, count(DISTINCT mother) AS EP_females, father AS IDmale
                FROM
                (SELECT year_, epy, mother, father FROM BTatWESTERHOLZ.PATERNITY P
                WHERE  father IS NOT NULL) AS p2

                WHERE father IS NOT NULL AND epy = 1
                GROUP BY year_, father ) AS F
                ON
                Y.IDmale = F.IDmale AND Y.year_ = F.year_ ) ; ")
    eppGain = eppGain[, -6, with  =FALSE]
    eppGain = eppGain[, -4, with  =FALSE]
    setnames(eppGain, "IDmale", "ID")
    setnames(eppGain, "year_", "season")

    eppLoss = dbq(con, 'SELECT year_ as season, box AS "box.breeding", sum(epy) as epy FROM BTatWESTERHOLZ.PATERNITY WHERE epy is not NULL group by year_, box')

    x = merge(x, eppGain, by = c('season', 'ID'), all.x = TRUE)
    x = merge(x, eppLoss, by = c('season', 'box.breeding'), all.x = TRUE)
    setnames(x, "epy", "EPP_loss")
    x[is.na(EP_females) & sex == 1, EP_females := 0]
    return(x)
  }

  markReplacements = function(con, x) {
    X = copy(x)
    fe = dbq(con, "SELECT DATE(firstEgg) as firstEgg, year_ as breeding_season, box FROM BTatWESTERHOLZ.BREEDING")
    #fe[, firstEgg := scale(as.IDate(firstEgg), center = TRUE, scale = FALSE), by = breeding_season]
    fe[, firstEgg := as.numeric(as.IDate(firstEgg))]
    fe[, max_ := ifelse(firstEgg > boxplot.stats(firstEgg)$stats[5], 1, 0), by = breeding_season]
    fe = subset(fe, max_ == 1)
    fe[, firstEgg := as.IDate(firstEgg, origin = "1970-01-01")]

    X[, replacementClutchOutlier := 0]
    X[paste(season, box.breeding) %in% paste(fe[,breeding_season], fe[,box]), replacementClutchOutlier := 1]

    return(X)
  }

  markAndRemoveExperiments = function(con, x) {

    #LEDs 2012, 2013
    exp12 = dbq(con, "SELECT box, date(Installed) as installed, date(turnedOff) as turnedOff FROM EXTRA_BTatWESTERHOLZ.2012_LS_LEDs where ExpOrControl = 'exp'")
    exp13 = dbq(con, "SELECT box, date(installed) as installed, date(turnedOff) as turnedOff FROM EXTRA_BTatWESTERHOLZ.2013_LS_LEDs where ExpOrControl = 'exp'")

    exp12[, turnedOff := as.IDate(turnedOff)]
    exp13[, turnedOff := as.IDate(turnedOff)]
    exp12[is.na(turnedOff), turnedOff := as.IDate("2012-05-30")]
    exp13[is.na(turnedOff), turnedOff := as.IDate("2013-05-30")]
    expLED = rbind(exp12, exp13)
    expLED = split(expLED, rownames(expLED))
    expLED = rbindlist(lapply(expLED, FUN = function(y) { dates = as.IDate(y[1, installed]) : as.IDate(y[1, turnedOff]); y = data.table(date_ = as.IDate(dates), box.sleep = rep(y[1,box], length(dates))); return(y)}))
    expLED[, remove := 1]
    x = merge(x, expLED, all.x = TRUE, by = c("date_", "box.sleep"))
    x[is.na(remove), experimental := 0]
    x[remove == 1, experimental := 1]
    x[, remove := NULL]

    #predation experiment 2017 - part 1
    exp17.1 = dbq(con, "SELECT * FROM FIELD_2017_BTatWESTERHOLZ.EXPERIMENTS")[1,]

    a = exp17.1$'function';  a = gsub("\r", ' ', a);  a = gsub("\n", ' ', a);  a = gsub("\t", ' ', a); a = substring(a, 1, 2122); a = paste(a, "}", sep = ''); exp17.1 = eval(parse(text = a))
    exp17.1 = as.data.table(exp17.1())
    exp17.1 = subset(exp17.1, treatment == "Predator")
    exp17.1[, treatment := NULL]
    exp17.1[, date_ := as.IDate(date_)]
    setnames(exp17.1, "box", "box.sleep")

    #predation experiment 2017 - part 2
    exp17.2 = dbq(con, "SELECT * FROM FIELD_2017_BTatWESTERHOLZ.EXPERIMENTS")[2,]
    a = exp17.2$'function';  a = gsub("\r", ' ', a);  a = gsub("\n", ' ', a);  a = gsub("\t", ' ', a); a = substring(a, 1, 6329); a = paste(a, "}", sep = ''); exp17.2 = eval(parse(text = a))
    exp17.2 = as.data.table(exp17.2())
    exp17.2 = subset(exp17.2, experimenter %in% c('BA', 'L', 'T')) #predator: BA, L, T; control: W, BU, C
    exp17.2[, experimenter := NULL]
    exp17.2[, date_ := as.IDate(date_)]
    setnames(exp17.2, "box", "box.sleep")

    #combine both 2017 experiments
    exp17 = rbind.data.frame(exp17.1, exp17.2)
    exp17[, remove := 1]
    x = merge(x, exp17, all.x = TRUE, by = c("date_", "box.sleep"))
    x[remove == 1, experimental := 1]
    x[, remove := NULL]

    return(x)
  }
}

#### 2. function calls ####
{

  create_sleep_data = function(hooray, SEX = 1) {
    #note that this is different to the other studies!!!
    dat = copy(hooray)

    #remove any entries before 4:30 o'clock
    dat = subset(dat, as.ITime(out_) > 4.5*60*60)
    #select first entry per individual per day
    dat = subset(dat, !is.na(transp))
    setkey(dat, transp, out_)
    dat[, date_ := as.IDate(out_)]
    dat[, counter := 1:.N, by = .(transp, date_)]
    dat = dat[counter == 1,]
    dat[, counter := NULL]
    dat[, out_time := as.numeric(as.ITime(out_))/60/60]

    #which direction_detail should be counted as start of activity? COmpare to "O/I|I/O" that is from one night to the next
    tmp = subset(dat, out_time > 4 & out_time < 8)
    tmp = tmp[nchar(direction_detail) == 3 | as.IDate(in_)+1 == date_, ]

    jpeg(paste0(getwd(), "/Figures/DirectionDetail.jpeg"), width = 1500, height = 1500)
    par(mfrow = c(10,11))
    par(mar = c(2, 2, 0.1, 0.1))
    par(cex = 1)

    for(i in sort(unique((hooray[, direction_detail])))) {
      tmptmp = tmp[direction_detail == i,out_time]
      if(length(tmptmp) > 1) {
        plot(density(tmptmp), xlim = c(5, 8), main = "")
        mtext(i, side = 1, adj = 1, line = -1, cex = 1.5)
        mtext(length(tmptmp), side = 1, adj = 0, line = -1, cex = 1.5)

      }
    }
    plot(density(tmp[direction_detail == "O/I|I/O" & as.IDate(in_) + 1 == date_,out_time]), xlim = c(5, 8), col = 'red', main = '')
    dev.off()

    #basically the rule boils down to "if one direction is ok and the other not opposing, then use it.
    dat = subset(dat, toupper(direction_detail) %in% c("?/I|I/?", "?/I|I/O", "?/O", "I/O", "O/I|I/?", "O/I|I/O") | direction_detail == "i/I|I/O" & direction_detail == "I/I|I/O")
    dat = subset(dat, nchar(direction_detail) == 3 | as.IDate(in_) + 1 == date_)
    jpeg(paste0(getwd(), "/Figures/DirectionFit.jpeg"), width = 500, height = 500)
    par(mfrow = c(2,1))
    par(mar = c(3,3,0.1, 0.1))
    plot(density(dat[direction_detail != "O/I|I/O",out_time]), xlim = c(5, 8), col = 'black', main = '')
    mtext("all data excluding reference data", side = 1, adj = 1, line = -1)
    mtext(length(dat[direction_detail != "O/I|I/O",out_time]), side = 1, adj = 0, line = -1)
    plot(density(dat[direction_detail == "O/I|I/O",out_time]), xlim = c(5, 8), col = 'red', main = '')
    mtext("reference data", side = 1, adj = 1, line = -1)
    mtext(length(dat[direction_detail == "O/I|I/O",out_time]), side = 1, adj = 0, line = -1)
    dev.off()



    dat[, year_ := year(date_)]

    con = dbcon("lschlicht")
    copy(dat) -> x
    x = addID(con, x)
    x = addSex(con, x)
    x = subset(x, sex == SEX)
    x = addBreeding(con, x)
    x = addIndVar(con, x) #warning message about "Inf" is ok
    x = addSunriseSunset(x)
    x = addEPP(con, x)
    x[, EPP_lossYN := ifelse(EPP_loss > 0, 1, EPP_loss)]
    x[, EPP_gainYN := ifelse(EPP_gain > 0, 1, EPP_gain)]
    x[, yid := paste(substring(firstEgg, 1, 4), ID, sep = '_')]
    x = markReplacements(con, x)
    x = markAndRemoveExperiments(con, x)
    x = subset(x, experimental == 0)
    setkey(x, year_, box.breeding, ID, date_)
    closeCon(con)

    #further transformations
    x = subset(x, !is.na(ID) & !is.na(sex))
    x[, in_r_pk := NULL]
    x[, out_r_pk := NULL]
    x[, direction_detail := NULL]
    x[, direction := NULL]
    x[, type := NULL]
    x = unique(x)
    x[, firstEgg := as.IDate(firstEgg)]

    #scale first egg within season, but take into account that individual data points were measured multiple times!
    tmp = unique(subset(x, select = c('firstEgg', 'season', 'box.breeding')))[, z_firstEgg := scale(firstEgg), by = season]
    x = merge(x, tmp, by = c('firstEgg', 'season', 'box.breeding'))
    #checks
    tmp = unique(subset(x, select = c("ID", "year_", "sex")))
    tmp[, count_ := .N, by = list(ID)]
    tmp[, ":=" (year_ = NULL)]
    tmp = unique(tmp)
    table(tmp$count_, tmp$sex)

    #restrict to breeding individuals
    x = subset(x, !is.na(firstEgg))
    x[, yid := paste0(year_, ID)]


    #add day-of-year column
    x[, YDAY := yday(date_)]




    x = subset(x, minAge > 0) #all these 8 data points are an SNB errors.

    #add id of partner
    con = dbcon("lschlicht")
    par = dbq(con, "SELECT IDmale as ID, IDfemale as IDpartner, box, year_ as season FROM BTatWESTERHOLZ.BREEDING where IDmale is not NULL and IDfemale is not NULL")
    closeCon(con)
    setnames(par, "box", "box.breeding")

    x = merge(x, par, by = c('ID', 'box.breeding', 'season'), all.x = TRUE, all.y = FALSE)

    #remove "wrong" data: more than 1.5 hours before or 1.5 hours after sunrise
    x = subset(x, abs(time_to_sunrise_min) <= 90 )

    save(x, file = paste0(getwd(), "/Data/", Sys.time(), "_full_data.RData"))

    return(x)
  }
}

  scale3 = function(x, N) {x = x - median(x, na.rm = TRUE); x = x / sd(x, na.rm = TRUE); if(length(x) < N) x = NA; return(x)}

  mcenter3 = function(x, N) {x = x - median(x, na.rm = TRUE); if(length(x) < N) x = NA; return(x)}

  median_n = function(x, N) { if(length(na.omit(x)) >= N) return(median(x, na.rm = TRUE)) else return(NA)}


  { #sound data
    addSong = function(x) {
    #old song data - only 3 males, not maybe worth it.
    library(readxl)
    male_song <- data.table(read_excel(paste0(getwd(), "/Sound data/male song.xlsx")))
    male_song = subset(male_song, select = c('date_', 'box', 'start quiet', 'start intermediate', 'start close'))
    setnames(male_song, names(male_song), make.names(names(male_song)))

    male_song[is.na(start.intermediate), start.intermediate := start.close]
    male_song[is.na(start.quiet), start.quiet := start.intermediate]

    library(readxl)
    dates_and_recording_times <- data.table(read_excel(paste0(getwd(), "/Sound data/dates and recording times.xlsx")))
    setnames(dates_and_recording_times, names(dates_and_recording_times), c("date_", "rec_start"))
    dates_and_recording_times[, rec_start := as.ITime(paste(date_, substring(rec_start, 12,), sep = ' '))]
    dates_and_recording_times
    male_song = merge(male_song, dates_and_recording_times)
    male_song[, bt := as.ITime(substring(start.quiet, 12,))]
    male_song[, bt45 := as.ITime(substring(start.intermediate, 12,))]
    male_song[, bt := bt + rec_start]
    male_song[, bt45 := bt45 + rec_start]
    male_song = subset(male_song, !is.na(bt) | !is.na(bt45), select = c('date_', 'box', 'bt', 'bt45'))
    male_song[, author := "LS"]
    male_song[, date_ := as.IDate(date_)]

    crds <- matrix(c(10.88, 48.13), nrow=1)
    dates <- as.POSIXct("2012-03-01", tz="Etc/GMT-2")
    crds_seq <- seq(from=dates, length.out=120, by="days")
    #sunrise
    up <- as.data.table(sunriset(crds, crds_seq, direction="sunrise", POSIXct.out=TRUE))
    up[, date_ := as.IDate(as.POSIXct(up$time))]
    up = up[, c('time', 'date_'), with = FALSE]
    names(up) = c('sunrise', 'date_')

    male_song = merge(male_song, up, by = 'date_')
    male_song[, bt := round(as.numeric(as.ITime(bt) - as.ITime(sunrise))/60, digits = 2)]
    male_song[, bt45 := round(as.numeric(as.ITime(bt45) - as.ITime(sunrise))/60, digits = 2)]
    male_song[, sunrise := NULL]

    #song data and stats#####
    song = data.table(read.csv(file = paste0(getwd(), "/Sound data/Male Song.csv"), sep = ';', stringsAsFactors = FALSE))
    song[, date_ := as.IDate(date, format = "%m.%d.%Y")]
    song[, age := NULL]
    song[, date := NULL]
    song[, ID := NULL]
    song[, author := "P3"]
    setcolorder(song, names(male_song))

    song19 = data.table(read.csv(file = paste0(getwd(), "/Sound data/Sound analysis 2019.csv"), sep = ';', stringsAsFactors = FALSE))
    song19[, date_ := as.IDate(date_, format = "%d/%m/%Y")]
    song19[, recorder_time := NULL]
    song19[, sunrise := NULL]
    song19[, author := "P3"]
    song19[, bt := NULL]
    song19[, bt45 := NULL]
    song19[, X := NULL]
    song19[, X.1 := NULL]
    setnames(song19, c("bt_mins_to_sunrise", "bt45_mins_to_sunrise", "box_"), c("bt", "bt45", "box"))
    setcolorder(song19, names(male_song))
    song19[, bt := sub(",", ".", bt)]
    song19[, bt45 := sub(",", ".", bt45)]

    #combine sound analyses
    S = rbindlist(list(male_song, song, song19))
    S[bt == "no recording", bt := NA]
    S[bt45 == "no recording", bt45 := NA]
    S[, bt := as.numeric(bt)]
    S[, bt45 := as.numeric(bt45)]


    x = merge(x, S, by.x = c("date_", "box.breeding"), by.y = c("date_", "box"), all.x = TRUE)
    return(x)
    }
}
