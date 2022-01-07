setwd("/ds/grpkempenaers/Lotte/R Studio projects/Data for ssam")

require(SNB2)
require(ssam)

a = Sys.time() #41 minutes
df = data.table(box = rep(1:277, 11), from = rep(paste0(2010:2020, "-01-01 00:00:00"), each = 277), to = rep(paste0(2010:2020, "-06-01 00:00:00"), each = 277))
hooray = eva('lschlicht', df)
save(hooray, file = paste0(getwd(), "/Data/", Sys.time(), "_raw_data.RData"))
#load("/ds/grpkempenaers/Lotte/R Studio projects/Data for ssam/Data/2021-03-02 13:51:00.666_raw_data.RData")
#load("/ds/grpkempenaers/Lotte/R Studio projects/Data for ssam/Data/2021-06-04 12:37:02.980_raw_data.RData")
Sys.time()-a

#for now: add 1 hour during winter time and 2 hours during summer time to the data of 2020
#29.03. 2:00 switch from winter to summer time
#add one hour in winter
hooray[in_ > "2020-01-01", in_ := in_ + 1*60*60]
hooray[out_ > "2020-01-01", out_ := out_ + 1*60*60]
#add another hour in summer
hooray[in_ > "2020-03-28", in_ := in_ + 1*60*60]
hooray[out_ > "2020-03-28", out_ := out_ + 1*60*60]


#plot to check (run before and after)
copy(hooray) -> test
test[, min_time := min(in_, out_), by = .(as.IDate(in_), box)]
test = test[min_time == in_ | min_time == out_,]
test[, season := year(min_time)]
test[, min_time_min := as.numeric(as.ITime(min_time))]
ggplot(test, aes(x = factor(season), y = min_time_min)) + geom_boxplot()
#
setwd("/ds/grpkempenaers/Lotte/R Studio projects/Data for ssam")
x = create_sleep_data(hooray, SEX = 1) #warnings are ok

save(x, file = "/ds/grpkempenaers/Lotte/R Studio projects/Data for ssam/Data/2021-06-04_full_data.RData")
#load("/ds/grpkempenaers/Lotte/R Studio projects/Data for ssam/Data/2021-06-04_full_data.RData")

x[, ":=" (hatchDate = NULL, lastHatchDate = NULL, fledgeDate = NULL, clutch = NULL, hatched  = NULL, fledged = NULL, laying_gap = NULL, tarsus = NULL, replacementClutchOutlier = NULL, experimental = NULL, sunrise = NULL, time_to_sunrise = NULL, time_to_sunrise_min = NULL, IDpartner = NULL, transp = NULL, out_time = NULL, sex = NULL, rel_day_dusk = NULL, minAge = NULL, YDAY = NULL, z_firstEgg = NULL, box.breeding = NULL, box.sleep = NULL, year_ = NULL, yid = NULL)]
write.table(x, file = "/ds/grpkempenaers/Lotte/R Studio projects/ssam/data/dataset.csv", sep = ",", row.names = FALSE)
save(x, file = "/ds/grpkempenaers/Lotte/R Studio projects/ssam/data/dataset.RData")


##ADDITIONAL INFORMATION FOR METHODS
####calculate proportion of yearling males among breeders
require(sdb)

con = dbcon("lschlicht")

br = unique(dbq(con, "SELECT year_ as season, IDmale as ID FROM BTatWESTERHOLZ.BREEDING where IDmale is not NULL"))
ad = dbq(con, "SELECT ID, season, age FROM BTatWESTERHOLZ.ADULTS where age is not NULL")
ch = dbq(con, "SELECT ID, year_ as season FROM BTatWESTERHOLZ.CHICKS")
ch[, age := 0]
age = unique(rbind(ad, ch))

br2 = merge(br, age, by = c("ID", "season"), all = FALSE)
br2 = subset(br2, age > 0)
a = (table(br2$age))
a[1]/(a[1] + a[2])
a = (table(br2$age, br2$season))
a[1,]/(a[1,] + a[2])
min(a[1,]/(a[1,] + a[2]))
max(a[1,]/(a[1,] + a[2]))


####number of ep sires etc.
#use x2 from the models script
a = table(x2$EPP_gainYN)
a
a[2]/sum(a)
sum(a)
b = table(x2$EP_females)
b
#0: 243, 1:123, 2:35, 3:4, 4:2, 5:1
#the number of the first group is off, there is a misspecification in the number of EP females (not relevant, since we're not using the variable)
b[2]/sum(b[2:6])
b[3]/sum(b[2:6])
sum(b[4:5])/sum(b[2:6])
table(x2$EP_females, x2$age)

