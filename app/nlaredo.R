########################################################
#Clean up the daily homicide data in Nuevo Laredo
########################################################
lardaily <- subset(lardaily, ANIODEF %in% c(2003:2006))
lardaily$CAUSAB <- substring(lardaily$CAUSAB, 1, 3)
lardaily <- subset(lardaily, CAUSAB %in% c("X93", "X94", "X95"))
lardaily$date <- with(lardaily, as.Date(paste(ANIODEF, MESDEF, DIADEF,
                                    sep = "-")))
lardaily$deaths <- 1

#Manually add the days that had no homicides
lardaily <- merge(data.frame(date = seq(as.Date("2003-01-01"),
                 as.Date("2005-12-31"),
                 by="day")), by = "date",
                 lardaily, all.x = TRUE)
lardaily[is.na(lardaily)] <- 0

awb.lar <- ddply(lardaily, .(date), function(df) sum(df$deaths))
#daily <- ddply(lardaily, .(date), function(df) sum(df$deaths))
awb.lar$group <- cut(awb.lar$date,
                   c(ban-1999, ban, ban+999999))
print(qplot(data = awb.lar, x= date, y = V1) +
    geom_point() +
    geom_smooth(aes(group = group), method = "glm",
                family = poisson(log)) +
    geom_vline(aes(xintercept = ban),
               color = "gray70",
               linetype = 2))


#Analyze only the murders from the time Osiel Cardenas was
#captured to the time the army arrived
awb.lar <- subset(awb.lar, date < fox.troops & date > osiel.captured)
awb.lar$daynum <- -549:271
awb.lar$awb <- awb.lar$daynum > 0

sink("reports/report.txt")
########################################################
#Poisson Model
########################################################
m1 <- glm(data = awb.lar,
          V1 ~ daynum, family = "poisson")
coefplot(m1)
m2 <- glm(data = awb.lar,
          V1 ~ awb / daynum, family = "poisson")
coefplot(m2)
anova(m1, m2)


########################################################
#Zero Inflated Poisson Model
########################################################
zip<-zeroinfl(data = awb.lar, V1 ~ daynum )
cat("***Zero inflated poisson Model without a breakpoint:***")
print(summary(zip))
cat("\n---\n")

zip2<-zeroinfl(data = awb.lar, V1 ~ awb / daynum)
cat("***Zero inflated poisson Model with a breakpoint corresponding to the expiration of the assault weapon ban:***")
print(summary(zip2))
cat("\n---\n")
cat("\n----\n")

########################################################
#Check that the Zero Inflated model is better than
#the Poisson one
########################################################
cat("***Comparing zero inflated poisson model to normal poisson without a breakpoint:***\n")
vuong(m2, zip2)
cat("\n---\n")
cat("***Comparing zero inflated poisson model to normal poisson with a breakpoint:***\n")
vuong(m1, zip)
cat("\n---\n")
cat("\n---\n")

########################################################
#Is the model with a breakpoint better than the one without?
########################################################
cat("***Likelihood ratio test comparing the zero inflated models with and\n without breakpoint***")
print(lrtest(zip, zip2)) #It is
unlink("reports/report.txt")


########################################################
#Now that it is proven that a model with a breakpoint that
#coincides with the assault weapon ban is better, the rest
#is model checking, effect size, charts
########################################################
summary(zeroinfl(data = subset(awb.lar, date > ban), V1 ~ daynum))

print(ggplot(awb.lar, aes(date, V1)) +
    geom_point(alpha = .5) +
    scale_x_date() +
    geom_smooth(aes(group = awb), method = "glm", family = "poisson") +
    geom_vline(aes(xintercept = as.Date(ban)),
               color = "#000000",
               linetype = 1) +
    ylab("number of homicides by firearm") +
    annotate("text", x = ban, y = 4,
             label = "Expiration of Assault Weapon Ban",
             vjust = -.3, angle = 90) +
    opts(title = "Daily Homicides in Nuevo Laredo from the time\nOsiel Cardenas was Captured to the Time the Army Arrived"))
dev.print(png, "graphs/nlaredo-daily.png", width = 640, height = 480)


########################################################
#A Chart with the weekly homicides is easier to read
########################################################
lardaily$week <- as.numeric(format(lardaily$date, "%W"))
lardaily.w <- ddply(lardaily, .(ANIODEF, week), function(df) nrow(df))
lardaily.w <- merge(data.frame(ANIODEF = rep(2003:2006, each = 52),
                          week = 1:52),
               lardaily.w, all.x = TRUE)
lardaily.w[is.na(lardaily.w)] <- 0
lardaily.w$date <- seq(as.Date("2003-01-01"),
                    as.Date("2006-12-26"),
                    by="week")
lardaily.w$group <- cut(lardaily.w$date,
                   c(ban-999, ban, ban+999999))

before.army <- subset(lardaily.w, (ANIODEF == 2005 & week < 24) |
                       (ANIODEF == 2004) |
                        (ANIODEF == 2003 & week > 11))

hist(before.army$V1)
before.army$weeknum <- -78:37
before.army$fac <- before.army$weeknum > 0

########################################################
#Poisson Model
########################################################
m1 <- glm(data = before.army,
          V1 ~ weeknum, family = "poisson")
coefplot(m1)
m2 <- glm(data = before.army,
          V1 ~ fac / weeknum, family = "poisson")
coefplot(m2)
anova(m1, m2)

########################################################
#Zero Inflated Model
########################################################
zip<-zeroinfl(data = before.army, V1 ~ weeknum )
summary(zip)
zip2<-zeroinfl(data = before.army, V1 ~ fac / weeknum)
summary(zip2)
vuong(m2, zip2)
vuong(m1, zip)
lrtest(zip, zip2)

before.army$num <- 1:nrow(before.army)
zip<-zeroinfl(data = subset(before.army, weeknum >= 0),
              V1 ~ num)
summary(zip)
zip2<-zeroinfl(data = subset(before.army, weeknum <= 0),
          V1 ~ num)
summary(zip2)


print(ggplot(before.army, aes(weeknum, V1)) +
    geom_point() +
    stat_smooth(aes(group = group), method = "glm",
                family = poisson(log)) +
    annotate("text", x = 0, y = 5,
             label = "Expiration of Assault Weapon Ban",
             vjust = -.3, angle = 90) +
    geom_vline(aes(xintercept = 0),
               size = 1) +
    opts(title = "Weekly Number of Homicides by Firearm in Nuevo Laredo from the\nDate Osiel Cardenas was Captured to the Arrival of the Army") +
    xlab("weeks from assault weapon ban") + ylab("number of homicides by firearm"))
dev.print(png, "graphs/nlaredo-weekly.png", width = 640, height = 480)

