
lardaily <- subset(lardaily, ANIODEF %in% c(2003:2006))
lardaily$CAUSAB <- substring(lardaily$CAUSAB, 1, 3)
lardaily <- subset(lardaily, CAUSAB %in% c("X93", "X94", "X95"))


lardaily$date <- with(lardaily, as.Date(paste(ANIODEF, MESDEF, DIADEF,
                                    sep = "-")))



lardaily$deaths <- 1

lardaily <- merge(data.frame(date = seq(as.Date("2003-01-01"),
                 as.Date("2005-12-31"),
                 by="day")), by = "date",
                 lardaily, all.x = TRUE)
lardaily[is.na(lardaily)] <- 0
qplot(data = ddply(lardaily, .(date), function(df) sum(df$deaths)),
      x= date, y = V1) +
    geom_point() +
    geom_vline(aes(xintercept = ban),
               color = "gray70",
               linetype = 2)


awb.lar <- ddply(lardaily, .(date), function(df) sum(df$deaths))
awb.lar <- subset(awb.lar, date < fox.troops & date > osiel.captured)
awb.lar$daynum <- -549:271
awb.lar$awb <- awb.lar$daynum > 0


m1 <- glm(data = awb.lar,
          V1 ~ daynum, family = "poisson")
summary(m1)


m2 <- glm(data = awb.lar,
          V1 ~ awb / daynum, family = "poisson")
summary(m2)

anova(m1, m2)

zip<-zeroinfl(data = awb.lar, V1 ~ daynum )
summary(zip)
zip2<-zeroinfl(data = awb.lar, V1 ~ awb / daynum)
summary(zip2)
vuong(m2, zip2)
vuong(m1, zip)

lrtest(zip, zip2)
summary(zeroinfl(data = subset(awb.lar, date > ban), V1 ~ daynum))

ggplot(awb.lar, aes(date, V1)) +
    geom_point() +
    scale_x_date() +
    geom_smooth(aes(group = fac), method = "glm", family = "poisson") +
    geom_vline(aes(xintercept = ban),
               color = "gray70",
               linetype = 2)



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
osiel.captured
hist(before.army$V1)
before.army$weeknum <- -78:37
fac <- before.army$weeknum > 0
before.army$fac <- fac
before.army$logV1 <- log(before.army$V1 + .0005)
m1 <- glm(data = before.army,
          V1 ~ weeknum, family = "poisson")
coefplot(m1)

m2 <- glm(data = before.army,
          V1 ~ fac / weeknum, family = "poisson")
coefplot(m2)
anova(m1, m2)





waldtest(m1, test="Chisq")

coeftest(m1, vcov=sandwich)


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
subset(before.army, weeknum < 0)

ggplot(before.army, aes(weeknum, V1)) +
    geom_point() +
    stat_smooth(aes(group = group), method = "glm",
                family = poisson(log)) +
    annotate("text", x = 0, y = 5,
             label = "Expiration of Assault Weapon Ban",
             vjust = -.3, angle = 90) +
    geom_vline(aes(xintercept = 0),
               size = 1) +
    opts(title = "Weekly Number of Homicides by Firearm in Nuevo Laredo from the\nDate Osiel Cardenas was Captured to the Arrival of the Army") +
    xlab("weeks from assault weapon ban") + ylab("number of homicides by firearm")
dev.print(png, "graphs/nlaredo-awb.png", width = 640, height = 480)
ban-fox.troops
