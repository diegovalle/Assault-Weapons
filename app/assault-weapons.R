########################################################
#####       Author: Diego Valle Jones
#####       Website: www.diegovalle.net
#####       Date Created: Fri Jun 11 12:22:27 2010
########################################################
#The effect of the assault weapons ban on the proportion of Firearms
#used to kill people
#Structural Change models and Graphics

source("app/functions.R")

#A macro to add a deseasonalized trend
addTrend <- defmacro(df, col, start, expr={
    df$trend <- data.frame(stl(ts(df[[col]], start = start, freq = 12), "per")$time.series)$trend
})


#Test for cointegration
unitRoot <- function(df){
    reg <- lm(df$Murders.with.Firearm ~ df$Murders)
    ht <- adf.test(residuals(reg))
    ht
}
#Looks ok
dlply(hom, .(State), unitRoot)


########################################################
#Suicides and Suicides with a Firearm
########################################################
addTrend(sui, "prop", 1998)
#sui$trend2 <- data.frame(stl(ts(sui$prop, start = 1998, freq = 12), "per")$time.series)$trend

p <- ggplot(sui, aes(date, prop)) +
    geom_line(size = .2) +
    geom_line(data = sui, aes(date, trend), color = "blue") +
    scale_x_date() +
    geom_vline(aes(xintercept = as.Date(ban)), color = "#000000",
               linetype = 2) +
    opts(title = "Suicides by Firearm Discharge as a Proportion of all Suicides") +
    scale_y_continuous(limits = c(0, .27)) +
    annotate("text", x = ban, y = .05, hjust = 1.01,
             label = "Assault weapon ban expiration") +
    xlab("date") + ylab("annualized monthly suicide rate")
savePlotAA(p, "graphs/mexico-sui-firearm-prop.png")

sui$trend <- NULL
sui.both <- sui
sui.both[3:4] <- data.frame(sapply(sui.both[3:4],
                function(x) x / pop2$Monthly[1:nrow(sui.both)] *
                                100000 * 12))
sstl <- apply(sui.both[3:4], 2,
      function(x) {
        stl(ts(x, start = 1998, freq = 12), "per")
      })

sstl <- lapply(sstl, function(x)
                      cbind(sui.both, data.frame(x$time.series)))

sui.both <- melt(sui.both, id = c("Year", "m"),
                 measure.var = c("Firearm.Suicides",
                 "Suicides"))
sui.both$date <- as.Date(paste(sui.both$Year,
                             sui.both$m,"15", sep = "/"), "%Y/%m/%d")
#Necessary for ggplot
sstl$Firearm.Suicides$date <- sui.both$date[1:132]
sstl$Suicides$date <- sui.both$date[1:132]
sstl$Firearm.Suicides$variable <- "foo"
sstl$Suicides$variable <- "foo"

p <- ggplot(sui.both, aes(date, value, group = variable)) +
    geom_line(size = .2) +
    geom_line(data = sstl$Firearm.Suicides, aes(date, trend),
              color = "blue") +
    geom_line(data = sstl$Suicides, aes(date, trend), color = "blue") +
    scale_x_date() +
    geom_vline(aes(xintercept = as.Date(ban)), color = "#000000",
               linetype = 2) +
    opts(title = "Monthly Suicide and Suicide by Firearm Discharge Rates with Trends") +
    annotate("text", x= as.Date("1999-01-15"), y = 4.2,
             label ="Suicides") +
    annotate("text", x= as.Date("2000-01-15"), y = 1.1,
             label ="Suicides with a Firearm") +
    annotate("text", x = ban, y = 2.5, hjust = 1.01,
             label = "Assault weapon ban expiration") +
    xlab("date") + ylab("annualized monthly suicide rate")
savePlotAA(p, "graphs/mexico-sui-firearm.png")

########################################################
#Homicide and Firearm Homicide rates
########################################################
hom.both <- ddply(hom, .(Year, m), function(df)
                  c(firearm = sum(df$Murders.with.Firearm),
                    homicides = sum(df$Murders)))
hom.both[3:4] <- data.frame(sapply(hom.both[3:4],
                function(x) x / pop2$Monthly[1:nrow(hom.both)] *
                                100000 * 12))
#STL Decomposition
stl <- apply(hom.both[3:4], 2,
      function(x) {
        stl(ts(x, start = 1998, freq = 12), "per")
      })

stl <- lapply(stl, function(x)
                      cbind(hom.both, data.frame(x$time.series)))


hom.both <- melt(hom.both, id = c("Year", "m"))
hom.both$date <- as.Date(paste(hom.both$Year,
                             hom.both$m,"15", sep = "/"), "%Y/%m/%d")

#Necessary for ggplot
stl$firearm$date <- hom.both$date[1:131]
stl$homicide$date <- hom.both$date[1:131]
stl$homicide$variable <- "foo"
stl$firearm$variable <- "foo"

p <- ggplot(hom.both, aes(date, value, group = variable)) +
    geom_line(size = .2) +
    geom_line(data = stl$firearm, aes(date, trend), color = "blue") +
    geom_line(data = stl$homicide, aes(date, trend), color = "blue") +
    scale_x_date() +
    geom_vline(aes(xintercept = as.Date(ban)), color = "#000000",
               linetype = 2) +
    opts(title = bothtitle) +
    scale_y_continuous(limits = c(0, 17)) +
    annotate("text", x= as.Date("2001-06-15"), y = 12.5,
             label ="Homicides") +
    annotate("text", x= as.Date("2002-01-15"), y = 3.5,
             label ="Homicides by Firearm") +
    annotate("text", x = ban, y = 16, hjust = 1.01,
             label = "Assault weapon ban expiration") +
    xlab("date") + ylab("annualized monthly homicide rate")
savePlotAA(p, "graphs/mexico-hom-firearm.png")

########################################################
#For all of Mexico
########################################################
hom.mx <- ddply(hom, .(Year, m), function(df)
                sum(df$Murders.with.Firearm) / sum(df$Murders))
hom.mx$date <- as.Date(paste(hom.mx$Year,
                             hom.mx$m,"15", sep = "/"), "%Y/%m/%d")

addTrend(hom.mx, "V1", 1998)

rate <- ts(hom.mx$V1, start = kstart.year, freq = 12)
breakmx <- breakpoints(rate ~ 1, h = 12, breaks = 1)
breakconf <- confint(breakmx, breaks = 1)$confint
aw.break <- sapply(breakconf, convertToDate)

p <- ggplot(hom.mx, aes(date, V1)) +
    geom_line(size = .2) +
    geom_line(aes(date, trend), color = "blue") +
    geom_vline(aes(xintercept = as.Date(ban)), color = "#000000",
               linetype = 2) +
    opts(title = "Homicides by Firearm Discharge as a Proportion of all Homicides in Mexico") +
    annotate("text", x = ban, y = .35, hjust = 1.01,
             label = "Assault weapon ban expiration") +
    xlab("date") + ylab("proportion") +
    scale_y_continuous(formatter = "percent", limits = c(0, .8))
savePlotAA(p, "graphs/mexico-prop.png", width = 800,
           height = 600)


########################################################
#For the different regions (SW NW, etc)
########################################################

x <- dlply(hom.region, .(Region), transform, trend = data.frame(stl(ts(prop, start = 2001, freq = 12), "per")$time.series)$trend )
hom.region <- rbind.fill(x)

#hom.region$trend <- data.frame(stl(ts(hom.region$prop, start = 1998, freq = 12), "per")$time.series)$trend

hom.region <- ddply(hom.region, .(Region), transform,
                    order = mean(prop))
hom.region$Region <- reorder(hom.region$Region, -hom.region$order)
p <- ggplot(hom.region, aes(date, prop)) +
       geom_vline(aes(xintercept = as.Date(ban)), color = "#000000",
               linetype = 2) +
       geom_line(size = 0.2) +
       geom_line(aes(date, trend), color = "blue") +
       scale_x_date(major = "2 years") +
       facet_wrap(~ Region) +
       xlab("date") + ylab("proportion of homicides by firearm") +
       opts(title="Proportion of Homicides by Firearm Discharge in the Crazy Wrong  Regions of Mexico")+
       scale_y_continuous(formatter = "percent", limits = c(0, .8))
savePlotAA(p, "graphs/regions.png")


########################################################
#The Regions according to INEGI look crazy, let's do them manually
#http://es.wikipedia.org/wiki/Archivo:800px-Mexico_map_of_regionsfr.png
########################################################
hom$region <- 0

setRegion <- defmacro(df, states, region.name, expr={
    df[df$State %in% states,]$region <- region.name
})

regions <- list()
regions[["North West"]] <- c("Durango", "Sinaloa", "Chihuahua", "Sonora", "Baja California Sur", "Baja California")
regions[["West"]] <- c("Nayarit", "Jalisco", "Colima", "Michoacán")
regions[["East"]] <- c("Puebla", "Veracruz Llave", "Tlaxcala", "Hidalgo")
regions[["North East"]] <- c("Coahuila", "Nuevo León", "Tamaulipas")
regions[["South East"]] <- c("Tabasco", "Campeche", "Quintana Roo", "Yucatán")
regions[["South West"]] <- c("Guerrero", "Oaxaca","Chiapas")
regions[["Center North"]] <- c("Aguascalientes", "Guanajuato", "Querétaro", "San Luis Potosí", "Zacatecas")
regions[["Center South"]] <- c("Morelos", "México", "Distrito Federal")


for(i in 1:length(regions))
  setRegion(hom, regions[[i]], names(regions)[i])


hom.region <- ddply(hom, .(region, date), function (df) sum(df$Murders.with.Firearm) / sum(df$Murders))

x <- dlply(hom.region, .(region), transform, trend = data.frame(stl(ts(V1, start = 2001, freq = 12), "per")$time.series)$trend )
hom.region <- rbind.fill(x)

hom.region <- ddply(hom.region, .(region), transform,
                    order = mean(V1))
hom.region$region <- reorder(hom.region$region, -hom.region$order)
p <- ggplot(hom.region, aes(date, V1)) +
       geom_vline(aes(xintercept = as.Date(ban)), color = "#000000",
               linetype = 2) +
       geom_line(size = 0.4) +
       geom_line(aes(date, trend), size = .3, color = "blue") +
       scale_x_date(major = "2 years") +
       facet_wrap(~ region) +
       xlab("date") + ylab("proportion of homicides by firearm") +
       opts(title="Proportion of Homicides by Firearm Discharge in the Different Regions of Mexico")+
       scale_y_continuous(formatter = "percent", limits = c(0, .8))
savePlotAA(p, "graphs/regions2.png")

########################################################
#For Municipalities near the US Border
########################################################
addTrend(hom.border, "prop", 1998)

p <- ggplot(hom.border, aes(date, prop)) +
       geom_vline(aes(xintercept = as.Date(ban)), color = "#000000",
               linetype = 2) +
       geom_line(size = 0.2) +
       geom_line(aes(date, trend), color = "blue") +
       scale_x_date(major = "2 years") +
       geom_vline(aes(xintercept = as.Date(ban)), color = "#000000",
               linetype = 2) +
       annotate("text", x = ban, y = .37, hjust = 1.03,
             label = "Assault weapon ban expiration") +
       xlab("date") + ylab("proportion of homicides by firearm") +
       opts(title="Homicides by Firearm Discharge as a Proportion of all Homicides in Mexican Municipalities that Border the US") +
       scale_y_continuous(formatter = "percent", limits = c(0, .9))
savePlotAA(p, "graphs/us-border.png")

########################################################
#For the big border cities
########################################################

x <- dlply(hom.borderct, .(Municipality2), transform, trendP = data.frame(stl(ts(prop, start = 2001, freq = 12), "per")$time.series)$trend )
hom.borderct <- rbind.fill(x)

x <- dlply(hom.borderct, .(Municipality2), transform, trendH = data.frame(stl(ts(Homicides, start = 2001, freq = 12), "per")$time.series)$trend )
hom.borderct <- rbind.fill(x)

hom.borderct <- ddply(hom.borderct, .(Municipality2), transform,
                    order = mean(prop))
hom.borderct$Municipality2 <- reorder(hom.borderct$Municipality2,
                                      -hom.borderct$order)

p <- ggplot(hom.borderct, aes(date, prop)) +
    geom_vline(aes(xintercept = as.Date(ban)), color = "#000000",
               linetype = 2) +
    geom_line(size = 0.2) +
    geom_line(aes(date, trendP), color = "blue") +
#    geom_line(aes(date, trend), color = "blue") +
    facet_wrap(~Municipality2) +
    scale_y_continuous(formatter = "percent") +
    opts(title = "Effect of the Expiration of the Assault Weapon Ban on the Proportion of Homicides\nCommited by Firearm in Mexican Cities that Border the US") +
    opts(axis.text.x=theme_text(angle=60, hjust=1.2 )) +
    ylab("proportion of homicides by firearm")
savePlotAA(p, "graphs/us-cities-prop.png")

hom.borderct <- ddply(hom.borderct, .(Municipality2), transform,
                    order = mean(Homicides))
hom.borderct$Municipality2 <- reorder(hom.borderct$Municipality2,
                                      -hom.borderct$order)

p <- ggplot(hom.borderct, aes(date, Homicides)) +
    geom_vline(aes(xintercept = as.Date(ban)), color = "#000000",
               linetype = 2) +
    geom_line(size = 0.2) +
    geom_line(aes(date, trendH), color = "blue") +
#    geom_line(aes(date, Firearm.Homicides), color = "#2BC258") +
    facet_wrap(~Municipality2)+
    opts(title = "Effect of the Expiration of the Assault Weapon Ban on the Number of Homicides Commited\nby Firearm in Mexican Cities that Border the US") +
    ylab("number of homicides by firearm")
savePlotAA(p, "graphs/us-cities-number.png")



p <- ggplot(subset(hom.borderct, Municipality2 == "NUEVO LAREDO"),
            aes(date, Homicides)) +
    geom_vline(aes(xintercept = as.Date(ban)), color = "#000000",
               linetype = 2) +
    geom_vline(aes(xintercept = fox.troops),
               color = "#000000",
               linetype = 2) +
    geom_vline(aes(xintercept = osiel.captured),
               color = "#000000",
               linetype = 2) +
    annotate("text", x = fox.troops, y = 23, label = "Troops\nin NL",
             hjust = 1.03) +
    annotate("text", x = ban, y = 17, hjust = 1.03,
             label = "Assault weapon ban\nexpiration") +
    annotate("text", x = osiel.captured, y = 12, hjust = 1.03,
             label = "Leader of Gulf Cartel\nCaptured") +
    #geom_line(color = "#F67D75") +
    geom_line(aes(date, Firearm.Homicides)) +
    ylab("number of homicides by firearm") +
    opts(title = "Number of Homicides by Firearm Discharge in Nuevo Laredo")
savePlotAA(p, "graphs/us-nuevo-laredo.png")


########################################################
#Small Multiples of all the states with breakpoints
########################################################
homsub <- subset(hom, Year >= kstart.year & Year <= kend.year)
breaks.df <- ddply(homsub, .(State), findBreakAW)
names(breaks.df) <- c("State", "low", "breakpoints", "up")
breaks.df$low[breaks.df$low < 0] <- 0
breaks.df$up[breaks.df$up > (knum.years * 12-1)] <- knum.years * 12-1
breaks.df$date <- convertToDate(as.numeric(breaks.df$breakpoints))
breaks.df$min <- convertToDate(as.numeric(breaks.df$low))
breaks.df$max <- convertToDate(as.numeric(breaks.df$up))
breaks.df$prop <- 0

x <- dlply(homsub, .(State), transform, trend = data.frame(stl(ts(prop, start = 2001, freq = 12), "per")$time.series)$trend )
homsub <- rbind.fill(x)

dts <- c("Chihuahua", "Sinaloa", "Durango", "Sonora",
         "Guerrero", "Baja California","Michoacán", "Tamaulipas")
st <- c("México", "Chiapas", "Puebla", "Nuevo León",
        "Quintana Roo")


filenames <- c("all", "chihuahua", "nuevo-leon", "michoacan","sonora", "dts", "interesting")
filenames <- sapply(filenames,
                    function(x) paste("graphs/", x,
                                      ".png", sep = ""))
widths <- c(960, 640, 640, 640, 640, 700, 700)
heights <- c(600, 480, 480, 480, 480, 525, 525)

mapply(function(x, y, z, height, width)
            savePlotAA(plotAsWeBreaks(homsub, breaks.df, ban, x, z), y,
                       height, width),
       list(NULL, "Chihuahua", "Nuevo León", "Michoacán","Sonora",
            dts, st), filenames,
       list(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE), widths,
       heights)

########################################################
#Number of Homicides by Firearm in Nuevo Leon
########################################################
nuevo.leon00 <- subset(homsub, homsub$State == "Nuevo León")
addTrend(nuevo.leon00, "Murders.with.Firearm", 2000)

p <- ggplot(nuevo.leon00, aes(date, Murders)) +
#    geom_line(color = "red") +
    geom_line(aes(date, trend), color = "blue") +
    geom_line(aes(date, Murders.with.Firearm)) +
    geom_vline(aes(xintercept = as.Date(ban)), color = "#000000",
               linetype = 2) +
    annotate("text", x = ban, y = 16, hjust = 1.05,
             label = "Assault weapon ban expiration") +
    ylab("number of homicides by firearm discharge") +
    opts(title = "Number of Homicides by Firearm Discharge in Nuevo Leon")
savePlotAA(p, "graphs/guns-num-nuevo-leon.png")

########################################################
#Number of Homicides by Firearm in Chihuahua
########################################################
chi00 <- subset(homsub, homsub$State == "Chihuahua")
addTrend(chi00, "Murders.with.Firearm", 2000)

p <- ggplot(chi00) +
#    geom_line(color = "red") +
    geom_line(aes(date, trend), color = "blue") +
    geom_line(aes(date, Murders.with.Firearm)) +
    geom_vline(aes(xintercept = as.Date(chapo.escape)), color = "#000000",
               linetype = 2) +
    annotate("text", x = chapo.escape, y = 47, hjust = 1.05,
             label = "\"El Chapo\" Escapes\nfrom Prision") +
    geom_vline(aes(xintercept = as.Date(ban)), color = "#000000",
               linetype = 2) +
    annotate("text", x = ban, y = 3, hjust = 1.05,
             label = "Assault weapon ban expiration\n(2004-Sep-14)") +
    geom_vline(aes(xintercept = as.Date(rodolfo.death)),
               color = "#000000",
               linetype = 2) +
    annotate("text", x = rodolfo.death, y = 47, hjust = 1.05,
             label = "Brother of the leader\nof the Juarez Cartel\nKilled by the Sinaloa Cartel\n(2004-Sep-11)") +
    ylab("number of homicides by firearm discharge") +
    scale_y_continuous(limits = c(0, 50)) +
    opts(title = "Number of Homicides by Firearm Discharge in Chihuahua")
savePlotAA(p, "graphs/guns-num-chihuahua.png", width = 800,
           height = 600)

########################################################
#Number of Homicides by Firearm in Chihuahua (including Calderon's
#drug war)
########################################################
p <- ggplot(chihuahua) +
#    geom_line(color = "red") +
    geom_line(aes(date, Murders.with.Firearm)) +
    geom_vline(aes(xintercept = as.Date(ban)), color = "#000000",
               linetype = 2) +
    annotate("text", x = ban, y = 220, hjust = 1.05,
             label = "Assault weapon ban expiration\n(2004-Sep-14)") +
    ylab("number of homicides by firearm discharge") +
    scale_y_continuous(limits =
                       c(0, max(chihuahua$Murders.with.Firearm))) +
    opts(title = "Monthly Number of Homicides by Firearm Discharge in Chihuahua(1998-2008)")
savePlotAA(p, "graphs/guns-num-chihuahua-1998-2008.png", width = 640, height = 480)


########################################################
#Number of Homicides in Nuevo Leon
########################################################
nuevo.leon00 <- subset(hom, hom$State == "Nuevo León")
homicides.nl <- c(nuevo.leon00$Murders, 8,
                  c(20, 16, 7, 26, 15, 13, 24, 32, 31,
                    26, 15, 42, 23, 29, 73, 101, 58, 102, 123))

p <- qplot(seq(as.Date("1998-01-15"), by='1 month',length=12*13-5),
      homicides.nl, geom = "line") +
    scale_x_date() +
    xlab("date") + ylab("number of homicides") +
    opts(title = "Monthly Number of Homicides in Nuevo Leon (1998-July 2010)\n(1998-2008 data from INEGI, 2009-2010 from the State Police)") +
    #geom_vline(aes(xintercept = as.Date(concord)), color = "#000000",
    #           linetype = 2) +
    #annotate("text", x = concord, y = 102, hjust = 1.05,
    #         label = "Zetas split\nwith Gulf Cartel") +
    geom_vline(aes(xintercept = as.Date(ban)), color = "#000000",
               linetype = 2) +
    annotate("text", x = ban, y = 42, hjust = 1.05,
             label = "Assault weapon ban expiration")
    #geom_vline(aes(xintercept = as.Date(op.tam.nl)),
    #           color = "#000000",
    #           linetype = 2) +
    #annotate("text", x = op.tam.nl, y = 62, hjust = 1.05,
    #         label = "Joint\nOperation\nTamaulipas-\nNuevo Leon")

savePlotAA(p, "graphs/homicides-num-nl-1998-2008.png", width = 640, height = 480)

