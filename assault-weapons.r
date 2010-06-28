########################################################
#####       Author: Diego Valle Jones
#####       Website: www.diegovalle.net
#####       Date Created: Fri Jun 11 12:22:27 2010
########################################################
#The effect of the assault weapons ban on the proportion of Firearms
#used to kill people
#Structural Change models and Graphics

library(ggplot2)
library(strucchange)
library(Cairo)
library(tseries)

#Run this function if you're having trouble because of the
#accents ·È
s <- function() source("assault-weapons.r")

cleanNames <- function(df, varname = "State"){
  df[[varname]] <- gsub("* de .*","", df[[varname]])
  df[[varname]]
}

plotAsWe <- function(df){
  ggplot(df, aes(as.Date(date), prop, group = State)) +
    geom_line() +
    scale_x_date() +
    geom_smooth(method = "lm", formula = y ~ ns(x,3)) +
    opts(axis.text.x=theme_text(angle=60, hjust=1.2 )) +
    geom_vline(aes(xintercept = as.Date(ban)), color = "red") +
    facet_wrap(~ State, scale = "free_y")
}

savePlotAA <- function(p, filename, width = 960, height = 600){
    Cairo(file = filename, width=width, height=height)
    print(p)
    dev.off()
    #dev.print(png, file = filename, width=width, height=height)
}

findBreakAW <- function(df){
  rate <- ts(df$prop, start=kstart.year, freq=12)
  print(df$State[1])
  breakp <- breakpoints(rate ~ 1, h = 12, breaks = 1)
  x <- confint(breakp, breaks = 1)
  data.frame(x$confint)
}

convertToDate <- function(x){
    as.Date(paste((x %% 12) + 1,"/",
                    "15", "/",
                    floor(x / 12) + kstart.year, sep =""), "%m/%d/%Y")
}

plotAsWeBreaks <- function(df, breaks.df, ban, sub = NULL,
                           se = FALSE) {
  if(!is.null(sub)){
      df <- subset(df, State %in% sub)
      breaks.df <- subset(breaks.df, State %in% sub)
  }
  df <- merge(df, breaks.df, by = "State")
  df$group <- df$date.x < df$date.y
  df$State <- reorder(df$State, abs(df$date.y - ban))
  p <- ggplot(df, aes(as.Date(date.x), prop.x, group = State)) +
    geom_line(size = .2) +
    scale_x_date(major="2 years") +
    geom_smooth(se = FALSE, aes(group = group), method = lm) +
    opts(axis.text.x=theme_text(angle=60, hjust=1.2 )) +
    geom_vline(aes(xintercept = as.Date(date.y)),
               color = "red", alpha = .7) +
    geom_vline(aes(xintercept = as.Date(ban)), color = "gray",
               linetype = 2) +
    opts(title = plottitle) +
    xlab("date") + ylab("proportion") +
    scale_y_continuous(formatter = "percent",
                       breaks = c(.25, .5, .75)) +
    facet_wrap(~ State)#, scale = "free_y")
  if (!se) {
      p
  } else {
      p + geom_rect(aes(xmin = as.Date(min),
              xmax = as.Date(max),
              ymin=-Inf, ymax=Inf),
              alpha = .01, fill = "#FFC8CB")
  }
}

#Constants
plottitle <- "Firearm Homicides as a Proportion of all Homicides (the Gray Line is the Assault Weapon Ban Expiration Date)"
bothtitle <-  "Monthly Homicide and Homicide with Firearm Rates with Trends (the Gray Line is the Assault Weapon Ban Expiration Date)"
kstart.year <- 2000
kend.year <- 2006
knum.years <- kend.year - kstart.year + 1
ban <- as.Date("2004/09/13", "%Y/%m/%d")

########################################################
#Read the data
########################################################
#hom <- read.csv(file("http://spreadsheets.google.com/pub?key=0AjjLwVYbDGx7dHdaMHhCQ21OZ1VzQk42WnltUXFkUWc&single=true&gid=0&output=csv",encoding = "UTF8"))
hom <- read.csv(bzfile("guns-month.csv.bz2"))
hom <- subset(hom, Month != "No Especificado")
hom <- hom[-grep("Total", hom$State),]
hom$Year <- rep(1998:2008, each = 12)
hom$State <- rep(subset(unique(hom$State),unique(hom$State) != ""),
                 each = 12*11)
hom$prop <- hom$Murders.with.Firearm / hom$Murders
hom$prop[is.nan(hom$prop)] <- 0
hom$m <- rep(1:12)
hom$date <- as.Date(paste(hom$Year,hom$m,"15", sep = "/"), "%Y/%m/%d")
hom$State <- cleanNames(hom, "State")
hom <- subset(hom, m != 12 | Year != 2008)

#Monthly Population Interpolation
pop <- read.csv("conapo-pop.csv")
pop2 <- data.frame(year = rep(1998:2008, each = 12),
                   month = rep(1:12))
pop2$Monthly.Pop[pop2$month == 6] <- unlist(pop[1,1:ncol(pop)])
pop2$Monthly <- na.spline(pop2$Monthly.Pop, na.rm=FALSE)


#Test for cointegration
unitRoot <- function(df){
    reg <- lm(df$Murders.with.Firearm ~ df$Murders)
    ht <- adf.test(residuals(reg))
    ht
}
#Looks ok
dlply(hom, .(State), unitRoot)

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
    geom_vline(aes(xintercept = as.Date(ban)), color = "gray70",
               linetype = 2) +
    opts(title = bothtitle) +
    xlab("date") + ylab("annualized monthly homicide rate")
savePlotAA(p, "output/mexico-hom-firearm.png")

########################################################
#For all of Mexico
########################################################
hom.mx <- ddply(hom, .(Year, m), function(df)
                sum(df$Murders.with.Firearm) / sum(df$Murders))
hom.mx$date <- as.Date(paste(hom.mx$Year,
                             hom.mx$m,"15", sep = "/"), "%Y/%m/%d")


hom.mx$trend <- data.frame(stl(ts(hom.mx$V1, start = 1998, freq = 12), "per")$time.series)$trend


rate <- ts(hom.mx$V1, start = kstart.year, freq = 12)
breakmx <- breakpoints(rate ~ 1, h = 12, breaks = 1)
breakconf <- confint(breakmx, breaks = 1)$confint
aw.break <- sapply(breakconf, convertToDate)

p <- ggplot(hom.mx, aes(date, V1)) +
    geom_line(size = .2) +
    geom_line(aes(date, trend), color = "blue") +
    geom_vline(aes(xintercept = as.Date(ban)), color = "gray70",
               linetype = 2) +
    opts(title = plottitle) +
    xlab("date") + ylab("proportion") +
    scale_y_continuous(formatter = "percent")
savePlotAA(p, "output/mexico.png")


########################################################
#Small Multiples of all the states with breakpoints
########################################################
hom <- subset(hom, Year >= kstart.year & Year <= kend.year)
breaks.df <- ddply(hom, .(State), findBreakAW)
names(breaks.df) <- c("State", "low", "breakpoints", "up")
breaks.df$low[breaks.df$low < 0] <- 0
breaks.df$up[breaks.df$up > (knum.years * 12-1)] <- knum.years * 12-1
breaks.df$date <- convertToDate(as.numeric(breaks.df$breakpoints))
breaks.df$min <- convertToDate(as.numeric(breaks.df$low))
breaks.df$max <- convertToDate(as.numeric(breaks.df$up))
breaks.df$prop <- 0

dts <- c("Chihuahua", "Sinaloa", "Durango", "Sonora",
         "Guerrero", "Baja California","Michoac·n", "Tamaulipas")
st <- c("Aguascalientes", "MÈxico", "Chiapas", "Morelos", "Nuevo LeÛn",
        "Quintana Roo")


filenames <- c("all", "sonora", "dts", "interesting")
filenames <- sapply(filenames,
                    function(x) paste("output/", x,
                                      ".png", sep = ""))


mapply(function(x, y, z)
            savePlotAA(plotAsWeBreaks(hom, breaks.df, ban, x, z), y),
       list(NULL, "Sonora", dts, st), filenames,
       list(TRUE, FALSE, FALSE, FALSE)
      )

