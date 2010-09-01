########################################################
#Clean homicide data
########################################################
#hom <- read.csv(file("http://spreadsheets.google.com/pub?key=0AjjLwVYbDGx7dHdaMHhCQ21OZ1VzQk42WnltUXFkUWc&single=true&gid=0&graphs=csv",encoding = "UTF8"))
#hom <- read.csv(bzfile("data/guns-month.csv.bz2"))
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
#pop <- read.csv("data/conapo-pop.csv")
pop2 <- data.frame(year = rep(1998:2008, each = 12),
                   month = rep(1:12))
pop2$Monthly.Pop[pop2$month == 6] <- unlist(pop[1,1:ncol(pop)])
pop2$Monthly <- na.spline(pop2$Monthly.Pop, na.rm=FALSE)





########################################################
#Suicide Data
########################################################
#sui <- read.csv("data/suicides.csv")
sui <- subset(sui, Month != "No Especificado")
sui$Year <- rep(1998:2008, each = 12)
sui$m <- rep(1:12)
sui$prop <- sui$Firearm.Suicides / sui$Suicides

sui$date <- as.Date(paste(sui$Year,
                             sui$m,"15", sep = "/"), "%Y/%m/%d")

########################################################
#Regions of Mexico (!THE DATA HAS A CODING ERROR AND IS
#USELESS)
########################################################
#hom.region <- read.csv("data/regions.csv")
hom.region$Region <- rep(c("CE Center-East",
"CW Center West", "NE North East", "No disponible", "NW Northwest",
"S South"), each = 13*11)
hom.region <- subset(hom.region, Month != "No Especificado" &
                                 Region != "No disponible")
hom.region$date <- seq(as.Date("1998-01-15"), by='1 month',
                       length=12*11)
hom.region$prop <- hom.region$Firearm.Homicide / hom.region$Homicide

########################################################
#Municipalities near the US Border
########################################################
#hom.border <- read.csv("data/frontier.csv")
hom.border$date <- seq(as.Date("1998-01-15"), by='1 month',
                       length=12*11)
hom.border$prop <- hom.border$Firearm.Homicide / hom.border$Homicide


########################################################
#Big Border Cities
########################################################
#2BC258 619CFF F67D75
#hom.borderct <- read.csv("data/border-cities.csv")
hom.borderct <- subset(hom.borderct, Month != "No Especificado")
hom.borderct$Year <- rep(2001:2006, each = 12)
muns <- unique(hom.borderct$Municipality)[unique(hom.borderct$Municipality) != ""]
hom.borderct$Municipality2 <- rep(muns, each=12*6)
hom.borderct$Municipality2 <- gsub("[0-9]+ ", "",
                                   hom.borderct$Municipality2)
hom.borderct$MonthN <- c(1:12)
hom.borderct$date <- seq(as.Date("2001-01-15"), by='1 month',
                       length=12*6)
hom.borderct$prop <- with(hom.borderct,
                            Firearm.Homicides / Homicides)
#hom.borderct$prop[is.na(hom.borderct$prop)] <- 0
hom.borderct[6:11][is.na(hom.borderct[6:11])] <- 0

########################################################
#Chihuahua
########################################################

chihuahua$date <- seq(as.Date("1998-01-15"), by='1 month',
                       length=12*11)

