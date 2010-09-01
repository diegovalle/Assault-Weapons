extensions <- c("\\.csv$" = ",",
                "\\.tsv$" = "\t",
                "\\.wvs$" = " ",
                "\\.csv.bz2$" = ",",
                "\\.csv.zip$" = ",",
                "\\.csv.gz$" = ",",
                "\\.Rdata$" = "")
data.files <- dir('data')

for (data.file in data.files)
{
  filename <- file.path('data', data.file)

  for (extension in names(extensions))
  {
      if (grepl(extension, data.file, ignore.case = TRUE, perl = TRUE))
      {
        variable.name <- sub(extension, '', data.file,
                             ignore.case = TRUE,perl = TRUE)
        variable.name <- clean.variable.name(variable.name)
        separator <- extensions[[extension]]
        cat(paste("Loading data set: ", variable.name, '\n', sep = ''))
        if(separator != "")
        {
            assign(variable.name, read.csv(filename, header = TRUE,
                                       sep = separator))
        } else {
            load(filename)
        }
      }
  }
}
########################################################
#Load the daily data
########################################################
process <- function(x) {
    if(length(grep("04", x))) {
      cols <- c("ANODEF","MESDEF", "DIADEF", "CAUSAB")
    } else {
        cols <- c("ANIODEF","MESDEF", "DIADEF", "CAUSAB")
    }
    if(length(grep("0[678]", x))) {
        cols <- c("ANIODEF","MESDEF", "DIADEF", "CAUSADEF")
    }
    if(length(grep("03", x))) {
        cols <- c("ANODEF","MESDEF", "DIADEF", "CAUSA")
    }
    df <- read.csv(x)
    df <- subset(df, PRESUNTO == 2)
    df <- df[ ,cols]
    names(df) <- c("ANIODEF","MESDEF", "DIADEF", "CAUSAB")
    df
}

filenames <- paste("data/daily/0", 03:08, "laredo.csv.bz2", sep = "")
#filenames <- paste("0", 03:07, "nl.csv", sep = "")
lardaily <- lapply(filenames, process)
lardaily <- rbind.fill(lardaily)
