clean.variable.name <- function(variable.name)
{
  variable.name <- gsub('_', '.', variable.name, perl = TRUE)
  variable.name <- gsub('-', '.', variable.name, perl = TRUE)
  variable.name <- gsub('\\s+', '.', variable.name, perl = TRUE)
  return(variable.name)
}

cleanNames <- function(df, varname = "State"){
  df[[varname]] <- gsub("* de .*","", df[[varname]])
  df[[varname]]
}

savePlotAA <- function(p, filename, width = 800, height = 600){
    Cairo(file = filename, width=width, height=height)
    print(p)
    dev.off()
    #dev.print(png, file = filename, width=width, height=height)
}

convertToDate <- function(x){
    as.Date(paste((x %% 12) + 1,"/",
                    "15", "/",
                    floor(x / 12) + kstart.year, sep =""), "%m/%d/%Y")
}

defmacro <- function(..., expr) {
    expr <- substitute(expr)
    a <- substitute(list(...))[-1]
    ## process the argument list
    nn <- names(a)
    if (is.null(nn)) nn <- rep("", length(a))
    for(i in seq(length=length(a))) {
        if (nn[i] == "") {
            nn[i] <- paste(a[[i]])
            msg <- paste(a[[i]], "not supplied")
            a[[i]] <- substitute(stop(foo),
                                 list(foo = msg))
        }
    }
    names(a) <- nn
    a <- as.list(a)
    ## this is where the work is done
    ff <- eval(substitute(
                          function(){
                              tmp <- substitute(body)
                              eval(tmp, parent.frame())
                          },
                          list(body = expr)))
    ## add the argument list
    formals(ff) <- a
    ## create a fake source attribute
    mm <- match.call()
    mm$expr <- NULL
    mm[[1]] <- as.name("macro")
    attr(ff, "source") <- c(deparse(mm),
                            deparse(expr))
    ## return the 'macro'
    ff
}


