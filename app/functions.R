plotAsWe <- function(df){
  ggplot(df, aes(as.Date(date), prop, group = State)) +
    geom_line() +
    scale_x_date() +
    geom_smooth(method = "lm", formula = y ~ ns(x,3)) +
    opts(axis.text.x=theme_text(angle=60, hjust=1.2 )) +
    geom_vline(aes(xintercept = as.Date(ban)), color = "red") +
    facet_wrap(~ State, scale = "free_y")
}


findBreakAW <- function(df){
  rate <- ts(df$prop, start=kstart.year, freq=12)
  print(df$State[1])
  breakp <- breakpoints(rate ~ 1, h = 12, breaks = 1)
  x <- confint(breakp, breaks = 1)
  data.frame(x$confint)
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
    geom_vline(aes(xintercept = as.Date(date.y)),
               color = "red", alpha = .7) +
    geom_vline(aes(xintercept = as.Date(ban)), color = "#333333",
               linetype = 2) +
    geom_smooth(se = FALSE, aes(group = group), method = lm,
                alpha = .7, size = .5) +
    geom_line(size = .2) +
    scale_x_date(major="2 years") +
    opts(axis.text.x=theme_text(angle=60, hjust=1.2 )) +
    opts(title = plottitle.mult) +
    xlab("date") + ylab("proportion of homicides by firearm") +
    scale_y_continuous(formatter = "percent", limits = c(0, 1)) +
    facet_wrap(~ State)# +
    #geom_line(aes(date.x, trend), color = "blue")
  if (!se) {
      p
  } else {
      p + geom_rect(aes(xmin = as.Date(min),
              xmax = as.Date(max),
              ymin=-Inf, ymax=Inf),
              alpha = .01, fill = "#FFC8CB")
  }
}

