#JZ-tools.R

biggerText <- function(x=14) theme(text = element_text(size=x))

library(showtext)
# https://fonts.google.com/featured/Superfamilies
font_add_google("Montserrat", "Montserrat")
font_add_google("Source Code Pro", "scp")
font_add_google("Lato", "Lato")
font_families()
showtext_auto() # needed to use Google Fonts above


# Inspiration from https://www.c82.net/werner/
werner <- c("#2c273c",
            "#804735",
            "#af342c",
            "#543854",
            "#211f20",
            "#332d49",
            "#8799a1",
            "#818745",
            "#bb7844")

# Thanks to https://github.com/kbroman/broman/blob/master/R/brocolors.R
CCalt <- c("AJ"  = "#FFDC00",
           "B6"  = "#888888",
           "129" = "#F08080",
           "NOD" = "#0064C9",
           "NZO" = "#7FDBFF",
           "CAST"= "#2ECC40",
           "PWK" = "#FF4136",
           "WSB" = "#B10DC9")

jzc <- c("#39425C","#6785D6","#BDB2D6","#8CDACA","#A4CED0","#41413b","#35564d","#9aaaa6","#F4B18E","#41413b")
jzc2 <- c("deepskyblue4","darkslateblue","cadetblue4","turquoise4","mediumpurple4")
jzc3 <- c("#74658A", "#D6A5CF","#92BCD2","#637C8A","#D6A5A8")
jz_brewer3cats <- c("#1B9E77","#D95F02","#7570B3")
jz_brewer <- c("#762A83","#C2A5CF","#D9F0D3","#A6DBA0","#5AAE61","#1B7837")
partycolors1 <- RColorBrewer::brewer.pal(5,"Set1")
partycolors2 <- c(partycolors1[2],partycolors1[1],partycolors1[3],partycolors1[4],partycolors1[5])


theme_jz <- function(font = "sans", fontsize = 14) {
  # good font options: Lato, Montserrat, and scp
  ggplot2::theme(
    panel.background = element_blank(),
    # set font
    text = element_text(size = fontsize, family = font),
    legend.background = element_blank(),
    # nice to move the y-axis label up
    axis.title.y = element_text(hjust=.9),
    # and center the title and make it .9x the rest of the text
    plot.title = element_text(hjust = 0.5, size = rel(1.1)),
    plot.subtitle = element_text(hjust = 0.5,size = rel(0.95)),
    legend.key = element_blank(),
    # legend.title = element_blank(),
    # turn off the background of the facet titles
    # strip.background = element_blank(),
    # a bit of space around the plot
    plot.margin = unit(c(1, 1, 1, 1), "lines")
  )
}


# Extract coefficients
extract_term <- function(obj, var) {
  obj %>% tibble %>% filter(factor %in% {{var}})
}


rescale01 <- function(x) {
  rng <- range(x, na.rm = TRUE)
  (x - rng[1]) / (rng[2] - rng[1])
}

r1sd <- function(x) {
  M <- mean(x,na.rm=T)
  sdx <- sd(x,na.rm=T)
  (x-M) / sdx
}

r2sd <- function(x, na.rm=T) {return ((x-mean(x,na.rm=na.rm))/(2*sd(x, na.rm=na.rm)))}

