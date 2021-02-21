# ---
#' Covid Changes Between Days
#'
#' @description This function creates a bar chart using gg plot of the two dates selected, and #'plots the number of people who have died and been infected by COVID-19 in a specified region, #'on these two dates.


#' @param day1 The first date of the bar chart that you wish to visualize and compare. Written in yyyy=mm-dd format.
#' @param day2 The second date of the bar chart that you wish to visualize and compare. Written in yyyy=mm-dd format.
#' @param region1 The region whose COVID-19 data you wish to visualize in the this comparative bar chart, written in string format.
#' @param region2 The other regions whose COVID-19 data you wish to visualize in the #'this comparative bar chart, written in string format.
#' Defaults to 'NULL'
#' @param region3 The other regions whose COVID-19 data you wish to visualize in the #'this comparative bar chart, written in string format.
#' Defaults to 'NULL'
#' @param region4 The other regions whose COVID-19 data you wish to visualize in the #'this comparative bar chart, written in string format.
#' Defaults to 'NULL'
#' @param region5 The other regions whose COVID-19 data you wish to visualize in the #'this comparative bar chart, written in string format.
#' Defaults to 'NULL'
#' @param region6 The other regions whose COVID-19 data you wish to visualize in the #'this comparative bar chart, written in string format.
#' Defaults to 'NULL'
#' @param region7 The other regions whose COVID-19 data you wish to visualize in the #'this comparative bar chart, written in string format.
#' Defaults to 'NULL'
#' @param region8 The other regions whose COVID-19 data you wish to visualize in the #'this comparative bar chart, written in string format.
#' Defaults to 'NULL'
#' @param region9 The other regions whose COVID-19 data you wish to visualize in the #'this comparative bar chart, written in string format.
#' Defaults to 'NULL'
#' @param region10 The other regions whose COVID-19 data you wish to visualize in the #'this comparative bar chart, written in string format.
#' Defaults to 'NULL'
#' @param region11 The other regions whose COVID-19 data you wish to visualize in the #'this comparative bar chart, written in string format.
#' Defaults to 'NULL'
#' @param region12 The other regions whose COVID-19 data you wish to visualize in the #'this comparative bar chart, written in string format.
#' Defaults to 'NULL'
#' @param region13 The other regions whose COVID-19 data you wish to visualize in the #'this comparative bar chart, written in string format.
#' Defaults to 'NULL'
#' @param region14 The other regions whose COVID-19 data you wish to visualize in the #'this comparative bar chart, written in string format.
#' Defaults to 'NULL'
#' @param region15 The other regions whose COVID-19 data you wish to visualize in the #'this comparative bar chart, written in string format.
#' Defaults to 'NULL'
#' @param region16 The other regions whose COVID-19 data you wish to visualize in the #'this comparative bar chart, written in string format.
#' Defaults to 'NULL'
#' @usage covid_changes_between_days(day1, day2, region1, region2=NULL, region3=NULL,region4=NULL,region5=NULL,region6=NULL,region7=NULL,region8=NULL,region9=NULL,region10=NULL,region11=NULL,region12=NULL,region13=NULL,region14=NULL,region15=NULL,region16=NULL)
#' @return Two GG plot bar charts of the COVID-19 data, for days and regions selected
#' @note Regions must be written in full names

covid_changes_between_days <- function(day1, day2, region1, region2=NULL, region3=NULL,region4=NULL,region5=NULL
                                ,region6=NULL,region7=NULL,region8=NULL,region9=NULL,region10=NULL,region11=NULL,region12=NULL
                                 ,region13=NULL,region14=NULL,region15=NULL,region16=NULL){


suppressWarnings(if(!require(jsonlite)) install.packages('jsonlite', repos='http://cran.rstudio.com/'))
suppressWarnings(if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org"))
suppressWarnings(if(!require(stringr)) install.packages("stringr", repos = "http://cran.us.r-project.org"))
suppressWarnings(if(!require(plotly)) install.packages("plotly", repos = "http://cran.us.r-project.org"))
suppressWarnings(if(!require(cowplot)) install.packages("cowplot", repos = "https://wilkelab.org/cowplot/"))
suppressWarnings(if(!require(tidyverse)) install.packages("tidyverse", repos ="http://tidyverse.tidyverse.org"))
suppressWarnings(if(!require(ggrepel)) install.packages("ggrepel", repos = "https://github.com/slowkow/ggrepel"))
require(httr)
# if statement checks for correctness of input parameters. If this is not met, the error message is printed out in the else
if(((is.character(day1))&&(is.character(day2))&&(is.character(region1)))==TRUE){


# insert token into daily_count function to get all historical data + calculations
df3<-daily_count()

# subset both selected days into their own data frames


df2 <- subset(df3, date==day1)

df3 <- subset(df3, date==day2)






# make regions selected into a vector
which_regions2 = c(region1,region2, region3,region4,region5,region6,region7,region8,region9,region10,region11,region12
                 ,region13,region14,region15,region16)



#subset both  data frames by regions selected
new_df2 <- subset(df2, region%in%which_regions2)

new_df3 <- subset(df3, region%in%which_regions2)



#bind both data frames together
both_days <- rbind(new_df2,new_df3)




#plot deceased count for both days and facet by region
plot <-  ggplot(both_days, aes(x = as.factor(format(date, format = "%Y-%m-%d")), y=deceasedCount, fill=date)) + geom_bar(stat = "identity", position = 'dodge')+labs(y="Deceased count")+ geom_text(aes(label=deceasedCount), position=position_dodge(width=0.9), vjust=-0.25)+ ggtitle(paste0('Comparison between ',day1,' and ',day2)) +facet_wrap(~region) + theme(legend.position = "none")  + theme(
                                                                                                        ,

                                                                                                        axis.text.y=element_blank(),axis.ticks.y=element_blank(), panel.grid.major = element_blank(),
panel.grid.minor = element_blank() , axis.title.x = element_text(face="bold"), axis.title.y = element_text(face="bold")) + scale_x_discrete(name ="Date")

#plot  infected count for both days and facet by region
plot2 <- ggplot(both_days, aes(x = as.factor(format(date, format = "%Y-%m-%d")), y=infectedCount, fill=date)) + geom_bar(stat = "identity", position = 'dodge')+labs(y="Infected count") +facet_wrap(~region)+ geom_text(aes(label=infectedCount), position=position_dodge(width=0.9), vjust=-0.25) + theme(legend.position = "none")  + theme(
                                                                                                        ,

                                                                                                        axis.text.y=element_blank(),axis.ticks.y=element_blank(), panel.grid.major = element_blank(),
panel.grid.minor = element_blank() , axis.title.x = element_text(face="bold"), axis.title.y = element_text(face="bold"))  + scale_x_discrete(name ="Date")

#arange in a grid
plots <- plot_grid(plot,plot2, ncol=1)


return(plots)



}else{ print(" Enter dates in yyyy-mm-dd string format. Enter valid regions of Canada in string format")}








}
