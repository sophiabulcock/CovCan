#' Covid Daily Change
#'
#' @description This function creates a bar chart using gg plot of date selected, and plots the number of new people who have died and been infected by COVID-19 in a specified region, on this #'date .


#' @param date1 The date of the bar chart whose new COVID-19 data you wish to visualize. Written in yyyy=mm-dd format.
#' @param region1 The region whose new COVID-19 data you wish to visualize in the this bar chart, written in string format.
#' @param region2 The other regions whose new COVID-19 data you wish to visualize in the this bar chart, written in string format.
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
#' @usage day_covid_changes(date1, region1, region2=NULL, region3=NULL,region4=NULL,region5=NULL,region6=NULL,region7=NULL,region8=NULL,region9=NULL,region10=NULL,region11=NULL,region12=NULL, region13=NULL,region14=NULL,region15=NULL, region16=NULL)
#' @return Two GG plot bar charts of the number of new infections and deaths, on a specified date for the regions selected
#' @note Regions must be written in full names

day_covid_changes <- function(date1, region1, region2=NULL, region3=NULL,region4=NULL,region5=NULL
                                ,region6=NULL,region7=NULL,region8=NULL,region9=NULL,region10=NULL,region11=NULL,region12=NULL, region13=NULL,region14=NULL,region15=NULL, region16=NULL
                               ){


suppressWarnings(if(!require(jsonlite)) install.packages('jsonlite', repos='http://cran.rstudio.com/'))
suppressWarnings(if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org"))
suppressWarnings(if(!require(stringr)) install.packages("stringr", repos = "http://cran.us.r-project.org"))
suppressWarnings(if(!require(plotly)) install.packages("plotly", repos = "http://cran.us.r-project.org"))
suppressWarnings(if(!require(cowplot)) install.packages("cowplot", repos = "https://wilkelab.org/cowplot/"))
suppressWarnings(if(!require(tidyverse)) install.packages("tidyverse", repos ="http://tidyverse.tidyverse.org"))
suppressWarnings(if(!require(ggrepel)) install.packages("ggrepel", repos = "https://github.com/slowkow/ggrepel"))

# if statement checks for correctness of input parameters. If this is not met, the error message is printed out in the else
if(((is.character(date1))&&(is.character(region1)))==TRUE){
# insert token into daily_count function to get all historical data + calculations

df3<-daily_count()
# subset data frame based on date selected
new_df2 <- subset(df3, date==date1)

day <- date1

# make a vector with regions selected
which_regions2 = c(region1,region2, region3,region4,region5,region6,region7,region8,region9,region10,region11,region12
                 ,region13,region14,region15,region16)



# subset data frame based on regions selected
new_df <- subset(new_df2, region%in%which_regions2)


# plot daily deaths

dd <-ggplot(data=new_df, aes(x=region, y=Daily_deaths, fill=region)) +
  geom_bar(stat="identity")+
  xlab("Region of Canada") + ylab("Number of new people desceased from COVID-19") + labs(colour = "Region")+ggtitle(paste0('Date:  ',day))+theme_minimal()  + theme(axis.text.x=element_blank(),

                                                                                                                                                                                                          axis.ticks.y=element_blank(),legend.position='blank',  panel.grid.major = element_blank(),
panel.grid.minor = element_blank()) + geom_text(aes(label=Daily_deaths), position=position_dodge(width=0.9), vjust=-0.25)+ theme(axis.title.x = element_text(face="bold"), axis.title.y = element_text(face="bold"), plot.title = element_text(size=22)) + coord_flip()


# plot daily new cases
dnewc <-ggplot(data=new_df, aes(x=region, y=Daily_new_cases, fill=region)) +
  geom_bar(stat="identity")+
  xlab("Region of Canada") + ylab("Number of new people infected by COVID-19") + labs(colour = "Region")+theme_minimal()  + theme(axis.text.x=element_blank(),


                                                                                                       axis.ticks.y=element_blank(), legend.position='blank',  panel.grid.major = element_blank(),
panel.grid.minor = element_blank()) + geom_text(aes(label=deceasedCount), position=position_dodge(width=0.9), vjust=-0.25)+ theme(axis.title.x = element_text(face="bold"), axis.title.y = element_text(face="bold"))+ coord_flip()



# plot both plots in a grid
plots <- plot_grid(dd,dnewc, ncol=1)


return(plots)


} else{ print("Please enter valid parameters. Enter a date in yyyy-mm-dd string format. Enter valid regions of Canada in string format
")}





}

