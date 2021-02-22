#' Time Series Plot
#'
#' @description This function creates a time series plot using gg plot of the date range
#' selected, and plots the number of  people who have died or been infected by COVID-19 in a specified region/regions.
#'
#' @import jsonlite
#' @import ggplot2
#' @import stringr
#' @import cowplot
#' @import tidyverse
#' @import ggrepel
#' @param apikey API token from api.pify
#' @param data character. The COVID-19 you wish to visualize. Use 'infected count', 'infected', 'deceased count' or
#' 'deceased'.
#' @param start_date The start date of the time series you wish to visualize. Written in yyyy-mm-dd format.
#' @param end_date The end date of the time series you wish to visualize. Written in yyyy-mm-dd format.
#' @param region1 The region you wish to visualize in the this time series plot, written in string format.
#' @param region2 The other regions you wish to visualize in the this time series plot written in string format.
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
#' @usage time_series_plot(apikey='4T9GEYHZ7PE9w8H29xynebW3L', data, start_date, end_date, region1, region2=NULL, region3=NULL,region4=NULL,region5=NULL,region6=NULL,region7=NULL,region8=NULL,region9=NULL,region10=NULL,region11=NULL,region12=NULL,region13=NULL,region14=NULL,region15=NULL,region16=NULL)
#' @return a GG plot time series of the date range, data and regions selected
#' @note Regions must be written in full names
#' @export
time_series_plot <- function(apikey='4T9GEYHZ7PE9w8H29xynebW3L', data, start_date, end_date, region1, region2=NULL, region3=NULL,region4=NULL,region5=NULL
                                ,region6=NULL,region7=NULL,region8=NULL,region9=NULL,region10=NULL,region11=NULL,region12=NULL
                                 ,region13=NULL,region14=NULL,region15=NULL,region16=NULL) {




suppressWarnings(if(!require(lubridate)) install.packages("lubridate", repos = "https://lubridate.tidyverse.org"))
suppressWarnings(if(!require(jsonlite)) install.packages('jsonlite', repos='http://cran.rstudio.com/'))
suppressWarnings(if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org"))
suppressWarnings(if(!require(stringr)) install.packages("stringr", repos = "http://cran.us.r-project.org"))
suppressWarnings(if(!require(plotly)) install.packages("plotly", repos = "http://cran.us.r-project.org"))
suppressWarnings(if(!require(cowplot)) install.packages("cowplot", repos = "https://wilkelab.org/cowplot/"))
suppressWarnings(if(!require(tidyverse)) install.packages("tidyverse", repos ="http://tidyverse.tidyverse.org"))
suppressWarnings(if(!require(ggrepel)) install.packages("ggrepel", repos = "https://github.com/slowkow/ggrepel"))


# if statement checks for correctness of input parameters. If this is not met, the error message is printed out in the else
if(((((data=='infected count') | (data=='infected')|(data=='deceased count') |  (data=='deceased'))) && (is.character(start_date))&&(is.character(end_date))&&(is.character(region1)))==TRUE){


# insert token into get_data_hist function to get all historical data
historical_clean_df<- Get_data(apikey='4T9GEYHZ7PE9w8H29xynebW3L')

# make a vector of the regions inputed as parameters

which_regions = c(region1,region2, region3,region4,region5,region6,region7,region8,region9,region10,region11,region12
                 ,region13,region14,region15,region16)


# subset historical data frame by the regions selected by user
new_df <- subset(historical_clean_df, region%in%which_regions)

# subset data frame again by start and end date specified by user
new_df2 <- subset(new_df, date<=end_date & date>=start_date)


# get most recent infected counts to add to end of time series graphs
data_ends <- new_df2 %>%
group_by(region) %>%
top_n(1, infectedCount)

# plot infected count time series plot using gg plot

infected_plot <- ggplot(new_df2, aes(x=date , y=infectedCount, color=region)) +
  geom_line()+ geom_line( size = 1)+
  xlab("Date") + ylab("Infected Count") +
  geom_text_repel(
    aes(label = infectedCount), data = data_ends,
    #size = 5,show.legend = FALSE) +theme_bw(base_size = 15) + labs(colour = "Region") + theme(axis.title.x = element_text(face="bold"), axis.title.y = element_text(face="bold")) + scale_y_continuous(labels = scales::comma)
    size = 5,show.legend = FALSE) +theme_bw(base_size = 15) + labs(colour = "Region") + theme(axis.title.x = element_text(face="bold"), axis.title.y = element_text(face="bold")) + scale_y_continuous(labels = comma)

# get most recent deceased counts to add to end of time series graphs
data_ends2 <- new_df2 %>%
group_by(region) %>%
top_n(1, deceasedCount)



# plot deceased count time series plot using gg plot
 desceased_plot <- ggplot(new_df2, aes(x=date , y=deceasedCount, color=region)) +
  geom_line()+ geom_line( size = 1)+
  xlab("Date") + ylab("Deceased Count") +
  geom_text_repel(
    aes(label = deceasedCount), data = data_ends2,
    #size = 5,show.legend = FALSE) +theme_bw(base_size = 15) + labs(colour = "Region")+ theme(axis.title.x = element_text(face="bold"), axis.title.y = element_text(face="bold")) + scale_y_continuous(labels = scales::comma)
    size = 5,show.legend = FALSE) +theme_bw(base_size = 15) + labs(colour = "Region")+ theme(axis.title.x = element_text(face="bold"), axis.title.y = element_text(face="bold")) + scale_y_continuous(labels = comma)









# return specified plot as output

if ((data=='infected count') | (data=='infected')){

    return (infected_plot)}

if ( (data=='deceased count') |  (data=='deceased')){



return(desceased_plot)}
}
   else{ print("Please enter valid parameters. Choose infected, infected count, deceased count or deceased for data in string format. Enter dates in yyyy-mm-dd string format. Enter valid regions of Canada in string format
")}





}


