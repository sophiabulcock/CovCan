#' Time Series Plot
#'
#' @description This function creates a time series plot using gg plot of the date range
#' selected, and plots the number of  people who have died or been infected by COVID-19 in a specified region/regions.

#' @param data character. The COVID-19 you wish to visualize. Use 'infected count', 'infected', 'deceased count' or
#' 'deceased'.
#' @param start_date The start date of the time series you wish to visualize. Written in yyyy-mm-dd format.
#' @param end_date The end date of the time series you wish to visualize. Written in yyyy-mm-dd format.
#' @param region1 The region you wish to visualize in the this time series plot, written in string format.
#' @param region2 to region 16 The other regions you wish to visualize in the this time series plot written in string format.
#' Defaults to 'NULL'
#' @usage time_series_plot(data, start_date, end_date,region1 )
#' @return a GG plot time series of the date range, data and regions selected
#' @note Regions must be written in full names


time_series_plot <- function(data, start_date, end_date, region1, region2=NULL, region3=NULL,region4=NULL,region5=NULL
                                ,region6=NULL,region7=NULL,region8=NULL,region9=NULL,region10=NULL,region11=NULL,region12=NULL
                                 ,region13=NULL,region14=NULL,region15=NULL,region16=NULL) {



# install.packages('ggrepel')
#     library(ggrepel)
# install.packages('tidyverse')
# library(tidyverse)
# install.packages('stringr')
# library(stringr)
# install.packages('ggplot2')
# library(ggplot2)
# install.packages('cowplot')
# library(cowplot)

if(!require(lubridate)) install.packages("lubridate", repos = "https://lubridate.tidyverse.org")
if(!require(jsonlite)) install.packages('jsonlite', repos='http://cran.rstudio.com/')
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(stringr)) install.packages("stringr", repos = "http://cran.us.r-project.org")
if(!require(plotly)) install.packages("plotly", repos = "http://cran.us.r-project.org")
if(!require(cowplot)) install.packages("cowplot", repos = "	https://wilkelab.org/cowplot/")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://tidyverse.tidyverse.org")
if(!require(ggrepel)) install.packages("ggrepel", repos = "https://github.com/slowkow/ggrepel")
#source('get_data.r')


# if statement checks for correctness of input parameters. If this is not met, the error message is printed out in the else
if(((((data=='infected count') | (data=='infected')|(data=='deceased count') |  (data=='deceased'))) && (is.character(start_date))&&(is.character(end_date))&&(is.character(region1)))==TRUE){


# insert token into get_data_hist function to get all historical data
token="Tow8X4YNqnsWMFGbWxuPynzHh"
historical_clean_df<- Get_data(token)

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
    size = 5,show.legend = FALSE) +theme_bw(base_size = 15) + labs(colour = "Region") + theme(axis.title.x = element_text(face="bold"), axis.title.y = element_text(face="bold")) + scale_y_continuous(labels = scales::comma)

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
    size = 5,show.legend = FALSE) +theme_bw(base_size = 15) + labs(colour = "Region")+ theme(axis.title.x = element_text(face="bold"), axis.title.y = element_text(face="bold")) + scale_y_continuous(labels = scales::comma)









# return specified plot as output

if ((data=='infected count') | (data=='infected')){

    return (infected_plot)}

if ( (data=='deceased count') |  (data=='deceased')){



return(desceased_plot)}
}
   else{ print("Please enter valid parameters. Choose infected, infected count, deceased count or deceased for data in string format. Enter dates in yyyy-mm-dd string format. Enter valid regions of Canada in string format
")}





}


