#' Current Data Bar CHart
#'
#' @description This function creates a bar chart plot using gg plot, and plots a bar chart of the number of people who have died or been infected by COVID-19 to date in a specified region/regions.
#' @import jsonlite
#' @import ggplot2
#' @import stringr
#' @import cowplot
#' @import tidyverse
#' @import ggrepel
#'
#' @param token apikey used to access data'
#' @param data character. The COVID-19 you wish to visualize. Use 'infected count', 'infected', 'deceased count',
#' 'deceased', 'both or 'Both'.
#' @param region1 The region you wish to visualize in the this current data bar chart, written in string format.
#' @param region2 The other regions you wish to visualize in this bar chart written in string format.
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
#' @usage current_data_barchart(token='4T9GEYHZ7PE9w8H29xynebW3L', data, region1, region2=NULL, region3=NULL,region4=NULL,region5=NULL,region6=NULL,region7=NULL,region8=NULL,region9=NULL,region10=NULL,region11=NULL,region12=NULL,region13=NULL,region14=NULL,region15=NULL,region16=NULL)
#' @return a GG plot bar chart of the current COVID-19 data for the specified regions
#' @note Regions must be written in full names
#' @export

current_data_barchart <- function(token='4T9GEYHZ7PE9w8H29xynebW3L', data, region1, region2=NULL, region3=NULL,region4=NULL,region5=NULL
                                ,region6=NULL,region7=NULL,region8=NULL,region9=NULL,region10=NULL,region11=NULL,region12=NULL
                                 ,region13=NULL,region14=NULL,region15=NULL,region16=NULL) {


suppressWarnings(if(!require(jsonlite)) install.packages('jsonlite', repos='http://cran.rstudio.com/'))
suppressWarnings(if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org"))
suppressWarnings(if(!require(stringr)) install.packages("stringr", repos = "http://cran.us.r-project.org"))
suppressWarnings(if(!require(plotly)) install.packages("plotly", repos = "http://cran.us.r-project.org"))
suppressWarnings(if(!require(cowplot)) install.packages("cowplot", repos = "https://wilkelab.org/cowplot/"))
suppressWarnings(if(!require(tidyverse)) install.packages("tidyverse", repos ="http://tidyverse.tidyverse.org"))
suppressWarnings(if(!require(ggrepel)) install.packages("ggrepel", repos = "https://github.com/slowkow/ggrepel"))



# if statement checks for correctness of input parameters. If this is not met, the error message is printed out in the else
if(((((data=='infected count') | (data=='infected')|(data=='deceased count') |  (data=='deceased')|(data=='both') |  (data=='Both'))) && (is.character(region1)))==TRUE){

# make a vector of the regions inputed as parameters
which_regions2 = c(region1,region2, region3,region4,region5,region6,region7,region8,region9,region10,region11,region12
                 ,region13,region14,region15,region16)

# get data from get_newest_data function
# source('R/get_data.r')
token='4T9GEYHZ7PE9w8H29xynebW3L'
current_df <- Getdata_syncing(token)




# subset based on regions
new_df <- subset(current_df, region%in%which_regions2)




# plot infected count


infected_count_current <-ggplot(data=new_df, aes(x=region, y=infectedCount, fill=region)) +
  geom_bar(stat="identity") +
  xlab("Region of Canada") + ylab("Number of people infected by COVID-19")+ labs(fill = "Region") + theme_minimal()  + theme(axis.text.x=element_blank(),
                                                                                                        axis.title.x=element_blank(),
                                                                                                       axis.text.y=element_blank(),
                                                                                                       axis.ticks.y=element_blank(), panel.grid.major = element_blank(),
panel.grid.minor = element_blank(), axis.line = element_line(colour = "black") ) + geom_text(aes(label=infectedCount), position=position_dodge(width=0.9), vjust=-0.25) + labs(fill = "Region") + theme(axis.title.x = element_text(face="bold"), axis.title.y = element_text(face="bold"))


# plot deceased count

deceased_count_current <-ggplot(data=new_df, aes(x=region, y=deceasedCount, fill=region)) +
  geom_bar(stat="identity") +
  xlab("Region of Canada") + ylab("Number of people deceased from COVID-19") + labs(fill = "Region")+ theme_minimal()  + theme(axis.text.x=element_blank(),
                                                                                                        axis.title.x=element_blank(),

                                                                                                        axis.text.y=element_blank(),axis.ticks.y=element_blank() ) + geom_text(aes(label=deceasedCount), position=position_dodge(width=0.9), vjust=-0.25) + labs(fill = "Region")+ theme(axis.title.x = element_text(face="bold"), axis.title.y = element_text(face="bold"), panel.grid.major = element_blank(),
panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))


# return specified plot as output
if ((data=='infected count') | (data=='infected')) {


return(infected_count_current)

        }

if ( (data=='deceased count') |  (data=='deceased')){

return(deceased_count_current)
}

if ( (data=='both') |  (data=='Both'))

plots <- plot_grid(infected_count_current, deceased_count_current, labels = c('Infected Count', 'Deceased Count'), label_size = 12, ncol=1)

return(plots)




}

     else{ print("Please enter valid parameters. Choose infected, infected count, deceased count,deceased or both for data in string format. Enter valid regions of Canada in string format
")}





}

