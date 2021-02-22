


daily_count <- function(){


 #' Daily Counts
    #'
    #' @description This function creates a calculates the daily new cases and death counts from the pre-existing requesting data from the covid dataset.
    #' @import ggplot2
    #' @importFrom lubridate ymd_hms
    #' @import tidyverse
    #' @usage daily_count()
    #' @return a data frame with columns: 'region', 'infectedCount', 'deceasedCount', 'date', 'Daily_new_cases', and 'Daily_deaths', 'seven_day_average'.
    #' @export

suppressWarnings(if(!require(lubridate)) install.packages("lubridate", repos = "https://lubridate.tidyverse.org"))
suppressWarnings(if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org"))
suppressWarnings(if(!require(tidyverse)) install.packages("tidyverse", repos = "http://tidyverse.tidyverse.org"))
    require(httr)


    token="4T9GEYHZ7PE9w8H29xynebW3L"
    cleaned_data <- Get_data(token)

    # Add column for daily case counts
    provin <- unique(cleaned_data$region)
    cd_daily <- list()

    # Compute daily counts for each region
    for(prov in provin){
        temp_df <- cleaned_data[cleaned_data$region == prov,]
        temp_vector_cases <- c(0)
        temp_vector_deaths <- c(0)

        # Compute the difference in cases and deaths between dates
        for(i in 1:(nrow(temp_df)-1)){
            temp_case <- temp_df[[i+1,2]]-temp_df[[i,2]]
            temp_deaths <- temp_df[[i+1,3]]-temp_df[[i,3]]
            temp_vector_cases[i+1] <- temp_case
            temp_vector_deaths[i+1] <- temp_deaths
        }
        temp_df$Daily_new_cases <- temp_vector_cases
        temp_df$Daily_deaths <- temp_vector_deaths


        cd_daily <- rbind(cd_daily,temp_df)
    }

    # Add column for 7 day average case count
    cd_comp <- list()

    # Compute daily counts for each region
    for(prov in provin){
        temp_vector_md7 <- c()
        temp_df <- cd_daily[cd_daily$region == prov,]

        # Compute 7 day average for all rows in each region
        for(i in 0:(nrow(temp_df))){
            temp_sum <- 0
            count <- 0

            # Get total number of cases over a 7 day period
            for(j in 0:6){

                # If there are less than 7 days in the count
                # Calculates the average using the number of daily case counts added to the sum
                if(i-j>0){
                    temp_sum <- temp_sum + cd_daily[[i-j,5]]
                    count <- count+1
                }
            }
            temp_vector_md7[i] <- round(temp_sum/count, digits = 0)
        }
        temp_df$seven_day_average <- temp_vector_md7
        cd_comp <- rbind(cd_comp,temp_df)
    }

    return(cd_comp)
}

