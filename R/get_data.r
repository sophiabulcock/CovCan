

# # token from api.apify
# token="Tow8X4YNqnsWMFGbWxuPynzHh"



Get_data<-function(apikey='4T9GEYHZ7PE9w8H29xynebW3L') {
    #' Get_data
    #'
    #' @description This function will return a well-formed dataframe that contains basic Covid information
    #' @importFrom lubridate ymd_hms
    #' @import jsonlite
    #' @import ggplot2
    #' @import stringr
    #' @import cowplot
    #' @import tidyverse
    #' @import ggrepel
    #' @import httr
    #' @import tidyverse
    #' @param apikey API token from api.pify
    #' Defaults to "Tow8X4YNqnsWMFGbWxuPynzHh"

    #' @usage Get_data(apikey)
    #' @return A dataframe with 4 columns region,infectedCount,deceasedCount and date
    #' @export
  # load libraries
  suppressWarnings(if(!require(ggplot2)) install.packages("stringr", repos = "http://cran.us.r-project.org"))
  suppressWarnings(if(!require(dplyr)) install.packages("dplyr", repos = "https://github.com/tidyverse/dplyr"))
  suppressWarnings(if(!require(plotly)) install.packages("plotly", repos = "http://cran.us.r-project.org"))
  suppressWarnings(if(!require(httr)) install_github("r-lib/httr"))
  suppressWarnings(if(!require(lubridate)) install.packages("lubridate", repos = "https://lubridate.tidyverse.org"))
  suppressWarnings(if(!require(jsonlite)) install.packages("jsonlite", repos = "https://arxiv.org/abs/1403.2805"))
  suppressWarnings(if(!require(tidyverse)) install.packages("tidyverse", repos = "http://tidyverse.tidyverse.org"))
  require(httr)

    url<-"https://api.apify.com"
    path<-"v2/acts/lukass~covid-cad/run-sync-get-dataset-items"
    params=list(token =apikey,limit=1)
    raw.result=GET(url=url,path=path,query=params)

    # test if it has sucessfully connected
    if(http_status(raw.result)$category=="Success"){
        # get the Json data
        data = fromJSON(rawToChar(raw.result$content))
        # getting historical data
        his_raw=GET(data$historyData)
        # get the Json format historical data
        data = fromJSON(rawToChar(his_raw$content))
        data%>%dplyr::select(infectedByRegion,lastUpdatedAtApify)%>%unstack()->df
        for(i in 1:length(df)){
            if (max(df[i][[1]]$deceasedCount,na.omit=T)==0){
                print(df[i][1])
                df[i][[1]]<-transform(df[i][[1]], infectedCount=ifelse(deceasedCount == 0,NA,infectedCount),deceasedCount = ifelse(deceasedCount == 0, infectedCount,deceasedCount))

            }
        }
        # make a dataframe
        df<-do.call(what = "rbind", args = lapply(df, as.data.frame))
        # data cleaning for datetime
        df$datetime<-row.names(df)
        str_replace_all(df$datetime,'T',' ')->new
        df$datetime<-gsub("\\..*","",new)
        df$datetime<-ymd_hms(df$datetime,tz=Sys.timezone())
        # repalce row name to numeric index
        rownames(df) <- 1:nrow(df)
        df$infectedCount<-as.numeric(df$infectedCount)
        df$deceasedCount<-as.numeric(df$deceasedCount)
        df%>%na.omit()%>%mutate(date = as.Date(datetime))->df_new
        df_rem <- subset(df_new, select = -c(datetime))%>%unique()
        df_uni <- df_rem %>%group_by(region, date) %>%slice(n())
        provin <- unique(df_uni$region)
        cleaned_data <- list()
        for(prov in provin){
            temp_df <- df_uni[df_uni$region == prov,]
            for(i in 1:(nrow(temp_df)-1)){
                index <- i
                if((temp_df[[index,2]] > temp_df[[index+1,2]])|(temp_df[[index,3]] > temp_df[[index+1,3]])){
                    temp_df[[index+1,2]] <- temp_df[[index,2]]
                    temp_df[[index+1,3]] <- temp_df[[index,3]]
                }
            }
            cleaned_data <- rbind(cleaned_data,temp_df)
        }

        return(cleaned_data)
    }
    # print out the error message
    else{
        print(http_status(raw.result)$message)
    }


}


# get newest dataset
Getdata_syncing<-function(apikey='4T9GEYHZ7PE9w8H29xynebW3L') {
    #' Getdata_syncin
    #'
    #' @description This function will return a latest veresion of well-formed dataframe that contains basic Covid information

    #' @param apikey API token from api.pify
    #' Defaults to "Tow8X4YNqnsWMFGbWxuPynzHh"
    #' @import httr
    #' @usage Getdata_syncing(apikey)
    #' @return A dataframe with 4 columns region,infectedCount,deceasedCount and date

    url<-"https://api.apify.com"
    path<-"v2/acts/lukass~covid-cad/run-sync-get-dataset-items"
    params=list(token = apikey,limit=1)
    raw.result=GET(url=url,path=path,query=params)

    # test if it has sucessfully connected
    if(http_status(raw.result)$category=="Success"){
        # get the Json data
        data = fromJSON(rawToChar(raw.result$content))
        data[1:3]$infectedByRegion%>%as.data.frame()->df
        data[1:3]$infectedByRegion%>%as.data.frame()->df
        df$deceasedCount<-as.numeric(df$deceasedCount)
        df$infectedCount<-as.numeric(df$infectedCount)
        return(df)

    }
    #print out the error message
    else{
        print(http_status(raw.result)$message)
    }

 }

