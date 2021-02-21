# library(dplyr)
# library(tidyr)
# library(ggplot2)
# library(devtools)
# library(mapcan)
# library(viridis)


# load dataset

suppressWarnings(if(!require(jsonlite)) install.packages('jsonlite', repos='http://cran.rstudio.com/'))
suppressWarnings(if(!require(viridis)) install.packages("viridis", repos = "https://github.com/sjmgarnier/viridis"))
suppressWarnings(if(!require(mapcan)) install.packages("mapcan", repos = "https://github.com/mccormackandrew/mapcan"))
require(httr)

covid_mapca<-function(stats,token="4T9GEYHZ7PE9w8H29xynebW3L"){
    #' covid_mapca
    #'
    #' @description This function will return a map of Canada with the selected data of either the current infected count or the current deceased count.
    #' @param stats the data you want to visualize on the map, either 'infectedCount' or 'deceasedCount'
    #' @param token API token from api.pify
    #' Defaults to "Tow8X4YNqnsWMFGbWxuPynzHh"

    #' @usage covid_mapca(stats,token)
    #' @return return a Canada map with select statistics either infected count or deceased count
    parameter <- c("Total Infected Number", "Totol Deceased Number")




    names(parameter) <- c("infectedCount", "deceasedCount")
    # test if user has enter a right parameter
    if(stats=="infectedCount" |stats=="deceasedCount"){
        name<-parameter[[stats]]
         newest = Getdata_syncing(token)
        #pr_geographic <- mapcan::mapcan(boundaries = province,type = standard)
        pr_geographic <- mapcan(boundaries = province,type = standard)
        pr_geographic <- left_join(pr_geographic, newest, by = c("pr_english" = "region"))
        pr_geographic$group<-as.numeric(pr_geographic$group)
        cnames <-aggregate(cbind(long, lat, group) ~pr_english, data=pr_geographic,
                    FUN=function(x) median(range(x)))

        # correct few points
        cnames[[9,3]]<-298113
        cnames[[6,2]]<--1195756
        cnames[[6,3]]<-2201249
        title=paste("Canada",name,"by Province")
        inner_join(cnames,newest,by=c("pr_english"="region"))->c_df
        pr_geographic%>%select(pr_english,pr_alpha)->n
        inner_join(c_df,n,by=c("pr_english"="pr_english"))->c_df
        # use ggplot to plot the map
        p<-pr_geographic %>%
            ggplot(aes(x = long, y = lat, group = group,fill=infectedCount)) +
            geom_polygon() +
            coord_fixed() +
            theme_mapcan() +
            scale_fill_continuous(trans = 'reverse')+
            ggtitle(title)+guides(color=guide_legend("infect"))+
            theme(plot.title = element_text(face = "bold"),legend.position = c(0.8, 0.6))
            #,legend.position = c(0.7, 0.7)
        p<-p+labs(fill=name)
        p<-p+geom_text(data=c_df, aes(long, lat, label =infectedCount),color="grey", size=2.5)
        p

    }
    else{"please enter either infectedCount or deceasedCount"}


}

