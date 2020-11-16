# setwd("~/GitHub/bigdata_team_project/巨量期中")
rm(list=ls()) # clean env
pacman::p_load("data.table", "tidyverse", "sqldf", "jsonlite", "corrplot", "d3heatmap") # load packages
load("./pre_process.rdata") # load pre process data : youtube and mostViews

# channel of most frequently appearing in trending videos : 前 10 最常上熱門的頻道
group_by(youtube, channel_title) %>% summarise(n = n()) %>% arrange(desc(n)) %>% head(10) %>% ggplot(aes(x = reorder(channel_title, n), y = n, fill = channel_title)) + geom_bar(stat = "identity") + coord_flip()

# video publish time : 下午 4 點左右為影片發布熱點
mutate(youtube, hour = hour(publish_time)) %>% group_by(hour) %>% summarise(n = n()) %>% ggplot(aes(x = hour, y = n)) + geom_line() + scale_x_continuous(breaks=c(0:23)) + labs(x = "hour", y = "number of videos") + geom_vline(xintercept = 16 , color = "#ff2244", size = 1)

# Number of videos by weekdays
mutate(youtube, weekday = weekdays(youtube$publish_time)) %>% group_by(weekday) %>% summarise(n = n()) %>% ggplot(aes(x = weekday, y = n))
weekdays(youtube$publish_time)

min(mostViews$trending_date)
