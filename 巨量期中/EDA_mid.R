library(RColorBrewer)
library(tidyverse)

yt <- read_csv("./USvideos.csv")

summary(yt)
#資料有40949筆
#最少觀看數的是549次  最多是225211923次

#最多倒讚數的影片是1674420個倒讚
#最多讚的有5613827個讚
#最多評論數量的 有1361580  筆評論


###前處理與偉豪的檔案相同
sapply(yt, function(x) sum(ifelse(is.na(x), 1, 0))) # check NA，discription 有 NA
yt[is.na(yt)] = "" # fill NA，將 discription NA 填為空字串

# load category JSON file，讀取類別 JSON 檔
cjson = fromJSON("./US_category_id.json"); cid = cjson$items$id; ctable = as.data.frame(cid); ctable$category = cjson$items$snippet$title
# add category name to youtube data frame，新增類別名稱欄位
youtube = merge(yt, ctable, by.x = "category_id", by.y = "cid") 
# trending_date to Date type，資料型態轉換
youtube$trending_date = as.Date(youtube$trending_date, format = "%y.%d.%m") 


# most views of each video，將每部影片最終的資料獨立出來，避免重複統計。
mostViews = group_by(youtube, video_id) %>% filter(views == max(views))

#############################



####################
#每個類別影片的數量(已將每部影片最終的資料獨立出來，避免重複統計。)
sum1 <-group_by(mostViews,category) %>%summarise(count=n(), views=sum(views), likes=sum(likes), comments=sum(comment_count), 
                    +                                                  dislikes=sum(dislikes))

ggplot(sum1, aes(fct_reorder(category, count), count, fill=category))+
  +     geom_bar(stat = "identity") +theme(axis.text.x = element_text(angle = 30, hjust = 1), 
                                           +                                        plot.title = element_text(hjust = 0.5)) +
  +     xlab('Video Category') +
  +     ylab("Counts") +
  +     ggtitle("Video Categories by Frequency")
#跑出來的結果可以看出 Entertainment 是影片出現最多次的類別，或許是娛樂的定義太廣了，所以所涵蓋的內容比其他類別多





##############################
#類別觀看次數統計
options(scipen = 999)
#取消科學符號 因為y軸數值太大會有科學符號
ggplot(sum1, aes(category, group=1)) +
  geom_line(aes(y=views)) + 
  theme(axis.text.x = element_text(angle = 30, hjust = 1), 
        plot.title = element_text(hjust = 0.5)) +
  xlab('Video Category') +
  ylab("Views") +
  ggtitle("Views by Video Category")

#可以看出 Music類別的觀看次數是最多的 其次是Entertainment



##########################
#likes.comments dislikes 對於category的統計圖
#
#可以看出likes的讚是最多的
#dislike是Entertainment多一些些
#comments也是 music最多
ggplot(sum1, aes(category, group=1)) +
    geom_line(aes(y=likes, color="likes")) +
  
    geom_line(aes(y=dislikes, color="dislikes")) + 
    geom_line(aes(y=comments, color="comments")) + 

    
     
    theme(axis.text.x = element_text(angle = 30, hjust = 1), 
              +           plot.title = element_text(hjust = 0.5)) +
    xlab('Video Category') +
    ylab("Counts") +
    ggtitle("Likes, Comments, Dislikes by Video Category")
