rm(list=ls()) # clean env
pacman::p_load("data.table", "tidyverse", "sqldf", "jsonlite", "corrplot") # load packages

#############################################################################

## 資料集說明 :
# - 來源 : [kaggle](https://www.kaggle.com/datasnaek/youtube-new)
# - 資料集 : youtube 發燒影片列表資料，有 US、CA、DE、FR、GB、IN、JP、KR、MX、RU，共 10 個國家的統計資料。
# - 資料集大小 : 40949 筆資料、16個欄位
# - 時間 : 2017-12-01 ~ 2018-05-31

## 欄位說明 :
# - video_id : 影片 ID
# - trending_date : 發燒日期
# - title : 影片標題
# - channel_title : 頻道標題
# - category_id : 類別 ID
# - publish_time : 影片發布時間
# - tags : 標籤
# - views : 觀看數
# - likes : 喜歡數
# - dislikes : 倒讚數
# - comment_count : 評論數
# - thumbnail_link : 影片縮圖連結
# - comments_disabled : 是否允許評論
# - ratings_disabled : 是否允許評分
# - video_error_or_removed : 影片錯誤或移除
# - description : 影片描述

## 關鍵指標
# 1. YouTube 參與度指標 (觀看次數、喜歡人數、不喜歡人數、訂閱數) 代表觀眾與影片或頻道互動的次數。這些指標可以視為衡量影片或頻道整體熱門程度的重要指標。([Youtube](https://support.google.com/youtube/answer/2991785?hl=zh-Hant)) <br><br>
# 2. 發燒影片會將許多指標納入參考，以下為其中幾項： ([Youtube](https://support.google.com/youtube/answer/7239739?hl=zh-Hant))
  # - 觀看次數
  # - 影片產生觀看次數的速度 (即「熱度」)
  # - 觀看次數的來源 (包括 YouTube 以外的來源)
  # - 影片已發布多久
  # - 與同頻道最近上傳的其他影片相比，影片的表現如何

############################################################################

yt = read_csv("./USvideos.csv") # read csv
cat("開始日期:", min(yt$trending_date), "結束日期:", max(yt$trending_date))
sapply(yt, function(x) sum(ifelse(is.na(x), 1, 0))) # check NA，discription 有 NA
yt[is.na(yt)] = "" # fill NA，將 discription NA 填為空字串

# load category JSON file，讀取類別 JSON 檔
cjson = fromJSON("./US_category_id.json"); cid = cjson$items$id; ctable = as.data.frame(cid); ctable$category = cjson$items$snippet$title
# add category name to youtube data frame，新增類別名稱欄位
youtube = merge(yt, ctable, by.x = "category_id", by.y = "cid") 
# trending_date to Date type，資料型態轉換
youtube$trending_date = as.Date(youtube$trending_date, format = "%y.%d.%m") 
# remove 清理環境
rm(list=c("cjson", "ctable", "yt", "cid")) 
# add trending_days column，新增每部影片上幾天熱門的欄位
youtube = group_by(youtube, video_id) %>% mutate(trending_days = n())
# most views of each video，將每部影片最終的資料獨立出來，避免重複統計。
mostViews = group_by(youtube, video_id) %>% filter(views == max(views))
# category dataframe，新增類別資料框，針對類別作分析
C = group_by(mostViews, category) %>% summarise(likes = mean(likes), dislikes = mean(dislikes), comment_count = mean(comment_count), views = mean(views), trending_days = mean(trending_days))
# C = C %>% mutate(likes_rate = likes / sum(likes), dislikes_rate = dislikes / sum(dislikes), comment_rate = comment_count / sum(comment_count))

### 單變數分析
# 什麼類別的影片上熱門的次數最多 : Entertainment、Music、Howto & Style、Comedy、People & Blogs
# Entertainment 上發燒影片最多次，且差距很大，娛樂是觀眾接受度最高且熱度最高的類別。
group_by(youtube, category) %>% summarise(n = n()) %>% arrange(desc(n)) %>% ggplot(aes(x = reorder(category, n), y = n, fill = category)) + geom_bar(stat = "identity") + coord_flip()
# 什麼類別的影片總觀看數最多 : Music、Entertainment、Film & Animation、Comedy、People & Blogs
group_by(mostViews, category) %>% summarise(total_views = sum(views)) %>% ggplot(aes(x = reorder(category, total_views), y = total_views, fill = category)) + geom_bar(stat = "identity") + coord_flip()
# 什麼類型的影片觀眾參與度最高(喜歡數 + 倒讚數 + 評論數) : Music、Entertainment、Comedy、People & Blogs、Howto & Style
group_by(mostViews, category) %>% summarise(engagement = sum(likes + dislikes + comment_count)) %>% arrange(desc(engagement)) %>% ggplot(aes(x = reorder(category, engagement), y = engagement, fill = category)) + geom_bar(stat = "identity") + coord_flip()

# 什麼類別的影片平均上熱門時間最久 : Shows、Gaming、Music、Film & Animation、Howto & Style
group_by(youtube, category) %>% summarise(days = mean(trending_days)) %>% ggplot(aes(x = reorder(category, days), y = days, fill = category)) + geom_bar(stat = "identity") + coord_flip()

### 雙變數分析
ggplot(C, aes(x = likes, y = dislikes)) + geom_point() + geom_smooth(method = "lm", se = F)

ggplot(C, aes(x = likes, y = comment_count)) + geom_point() + geom_smooth(method = "lm", se = F)
ggplot(C, aes(x = likes, y = views)) + geom_point() + geom_smooth(method = "lm", se = F)
ggplot(C, aes(x = likes, y = trending_days)) + geom_point() + geom_smooth(method = "lm", se = F)

ggplot(C, aes(x = dislikes, y = comment_count)) + geom_point() + geom_smooth(method = "lm", se = F)
ggplot(C, aes(x = dislikes, y = views)) + geom_point() + geom_smooth(method = "lm", se = F)
ggplot(C, aes(x = dislikes, y = trending_days)) + geom_point() + geom_smooth(method = "lm", se = F)
 
# 相關係數 > 0.8 高度正相關 0.5 ~ 0.8 顯著正相關 0.3 ~ 0.5 低度正相關
# likes 與 views、comment_count 有顯著正相關，對於喜歡影片的人來說是蠻正常的
# 但可以發現 dislikes 與 comment_count 有最高的正相關，似乎討人厭的影片評論區能夠引發觀眾論戰。
category_cor = cor(C[, -1])
corrplot(category_cor, method="number", type="upper")

### 多變數分析
# 各類別喜歡數、不喜歡數、評論數走勢
dfplot = C[, c(1:4)] %>% gather(key, value, -category)
ggplot(dfplot, aes(x = category, y = value, group = key, color = key)) + geom_line() + theme(axis.text.x = element_text(angle = 90))
# bubble plot
# 可以發現影片倒讚數多，即使喜歡數多，觀看數卻不會提升
ggplot(C, aes(x = likes, y = views, size = dislikes, color = category)) + geom_point(alpha = 0.7) + scale_size(range = c(.1, 24), name="dislikes")
# 喜歡比(likes / dislikes)與評論比(comment_count / views)
# 可以看出 Pets & Animals 有很高的喜歡比，大部分人都很喜歡動物
# Nonprofits & Activism (非營利組織和行動主義) e.g. "TED x Talks"
# Nonprofits & Activism 喜歡比低且評論比高，此類別影片可能較多爭議，很多觀眾討論
mutate(C, likes_prop = likes / dislikes, comment_prop = 1000 * (comment_count / views)) %>% ggplot(aes(x = category)) + geom_col(aes(y = likes_prop), size = 1, color = "darkblue", fill = "white") + geom_line(aes(y = comment_prop), size = 1.5, color="red", group = 1) + scale_y_continuous(sec.axis = sec_axis(~./100, name = "comment_prop")) + theme(axis.text.x = element_text(angle = 90))

