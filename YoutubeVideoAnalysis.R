# Kaggle Videos
# install the required packages
library(ggplot2)
library(data.table)
library(dplyr)

# read the csv file
youtube_videos <- read.csv("USvideos.csv", stringsAsFactors=FALSE, na.strings=c("","NA"))

# Summary of the dataset
summary(youtube_videos)

# save the dataframe in dfVideos object
dfVideos <- data.frame(youtube_videos)

# Remove blank columns
dfVideos <- dfVideos[,1:11]

# Structure of the dataset
str(dfVideos)

# Convert comment_total to int type
dfVideos$comment_total <- as.integer(dfVideos$comment_total)
# Replace string with category id 16 and 17
dfVideos$category_id[2401] <- dfVideos$category_id[16]
dfVideos$category_id[2801] <- dfVideos$category_id[17]
# Replace NA with category id 19
dfVideos[is.na(dfVideos$category_id),]$category_id <- "19"


#------------------------------ which category is most popular based on their views and likes --------------
# total sum of video views by category id
popular_Vcategory <- tapply(dfVideos$views, dfVideos$category_id, sum)

# total sum of video likes by category id
popular_Lcategory <- tapply(dfVideos$likes, dfVideos$category_id, sum)

# combine above result by columns 
popular_category  <- cbind.data.frame(popular_Vcategory,popular_Lcategory)

# set rownames as column
setDT(popular_category , keep.rownames = TRUE)[]

# set column names
setnames(popular_category, old=c("rn","popular_Vcategory","popular_Lcategory"), new=c("Cat_ID", "Total_Views","Total_Likes"))

#convert category id from integer type to factor type
popular_category$Cat_ID <- as.factor(popular_category$Cat_ID)

# Normalize views and likes values to Million
popular_category$Total_Views <- round(popular_category$Total_Views/1000000, digit = 1)
popular_category$Total_Likes <- round(popular_category$Total_Likes/1000000, digit = 1)

# plot graph
ggplot(popular_category, aes(Cat_ID, Total_Views))+ 
  geom_point(aes(color =Total_Likes))+
  labs(title="Category Popularity Based on User Views and Likes",
       subtitle = "Views and Likes in Millions",
       x="Category ID",
       y="Total Views")



#----------------4D view between Channels and their likes, views and comments, and best fit lines-----------------------------
# Compare channels("ESPN", "BBC Earth", "HBO", "MTV") total no. of likes, views and comments 
new_cat <- dfVideos[dfVideos$channel_title %in% c("ESPN", "BBC Earth", "HBO", "MTV"), ]

# plot graph
ggplot(new_cat, aes(likes, views)) + 
  labs(subtitle="BBC Earth,ESPN,HBO,MTV",
       title="Likes VS Views")+
  geom_jitter(aes(col=channel_title, size=comment_total)) + 
  geom_smooth(aes(col=channel_title), method="lm", se=F)

 
#---------------Categorization as "Above Average and Below Average" of each video view of popular channels------------------
# Compare top channels views as below average or above average with their mean value
new_title <- dfVideos[dfVideos$channel_title %in% c("FBE", "Dude Perfect", "JennaMarbles", "NigaHiga","Smosh","Veritasium","PewDiePie","SciShow","Watchmojo.com","Rooster Teeth"), ]

# Calculate mean value, normalize views value by subtracting with the mean and round to 2 decimal place
new_title$views_n <- round((new_title$views-mean(new_title$views)),2)

# Categorize normalized value as below average or above average
new_title$views_type <- ifelse(new_title$views_n <= 0 , "below", "above")
new_title <- new_title[order(new_title$views_n), ]

# plot graph
ggplot(new_title, aes(x=channel_title, y=views_n, label=views_n)) + 
  geom_bar(stat='identity', aes(fill=views_type), width=.5)  +
  scale_fill_manual(name="Views", 
                    labels = c("Above Average", "Below Average"), 
                    values = c("above"="#85C1E9", "below"="#EC7063")) + 
  labs(subtitle="Classified as Above average and Below average", 
       title= "Normalised views for popular video channels",
       x="Channels",
       y="Views Comparison") + 
  coord_flip()


#---------Total number of channels in different tags --------------------------------------------
# Save tags name in x
x <- c("Travel","Entertainment","Music","Fashion")

#Extract rows where tags are matching
new_tags <- dfVideos[grep(paste(x, collapse = "|"),dfVideos$tags,ignore.case = TRUE),]

# Add a new column tag_type
new_tags$tag_type <- ifelse(grepl("Travel",new_tags$tags,ignore.case = TRUE), "Travel", ifelse(grepl("Entertainment",new_tags$tags,ignore.case = TRUE), "Entertainment", ifelse(grepl("Music",new_tags$tags,ignore.case = TRUE), "Music","Fashion")))

# count channels under each tag_type
count_channels <- new_tags %>%
  group_by(tag_type) %>%
  summarize(n_unique = n_distinct(channel_title))

# plot graph
ggplot(count_channels, aes(tag_type, n_unique))+
  geom_bar(stat='identity', fill="maroon") + 
  geom_text(aes(label=n_unique), vjust=0)+
  labs(title="Total number of channels under different tags", 
    subtitle="Tags - Entertainment, Fashion,Music, Travel",
    x="Tags",
    y="Count")


#------------------------------Percentage of Like, Views, Dislikes and Comments of Netflix videos channels-----------------
# Select all Netflix videos
netflix_popularity <- dfVideos[grepl("Netflix",dfVideos$tags, ignore.case = TRUE),]

# Add video likes by channels
totalLike <- aggregate(netflix_popularity$likes ~ netflix_popularity$channel_title, netflix_popularity, sum)

# Add video views by channels
totalViews <- aggregate(netflix_popularity$views ~ netflix_popularity$channel_title, netflix_popularity, sum)

# Add video dislikes by channels
totalDislike <- aggregate(netflix_popularity$dislikes ~ netflix_popularity$channel_title, netflix_popularity, sum)

# Add video comments by channels
totalComments <- aggregate(netflix_popularity$comment_total ~ netflix_popularity$channel_title, netflix_popularity, sum)

# combine above results by columns
newdf <- cbind.data.frame(totalLike,totalViews,totalDislike,totalComments)

# Select required columns
newdf <- newdf[,c(1,2,4,6,8)]

# Change column names with old column names
colnames(newdf)<-c("Channel","Likes","Views","Dislikes","Comments")

# Total likes, views, dislikes and comments by channels 
newdf$Tot<-newdf$Likes + newdf$Views + newdf$Dislikes + newdf$Comments

# Likes percentage 
newdf$LikesPercent<-(newdf$Likes/newdf$Tot)*100

# Views percentage
newdf$ViewsPercent<-(newdf$Views/newdf$Tot)*100

# Dislikes percentage
newdf$DislikesPercent<-(newdf$Dislikes/newdf$Tot)*100

# Comments percentage
newdf$CommentsPercent<-(newdf$Comments/newdf$Tot)*100

# Select required columns
newfdfinal<-newdf[,-2:-6]

# melt columns based on channel
newdffinalmelt <- melt(newfdfinal, id.vars="Channel")

# plot graph
ggplot() + geom_bar(aes(y=value, x = Channel, fill=variable), data=newdffinalmelt, stat="identity")+
  theme(axis.text.x=element_text(angle=90, hjust=1))+
  labs(title="Percent of likes,dislikes,views and comments", 
       subtitle="By Netflix Channels",
       x="Channel",
       y="Percent")

