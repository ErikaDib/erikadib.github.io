youtube <- read.csv("https://raw.githubusercontent.com/ErikaDib/erikadib.github.io/master/data.csv",
header = TRUE)
mean(youtube$Video.views)
mean(youtube$Subscribers, na.rm=TRUE)
mean(youtube$Video.Uploads, na.rm=TRUE)
sd(youtube$Video.views)
sd(youtube$Subscribers, na.rm=TRUE)
sd(youtube$Video.Uploads, na.rm=TRUE)
plot(youtube$Video.Uploads, col="blue", xlab="Channels", ylab="Channel total video uploads", main="Total uploads per YouTube Channel", col.main="red")
plot(youtube$Subscribers/10000000, col="red", xlab="Channels", ylab="Channel total subscribers in Millions", main="Total subscribers per YouTube Channel", col.main="Gray")
plot(youtube$Video.views/10000000000, axes=FALSE, col="red", xlab="Channels", ylab="Channel video views", main="Total video views per YouTube Channel", col.main="blue")
axis(1)
pts <- pretty(youtube$Video.views/10000000000)
axis(2, at=pts, labels = paste(pts,"B",sep=""))

install.packages('qcc')
library(qcc)
table(youtube$Grade)
grade <-c(448,41,10)
names(grade)<-c("A","A+","A++")
pareto.chart(grade, main='Pareto chart for grade of Youtube channels', ylab='YouTube Channel grade frequency',col=rainbow(length(grade)))

youtubeViewsMean <- mean(youtube$Video.views)
youtubeSubscriberMean <- mean(youtube$Subscribers, na.rm=TRUE)
youtubeUploadsMean <- mean(youtube$Video.Uploads, na.rm=TRUE)

qnorm(1-00.5/2)

MEviews <- 1.959964*(sd(youtube$Video.views)/sqrt(500))
youtubeViewsMean - MEviews
youtubeViewsMean + MEviews

MESubs <- 1.959964*(sd(youtube$Subscribers,na.rm=TRUE)/sqrt(500))
youtubeSubscriberMean - MESubs
youtubeSubscriberMean + MESubs

MEUpl <- 1.959964*(sd(youtube$Video.Uploads,na.rm=TRUE)/sqrt(500))
youtubeUploadsMean - MEUpl
youtubeUploadsMean + MEUpl

lots_of_video_uploads <- subset(youtube, Video.Uploads >= 10000)
less_video_uploads <- subset(youtube, Video.Uploads < 10000)
nrow(lots_of_video_uploads)
nrow(less_video_uploads)
mean_lots_of_video_uploads <- mean(lots_of_video_uploads$Video.views)
mean_less_video_uploads <- mean(less_video_uploads$Video.views)
sd_lots_of_video_uploads <- sd(lots_of_video_uploads$Video.views)
t.test(lots_of_video_uploads$Video.views,less_video_uploads$Video.views)
t.test(lots_of_video_uploads$Subscribers,less_video_uploads$Subscribers)
more_subscribers <- subset(youtube, Subscribers > 10000000)
less_subscribers <- subset(youtube, Subscribers < 1000000)
t.test(more_subscribers$Video.views,less_subscribers$Video.views)

video_views_lm <- lm(Video.views~Video.Uploads, data=youtube)
ci.plot(video_views_lm)
