whos <- read.csv(url("https://d37djvu3ytnwxt.cloudfront.net/asset-v1:MITx+15.071x_3+1T2016+type@asset+block/WHO.csv"), header = TRUE, stringsAsFactors = F)
str(whos)
head(whos)
colnames(whos)
summary(whos$FertilityRate)[4]
mean(whos$Over60)
whos$Country[which.min(whos$Over60)]
whos$Country[which.max(whos$LiteracyRate)]
library(ggplot2)

whos_hist <- ggplot(data = whos, aes(LifeExpectancy, fill = Region))
whos_hist +
  geom_histogram() 

whos_point <- ggplot(whos, aes(LifeExpectancy, FertilityRate)) 
whos_point +
  geom_point(aes(colour = Region))

with(whos, boxplot(ChildMortality ~ Region, ylab="Count", main="Boxplot of Child Mortality across regions"))

whos_box <- ggplot(whos, aes(Region, ChildMortality))
whos_box +
  geom_boxplot() +
  labs(title = "Child Mortality across world regions")

with(whos, tapply(ChildMortality, Region, mean))

rm(list=ls())
gc()


usfoods <- read.csv(url("https://d37djvu3ytnwxt.cloudfront.net/asset-v1:MITx+15.071x_3+1T2016+type@asset+block/USDA.csv"), stringsAsFactors = F)
str(usfoods)
head(usfoods)
summary(usfoods[,-c(1,2)])
usfoods_mat <- as.matrix(usfoods[,-c(1,2)])
head(usfoods_mat)
rownames(usfoods_mat) <- c(1:nrow(usfoods_mat))
class(usfoods_mat)
usfoods_mat <- data.matrix(usfoods_mat[complete.cases(usfoods_mat),])
dim(usfoods_mat)
head(usfoods_mat)

library(RColorBrewer)
library(DescTools)

todanger_palette <- colorRampPalette(c("green", "yellow", "red"))(n = 3)
heatmap(usfoods_mat[1:1000,], col = todanger_palette)

usfoods$Description
summary(usfoods$Calories)
hist(usfoods$Calories) # left skewed
mdn <- median(usfoods$Calorie, na.rm = T)
caloric_foods <- subset(usfoods, Calories > mdn)
caloric_foods <- caloric_foods[complete.cases(caloric_foods),]
hist(caloric_foods$Calories) # still skewed
str(caloric_foods)
head(caloric_foods)
set.seed(1994)
values = sample(1:nrow(caloric_foods), 1000)
hist(values)
caloric_foods <- data.frame(caloric_foods[values, ])
str(caloric_foods)
head(caloric_foods)
summary(caloric_foods$Calories)
hist(caloric_foods$Calories)
heatmap(data.matrix(caloric_foods[,-c(1,2)]), col = todanger_palette)

usfoods_plot <- ggplot(usfoods, aes(Protein, TotalFat))
usfoods_plot +
  geom_point(shape = 21, size = .5) +
  scale_color_brewer(palette = todanger_palette) 
head(usßfoods)
