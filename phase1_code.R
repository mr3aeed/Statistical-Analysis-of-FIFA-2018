#######################################Q0

q=read.csv("fifa18.csv")
missing_values <- apply(is.na(fifa18),2,sum)
missing_values_percentage <- missing_values/nrow(fifa18) * 100
plot ( missing_values_percentage , main = "Percentage of missing values across features" 
       , ylab = "Percentage" , type = 'l')

#######################################Q1
#1
heights <- fifa18$height_cm
heights <- na.omit(heights)
qplot(heights, geom="histogram" , binwidth = 5, main="Histogram of Heights" 
            , ylab= "Observations" , col=I("black") )
#2
ggplot(fifa18, aes(x=heights)) + geom_density(fill = "green") + 
  labs(x= "Heights" , title = "Density of Heights" , y = "")
#4
library("e1071")
mean (heights)
var(heights)
sd(heights)
skewness(heights)

#5
ggplot(fifa18, aes(x = "Height" ,y = heights , fill = "Height")) + 
      geom_boxplot() + labs( y ="") + stat_boxplot(geom ='errorbar') 
quantile(heights)
IQR(heights)
quantiles[[4]] - (1.5 * iqr )  # lower inner fence
quantiles[[4]] + (3 * iqr )  # upper outer fence

#6
boxplot.stats(longitude)$out
length(boxplot.stats(heights)$out)

#######################################Q2

#1
weights <- fifa18$weight_kg
ggplot(fifa18 , aes(x=heighs , y = weights)) + geom_point()

#2
CC <-cor(heights, weights , method="pearson")
CC

#3
ggplot(fifa18, aes(x= heights,y = weights)) + geom_point() + geom_smooth(method="lm")
ggplot(fifa18, aes(x= heights,y = weights)) + geom_point() + geom_smooth(method="loess")

#4
ages <- fifa18$age
ggplot(fifa18 , aes(x = ages , y = weights)) + geom_point()
CC <-cor(ages , weights , method="pearson")
CC
ggplot(fifa18, aes(x= ages,y = weights)) + geom_point() + geom_smooth(method="lm")
ggplot(fifa18, aes(x= ages,y = weights)) + geom_point() + geom_smooth(method="loess")

#5
mycolors <- function (n) {
       rev(heat.colors(n))
}
hexbinplot(weights ~ heights , xbins=10 , colramp = mycolors, type = "r" , colorcut=seq(0, 1, length = 8))
hexbinplot(weights ~ heights , xbins=20 , colramp = mycolors, type = "r" , colorcut=seq(0, 1, length = 8))

#6
library(dplyr)
library(GGally)
all_vars<-select(fifa18,"age","height_cm","weight_kg", "eur_wage","eur_release_clause" , "overall" )
ggcorr(all_vars , high = "red" , low = "darkblue" , label = TRUE , label_round = 2)

#######################################Q3

#1
cat_var <- fifa18$preferred_foot
data <- as.data.frame(table(cat_var))
ggplot(data=data, aes(x=cat_var, y=Freq)) + geom_bar(stat="identity" ,fill ="lightblue")+
  geom_text(aes(label=Freq), vjust=1.6, color="white", size=3.5)+labs(title = "Preferred Foot")+ 
  theme(plot.title = element_text(hjust = 0.5))
#2

ggplot(data=data, aes(x=reorder(cat_var,-Freq), y=Freq)) + geom_bar(stat="identity" ,fill ="lightblue")+
  geom_text(aes(label=Freq), vjust=1.6, color="white", size=3.5)+labs(title = "Preferred Foot")+ 
  theme(plot.title = element_text(hjust = 0.5))+coord_flip()

#3

table(cat_var)

#######################################Q4

#1
attack_rate <- fifa18$work_rate_att
preferred_foot <- fifa18$preferred_foot
table ( attack_rate , preferred_foot)

#2
data <- as.data.frame(table(preferred_foot , attack_rate))
ggplot(data=data, aes(x=preferred_foot, y=Freq, fill=attack_rate)) +
  geom_bar(stat="identity" ,  color="black" )

#3
ggplot(data=data, aes(x=preferred_foot, y=Freq, fill=attack_rate)) +
  geom_bar(stat="identity" ,  color="black" ,  position=position_dodge() ) + 
  geom_text(aes(label=Freq), vjust=1.6, color="white",
            position = position_dodge(0.9), size=3.5)

#4 
mosaicplot(table (preferred_foot , attack_rate) , main = "Mosaic Plot")

####################################Q5

#1
dm_sample <- diamonds[sample(nrow(diamonds), size = 200 ), ]
ggplot(dm_sample , aes(x=carat, y=price, color= color)) +
  geom_jitter(position=position_jitter(0.1)) +
  geom_text(aes(label = color),hjust=0,vjust=0)

dm_sample[, "price/carat"] <- dm_sample[, "price"] / dm_sample[, "carat"]
ggplot(dm_sample, aes(x = color ,y = price/carat , fill = color)) + 
          geom_boxplot()  + stat_boxplot(geom ='errorbar') 

#4
log_carat <- log(diamonds$carat)
log_price <- log(diamonds$price)
plot(log_carat , log_price)

####################################Q6
heights_sample <- sample(heights , size = 1000 , replace = FALSE)
x_bar <- mean (heights_sample)
s <- sd ( heights_sample) 
z_star <- abs (qnorm((1-0.98)/2))
ME <- z_star * s/sqrt(1000)
lower_bound <- x_bar-ME
upper_bound <- x_bar+ME

Hypothesis_test <- function(sample_data, null, alpha = 0.05, side = "both" ){
  sample_mean <- mean(sample_data)
  size <- length(sample_data)
  SE <- sd(sample_data)/sqrt(size)
  z_score <- (sample_mean - null)/SE
  df= size -1
  if  (side == "up"){
    if (size > 30 )
      p_value <- pnorm( z_score , lower.tail = FALSE)
    else
      p_value <- pt( z_score , df=df ,  lower.tail = FALSE)
  }
  else if (side == "low"){
    if (size > 30 )
      p_value <- pnorm ( z_score)
    else
      p_value <- pt( z_score , df=df )
  }
  else {
    if ( size > 30)
      p_value <- pnorm(abs(z_score) , lower.tail = FALSE) * 2
    else
      p_value <= pt ( abs(z_score) ,df = df ,lower.tail = FALSE  )*2
  }
  print(paste("P-Value = " , p_value))
  if(p_value < alpha) {
    print("H0 Rejected")
  }else{
    print("H0 Not Rejected")
  }
}
Hypothesis_test(heights_sample , null = 181 , alpha = 0.02 )
181-ME
SE= s/sqrt(1000)
pnorm((180.5041-180)/SE)

####################################Q7
sampled_fifa <- fifa18 [sample(nrow(fifa18) , 1000 , replace = FALSE) ,]
heights_sample <- sampled_fifa$height_cm
attack_rate_sample <- sampled_fifa$work_rate_att
summary(aov(heights_sample ~ attack_rate_sample))
idx_high =(which(attack_rate_sample=="High") )
idx_med  =(which(attack_rate_sample=="Medium") )
idx_low  =(which(attack_rate_sample=="Low") )

t.test(heights_sample[idx_high], heights_sample[idx_med], alternative = "two.sided", var.equal = FALSE)
