#########################Q1
Sample_Size <- 1000
fifa18_Sample <- fifa18[sample(nrow(fifa18), Sample_Size)  , ]
Preferred_Foot_Sample <- fifa18_Sample$preferred_foot
p_hat <- length(which(Preferred_Foot_Sample == 'Right' ))/Sample_Size
p_hat
p <- 0.5
SE = sqrt (p*(1-p)/Sample_Size)
Z_Score <- (p_hat-p)/SE
p_value <- pnorm((Z_Score) , lower.tail = FALSE)
p_value

p <- 0.7
SE = sqrt (p*(1-p)/Sample_Size)
Z_Score <- (p_hat-p)/SE
p_value <- pnorm((Z_Score) , lower.tail = FALSE)
p_value

#########################Q2

TB <- table ( fifa18_Sample[,c("preferred_foot","work_rate_att")] )
p_left <- 0.34 
n_left <- 229
p_right <- 0.235
n_right <- 771
SE <- sqrt (p_left*(1-p_left)/n_left + p_right*(1-p_right)/n_right )
z <- abs (qnorm((1-0.95)/2))
CI <- c( (p_left-p_right)-z*SE , (p_left-p_right)+z*SE )
CI

p_pooled <- (p_left*n_left + p_right*n_right)/1000 
SE <- sqrt (p_pooled*(1-p_pooled)/n_left + p_pooled*(1-p_pooled)/n_right )
z <- (p_left-p_right)/SE
p_value <- pnorm((z) , lower.tail = FALSE)*2


TB <- table ( fifa18_Sample[,c("preferred_foot","work_rate_att")] )
TB
chisq.test(TB)
#########################Q3

Sample_Size <- 15
Preferred_Foot_Small <- fifa18[sample(nrow(fifa18), Sample_Size)  , ] $preferred_foot
p_hat <- length(which(Preferred_Foot_Small == 'Right' ))/Sample_Size
p_hat

Result = c ("Right" , "Left")
Sim_Results<-t(replicate(n = 1000 , sample(Result, size= Sample_Size, replace = TRUE )))
Sim_Results <- data.frame(Sim_Results)
Sim_Results_2 <- apply(Sim_Results,1,function(x)length(which(x == "Right")) )
p_value <- length(which(Sim_Results_2 >= 11))/1000
p_value
hist(Sim_Results_2/Sample_Size)

#########################Q4

Data <- fifa18$work_rate_att
Sample_Size <- 500
Unbiased <- sample (Data , Sample_Size , replace = FALSE )
prb <- ifelse(Data =="High",0.5,ifelse(Data == "Medium", 0.2 , 0.3))
Biased <-  sample (Data , Sample_Size , prob = prb)
prob_orig <- c(prop.table(table(Data)))
table(Unbiased)
table(Biased)
chisq.test(x = table(Unbiased) ,p = prob_orig)
chisq.test(x = table(Biased) ,p = prob_orig)

TB <- table ( fifa18_Sample[,c("preferred_foot","work_rate_att")] )
chisq.test(TB)
#########################Q5

Left_Drible <- fifa18_Sample[which(fifa18_Sample$preferred_foot=="Left"),]$dribbling
Right_Drible <- fifa18_Sample[which(fifa18_Sample$preferred_foot=="Right"),]$dribbling
x_bar_left <- mean (Left_Drible)
x_bar_right <- mean (Right_Drible)
SE = sqrt (var(Left_Drible)/length(Left_Drible) + var(Right_Drible)/length(Right_Drible))
Z <- (x_bar_left-x_bar_right)/SE
pnorm(Z , lower.tail = FALSE)

#########################Q6
#Backward Elimination
Model1 <- lm (dribbling ~ preferred_foot + height_cm  + body_type + age +  acceleration, fifa18_Sample)
summary ( Model1 )

Model2 <- lm (dribbling ~ height_cm  + body_type + age +  acceleration, fifa18_Sample)
summary ( Model2 )

#Forward Selection
Model1_1 <- lm (dribbling ~ preferred_foot , fifa18_Sample)
summary ( Model1_1 )
Model1_2 <- lm (dribbling ~ height_cm , fifa18_Sample)
summary ( Model1_2 )
Model1_3 <- lm (dribbling ~ body_type , fifa18_Sample)
summary ( Model1_3 )
Model1_4 <- lm (dribbling ~ age , fifa18_Sample)
summary ( Model1_4 )
Model1_5 <- lm (dribbling ~ acceleration , fifa18_Sample)
summary ( Model1_5 )
Model2_1 <- lm (dribbling ~ acceleration+preferred_foot , fifa18_Sample)
summary ( Model2_1 )
Model2_2 <- lm (dribbling ~ acceleration+height_cm , fifa18_Sample)
summary ( Model2_2 )
Model2_3 <- lm (dribbling ~ acceleration+body_type , fifa18_Sample)
summary ( Model2_3 )
Model2_4 <- lm (dribbling ~ acceleration+age , fifa18_Sample)
summary ( Model2_4 )
Model3_1 <- lm (dribbling ~ acceleration+age+preferred_foot , fifa18_Sample)
summary ( Model3_1 )
Model3_2 <- lm (dribbling ~ acceleration+age+height_cm , fifa18_Sample)
summary ( Model3_2 )
Model3_3 <- lm (dribbling ~ acceleration+age+body_type , fifa18_Sample)
summary ( Model3_3 )
Model4_1 <- lm (dribbling ~ acceleration+age+body_type+preferred_foot, fifa18_Sample)
summary ( Model4_1 )
Model4_2 <- lm (dribbling ~ acceleration+age+body_type+height_cm, fifa18_Sample)
summary ( Model4_2 )
Model5_1 <- lm (dribbling ~ acceleration+age+body_type+height_cm+preferred_foot, fifa18_Sample)
summary ( Model5_1 )
##########################Q7

MLR_Model <- lm (dribbling ~ height_cm + age + acceleration + ball_control,fifa18_Sample)
summary ( MLR_Model )
#c
qqnorm(MLR_Model$residuals)
#D
plot(fifa18_Sample$height_cm ,   MLR_Model$residuals)
abline( h = 0 , col = "red" , lwd = 2)

plot(fifa18_Sample$age ,   MLR_Model$residuals)
abline( h = 0 , col = "red" , lwd = 2)

plot(fifa18_Sample$acceleration ,   MLR_Model$residuals)
abline( h = 0 , col = "red" , lwd = 2)

plot(fifa18_Sample$ball_control ,   MLR_Model$residuals)
abline( h = 0 , col = "red" , lwd = 2)

qqnorm(MLR_Model$residuals)
qqline(MLR_Model$residuals)

plot ( MLR_Model$residuals~MLR_Model$fitted.values , main = "Residuals V.S Fitted")

#F
library(dplyr) 
library(GGally) 
all_vars<-select(fifa18_Sample,"age","height_cm","acceleration" , "ball_control" ) 
ggcorr(all_vars , high = "red" , low = "darkblue" , label = TRUE , label_round = 2)

#G
MLR_Model <- lm (dribbling ~ ball_control,fifa18_Sample)
summary ( MLR_Model )

t <- abs ( qt((1-0.9)/2 , df = length(fifa18_Sample)-2 ) )
CI <- c(1.0587-t*0.0122 , 1.0587+t*0.0122)
CI

#H
k <- 5
library("DAAG")
fifa18_Sample <-as.data.frame ( apply(fifa18_Sample, 2, as.numeric))
CVmodel <- cv.lm(data =(fifa18_Sample), dribbling ~ height_cm + age + acceleration + ball_control, m=k, plotit=TRUE)
attr(CVmodel, "ms")

##########################Q8
#A
Response <- fifa18_Sample$work_rate_att
Response <- ifelse(Response == "High","True","False")
fifa18_Sample$att_rate_high <- Response
fifa18_Sample$att_rate_high <- as.factor(fifa18_Sample$att_rate_high)
log_model <- glm(att_rate_high ~ heading_accuracy + preferred_foot+ height_cm + pas + sho   , data = fifa18_Sample,family = binomial  )
summary(log_model)

#C
t <- abs ( qt((1-0.98)/2 , df = length(fifa18_Sample)-2 ) )
CIshoot <- c(0.06185-t*0.00846 , 0.06185+t*0.00846) 
CIshoot
CIpas <- c(-0.00816-t*0.01029 , -0.00816+t*0.01029) 
CIpas
CIheight <- c(-0.10126-t*0.01327 , -0.10126+t*0.01327) 
CIheight
CIhead <- c(0.03392-t*0.00570 , 0.03392+t*0.00570) 
CIhead
CIprefferedfoot <-  c(0.13949-t*0.18974 , 0.13949+t*0.18974) 
CIprefferedfoot

#D
library(ggplot2)
all_cases <-expand.grid(sho=sample(1:100,1000,rep=TRUE) ,preferred_foot=c("Right" ,"Left"),
            heading_accuracy=mean(fifa18$heading_accuracy),pas = mean(fifa18$pas), 
            height_cm <- mean(fifa18$height_cm) )
colnames(all_cases)[5] <- "height_cm"
z <- abs(qnorm((1-0.95)/2))
preds <- predict(log_model , all_cases ,  type = "response", se.fit = TRUE)
all_cases$PredictedProb <- preds$fit
all_cases$upr <- preds$fit + (z * preds$se.fit)
all_cases$lwr <- preds$fit - (z * preds$se.fit)

ggplot(all_cases, aes(x=sho, y=PredictedProb , colour =preferred_foot )) +
  geom_point() + geom_line() +
  geom_ribbon(data=all_cases, aes(ymin=lwr, ymax=upr ,  fill = preferred_foot), alpha=0.2, linetype=2) + 
  labs(x="Shoot", y="Predicted Prob.")

#E
library(pROC)
probs <- predict(log_model ,type=c("response"))
g <- roc(att_rate_high ~ probs , data = fifa18_Sample ,print.auc = TRUE)
plot(g)
g
