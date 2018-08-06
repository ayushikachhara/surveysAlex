library(plotly)
library(RColorBrewer)
library(grDevices)
library(ggplot2)
library(dplyr)
library(tidyr)
library(data.table)
path <- "Q:/AQData/Data/CONA/2018/Survey/Working/"
setwd(path)

d.survey <- read.csv("Tell us about your air06082018.csv")

## for quiz:
# d.survey <- d.survey[,c(1:2, 15:26)]

## for survey:
d.survey <- d.survey[,c(1:14)]

# hist(d.survey$Random.Value.Generator)

## formatting the date to date time format and then to NZST:

d.survey$Submission.Date <- as.POSIXct(as.character(d.survey$Submission.Date),
                                          format = "%d/%m/%Y %H:%M", tz = "GMT") +43200


d.survey$Date <- as.Date(d.survey$Submission.Date)
# ScoreCard <- cbind.data.frame((table(d.survey$Enter.login.ID, d.survey$Correct.)))
# ScoreCard <-  dcast(ScoreCard, formula=  Var1 ~ Var2,
#                     value.var = 'Freq',
#                     fun.aggregate = sum)
# ScoreCard$Score <- 100*ScoreCard$Correct/(ScoreCard$Correct + ScoreCard$Wrong)


# write.csv(ScoreCard, "ScoreCard.csv")

number.resp <- as.data.frame(table(d.survey$Enter.login.ID))
number.resp.daily <- as.data.frame.matrix(table(d.survey$Enter.login.ID, d.survey$Date))
number.resp.daily <- as.data.frame(table(d.survey$Date))


q1 <- as.data.frame.matrix(table(d.survey$Enter.login.ID, d.survey$Where.does.most.air.pollution.in.Alexandra.come.from.))
q1 <- q1[,c(2:5)]
q1$loginID <- rownames(q1)
100*sum(q1[,1])/sum(q1[,c(1:4)])

q2 <- as.data.frame.matrix(table(d.survey$Enter.login.ID, d.survey$X.Banking...filling.your.fireplace.with.wood.and.turning.down.low.before.going.to.bed.?..))
q2 <- q2[,c(2:4)]
q2$loginID <- rownames(q2)
100*sum(q2[,2])/sum(q2[,c(1:3)])

q3 <- as.data.frame.matrix(table(d.survey$Enter.login.ID, d.survey$A.fireplace.that.burns.wood.pellets.rather.than.logs.produces.less.smoke))
q3$loginID <- rownames(q3)
100*sum(q3[,1])/sum(q3[,c(1:2)])

q4 <- as.data.frame.matrix(table(d.survey$Enter.login.ID, d.survey$Burning.dried.wood..))
q4 <- q4[,c(2:4)]
q4$loginID <- rownames(q4)

100*sum(q4$`Creates less smoke`)/sum(q4$`Creates more smoke`,q4$`Creates less smoke`, q4$`Makes no difference to smoke levels`)

q5 <- as.data.frame.matrix(table(d.survey$Enter.login.ID, d.survey$Burning.wet.wood..))
q5 <- q5[,c(2:3)]
q5$loginID <- rownames(q5)

100*sum(q5$`Creates more smoke`)/sum(q5$`Creates more smoke`,q5$`Creates less smoke`)

q6 <- as.data.frame.matrix(table(d.survey$Enter.login.ID, d.survey$Air.quality.is.the.legal.responsibility.of))
q6 <- q6[,c(2:5)]
q6$loginID <- rownames(q6)
100*sum(q6$`Otago Regional Council`)/sum(q6$`Central Otago District Council`,q6$`Ministry of Health`,
                                         q6$NIWA,q6$`Otago Regional Council`)

q7 <- as.data.frame.matrix(table(d.survey$Enter.login.ID, d.survey$In.the.last.10.years.air.quality.in.Alexandra.has))
q7 <- q7[,c(2:4)]
q7$loginID <- rownames(q7)
100*sum(q7$`Not changed much`)/sum(q7$`Got worse`,q7$Improved,q7$`Not changed much`)

q8 <- as.data.frame.matrix(table(d.survey$Enter.login.ID, d.survey$In.the.last.10.years.air.quality.in.Christchurch.has))
q8 <- q8[,c(2:4)]
q8$loginID <- rownames(q8)
100*sum(q8$Improved)/sum(q8$`Got worse`,q8$Improved,q8$`Not changed much`)

q9 <- as.data.frame.matrix(table(d.survey$Enter.login.ID, d.survey$In.the.last.10.years.air.quality.in.Nelson.has))
q9 <- q9[,c(2:4)]
q9$loginID <- rownames(q9)
100*sum(q9$Improved)/sum(q9$`Got worse`,q9$Improved,q9$`Not changed much`)

q10 <- as.data.frame.matrix(table(d.survey$Enter.login.ID, d.survey$The.most.common.chemical.element.in.the.air.is))
q10 <- q10[,c(2:5)]
q10$loginID <- rownames(q10)

100*sum(q10$Nitrogen)/sum(q10$`Carbon Dioxide`, q10$Nitrogen, q10$Oxygen, q10$Phlogiston)


PDFfile <- paste0(path,"Response to survey questions.pdf")
pdf(file=PDFfile, paper = "USr", width = 22)

## q1
ggplot(d.survey[which(d.survey$Not.including.cigarette.smoking..can.you.smell.smoke. != ""),]) +
  geom_bar(aes(Not.including.cigarette.smoking..can.you.smell.smoke.), fill = "steelblue") + theme_bw()+
  ggtitle("Can you smell smoke?") +xlab(" ") +
  theme(axis.title = element_text(size = 22), 
        axis.text = element_text(size = 22),
        title = element_text(size = 22))
  
## q2

ggplot(d.survey[which(d.survey$How.does.the.air.outside.look.to.you. != ""),]) +
  geom_bar(aes(How.does.the.air.outside.look.to.you.), fill = "steelblue") + theme_bw()+
  ggtitle("How does the air outside look to you?") +xlab("") +
  theme(axis.title = element_text(size = 22), 
        axis.text = element_text(size = 16),
        title = element_text(size = 22))

## q3

ggplot(d.survey[which(d.survey$RIGHT.NOW..do.you.have.irritated.eyes..nose..throat. != ""),]) +
  geom_bar(aes(RIGHT.NOW..do.you.have.irritated.eyes..nose..throat.), fill = "steelblue") + theme_bw()+
  ggtitle("do you have irritated eyes/ nose/ throat?") +xlab("") +
  theme(axis.title = element_text(size = 22), 
        axis.text = element_text(size = 22),
        title = element_text(size = 22))

## q4

ggplot(d.survey[which(d.survey$RIGHT.NOW..do.you.have.breathlessness. != ""),]) +
  geom_bar(aes(RIGHT.NOW..do.you.have.breathlessness.), fill = "steelblue") + theme_bw()+
  ggtitle("do you have breathlessness?") +xlab("")+
  theme(axis.title = element_text(size = 22), 
        axis.text = element_text(size = 22),
        title = element_text(size = 22))

## q5

ggplot(d.survey[which(d.survey$RIGHT.NOW..do.you.have.wheezing. != ""),]) +
  geom_bar(aes(RIGHT.NOW..do.you.have.wheezing.), fill = "steelblue") + theme_bw()+
  ggtitle("do you have wheezing?") +xlab("")+
  theme(axis.title = element_text(size = 22), 
        axis.text = element_text(size = 22),
        title = element_text(size = 22))

## q6

ggplot(d.survey[which(d.survey$RIGHT.NOW..do.you.have.a.cough. != ""),]) +
  geom_bar(aes(RIGHT.NOW..do.you.have.a.cough.), fill = "steelblue") + theme_bw()+
  ggtitle("do you have a cough?") +xlab("")+
  theme(axis.title = element_text(size = 22), 
        axis.text = element_text(size = 22),
        title = element_text(size = 22))

## q7

ggplot(d.survey[which(d.survey$Have.you.had.to.use.an.inhaler.or.other.asthma.medicine.in.the.last.hour.!= ""),]) +
  geom_bar(aes(Have.you.had.to.use.an.inhaler.or.other.asthma.medicine.in.the.last.hour.), fill = "steelblue") + theme_bw()+
  ggtitle("Have you had to use an inhaler or \nother asthma medicine in the last hour?") +xlab("")+
  theme(axis.title = element_text(size = 22), 
        axis.text = element_text(size = 22),
        title = element_text(size = 22))

## q8

ggplot(d.survey[which(d.survey$RIGHT.NOW..do.you.have.difficulty.exercising.!= ""),]) +
  geom_bar(aes(RIGHT.NOW..do.you.have.difficulty.exercising.), fill = "steelblue") + theme_bw()+
  ggtitle("do you have difficulty exercising?") +xlab("")+
  theme(axis.title = element_text(size = 22), 
        axis.text = element_text(size = 22),
        title = element_text(size = 22))

## q9

ggplot(d.survey[which(d.survey$Is.smoke.annoying.you.right.now. != ""),]) +
  geom_bar(aes(Is.smoke.annoying.you.right.now.), fill = "steelblue") + theme_bw()+
  ggtitle("Is smoke annoying you right now?") +xlab("")+
  theme(axis.title = element_text(size = 22), 
        axis.text = element_text(size = 22),
        title = element_text(size = 22))

## q10

d.survey$How.would.you.rate.air.quality.right.now. <- ifelse(d.survey$How.would.you.rate.air.quality.right.now. == 1, "Very Poor",
                                                             ifelse(d.survey$How.would.you.rate.air.quality.right.now. == 2,"Poor",
                                                                     ifelse(d.survey$How.would.you.rate.air.quality.right.now. == 3, "Average",
                                                                             ifelse(d.survey$How.would.you.rate.air.quality.right.now. == 4, "Good","Very Good"))))

d.survey$How.would.you.rate.air.quality.right.now. <- factor(d.survey$How.would.you.rate.air.quality.right.now.,
                                                             levels = c("Very Poor","Poor","Average","Good","Very Good"))
ggplot(d.survey[which(d.survey$How.would.you.rate.air.quality.right.now. != ""),]) +
  geom_bar(aes(How.would.you.rate.air.quality.right.now.), fill = "orange2") + theme_bw()+
  ggtitle("How would you rate air quality right now?") +xlab("")+
  theme(axis.title = element_text(size = 22), 
        axis.text = element_text(size = 22),
        title = element_text(size = 22))

dev.off()


#### Response to quiz questions ####
PDFfile <- paste0(path,"Response to quiz questions_06082018.pdf")
pdf(file=PDFfile, paper = "USr", width = 22)

## q1
ggplot(d.survey[which(d.survey$Not.including.cigarette.smoking..can.you.smell.smoke. != ""),]) +
  geom_bar(aes(Not.including.cigarette.smoking..can.you.smell.smoke.), fill = "steelblue") + theme_bw()+
  ggtitle("Can you smell smoke?") +xlab(" ") +
  theme(axis.title = element_text(size = 22),
        axis.text = element_text(size = 22),
        title = element_text(size = 22))

## q2

ggplot(d.survey[which(d.survey$How.does.the.air.outside.look.to.you. != ""),]) +
  geom_bar(aes(How.does.the.air.outside.look.to.you.), fill = "steelblue") + theme_bw()+
  ggtitle("How does the air outside look to you?") +xlab("") +
  theme(axis.title = element_text(size = 22),
        axis.text = element_text(size = 18),
        title = element_text(size = 22))

## q3

ggplot(d.survey[which(d.survey$RIGHT.NOW..do.you.have.irritated.eyes..nose..throat. != ""),]) +
  geom_bar(aes(RIGHT.NOW..do.you.have.irritated.eyes..nose..throat.), fill = "steelblue") + theme_bw()+
  ggtitle("do you have irritated eyes/ nose/ throat?") +xlab("") +
  theme(axis.title = element_text(size = 22),
        axis.text = element_text(size = 22),
        title = element_text(size = 22))

## q4

ggplot(d.survey[which(d.survey$RIGHT.NOW..do.you.have.breathlessness. != ""),]) +
  geom_bar(aes(RIGHT.NOW..do.you.have.breathlessness.), fill = "steelblue") + theme_bw()+
  ggtitle("do you have breathlessness?") +xlab("")+
  theme(axis.title = element_text(size = 22),
        axis.text = element_text(size = 22),
        title = element_text(size = 22))

## q5

ggplot(d.survey[which(d.survey$RIGHT.NOW..do.you.have.wheezing. != ""),]) +
  geom_bar(aes(RIGHT.NOW..do.you.have.wheezing.), fill = "steelblue") + theme_bw()+
  ggtitle("do you have wheezing?") +xlab("")+
  theme(axis.title = element_text(size = 22),
        axis.text = element_text(size = 22),
        title = element_text(size = 22))

## q6

ggplot(d.survey[which(d.survey$RIGHT.NOW..do.you.have.a.cough. != ""),]) +
  geom_bar(aes(RIGHT.NOW..do.you.have.a.cough.), fill = "steelblue") + theme_bw()+
  ggtitle("do you have a cough?") +xlab("")+
  theme(axis.title = element_text(size = 22),
        axis.text = element_text(size = 22),
        title = element_text(size = 22))

## q7

ggplot(d.survey[which(d.survey$Have.you.had.to.use.an.inhaler.or.other.asthma.medicine.in.the.last.hour.!= ""),]) +
  geom_bar(aes(Have.you.had.to.use.an.inhaler.or.other.asthma.medicine.in.the.last.hour.), fill = "steelblue") + theme_bw()+
  ggtitle("Have you had to use an inhaler or \nother asthma medicine in the last hour?") +xlab("")+
  theme(axis.title = element_text(size = 22),
        axis.text = element_text(size = 22),
        title = element_text(size = 22))

## q8

ggplot(d.survey[which(d.survey$RIGHT.NOW..do.you.have.difficulty.exercising.!= ""),]) +
  geom_bar(aes(RIGHT.NOW..do.you.have.difficulty.exercising.), fill = "steelblue") + theme_bw()+
  ggtitle("do you have difficulty exercising?") +xlab("")+
  theme(axis.title = element_text(size = 22),
        axis.text = element_text(size = 22),
        title = element_text(size = 22))

## q9

ggplot(d.survey[which(d.survey$Is.smoke.annoying.you.right.now. != ""),]) +
  geom_bar(aes(Is.smoke.annoying.you.right.now.), fill = "steelblue") + theme_bw()+
  ggtitle("Is smoke annoying you right now?") +xlab("")+
  theme(axis.title = element_text(size = 22),
        axis.text = element_text(size = 22),
        title = element_text(size = 22))

## q10

ggplot(d.survey[which(d.survey$How.would.you.rate.air.quality.right.now. != ""),]) +
  geom_bar(aes(How.would.you.rate.air.quality.right.now.), fill = "steelblue") + theme_bw()+
  ggtitle("How would you rate air quality right now?") +xlab("")+
  theme(axis.title = element_text(size = 22),
        axis.text = element_text(size = 22),
        title = element_text(size = 22))

dev.off()
# 
