library(plyr) ##Data frame functionality, such as column renames
library(tidyr) ##Used to seperate list columns into seperate columns (tidyverse)
library(MASS) ##Used for MCA functionality for clustering categorical data
library(ggplot2)
rm(list = ls())

dataSet <- read.csv("middleSchoolSurvey.csv",na.strings=c("(Select)",""))

##Need to replace "(Select)" values with NaN
##No need to do this anymore, filtering occurs during import
#dataSet[dataSet=='(Select)'] <- NA
#dataSet <- lapply(dataSet, function(x) if(is.factor(x)) factor(x) else x) ##Refactor colums to account for NA values

##Rename dataFrame columns...  would prefer to use the set names array rather than retype!
setNames <- names(dataSet)
revNames <- c("ID"="ID")
revNames <- c(revNames, "Title" = "surveyName")
revNames <- c(revNames, "Date.Created" = "surveyDate")
revNames <- c(revNames, "I.have.my.own.smart.phone." = "smartPhone")
revNames <- c(revNames, "I.have.my.own.iTouch..iPad..or.tablet." = "smartDevice")
revNames <- c(revNames, "I.have.the.following.accounts..select.all.that.apply.." = "accountList")
revNames <- c(revNames, "Most.of.the.time.social.media.makes.me.feel..select.all.that.apply.." = "emotionsList")
revNames <- c(revNames, "I.feel.anxious.when.I.donâ..t.answer.a.text.right.away." = "textAnxiety")
revNames <- c(revNames, "I.feel.anxious.when.I.donâ..t.check.my.social.media.for.an.hour." = "mediaAnxiety")
revNames <- c(revNames, "I.feel.safe.sharing.my.location.on.social.media." = "locationComfort")
revNames <- c(revNames, "I.have.felt.threatened.or.bullied.on.social.media." = "mediaBullied")
revNames <- c(revNames, "I.have.bullied.or.threatened.someone.else.on.social.media." = "mediaBully")
revNames <- c(revNames, "I.feel.pressured.to.share.inappropriate.pictures.videos." = "inappropriateShare")
revNames <- c(revNames, "I.have.received.inappropriate.pictures.videos." = "inappropriateReceived")
revNames <- c(revNames, "I.know.someone..under.18..who.has.shared.inappropriate.pictures.videos." = "inappropriateKnown")
revNames <- c(revNames, "I.charge.my.phone..tablet..computer.in.my.room.at.night." = "chargeRoom")
revNames <- c(revNames, "Age" = "age")
revNames <- c(revNames, "Gender" = "gender")
dataSet <- rename(dataSet,revNames) ##using the Plyr package

##Split up the list elements into columns
accounts <- strsplit(as.character(dataSet$accountList), split = ", ")
accounts <- unique(unlist(accounts))
for (i in 1:length(accounts))(
    dataSet[[paste("account_",gsub("\\.","",gsub(" ","",accounts[i])),sep="")]] <- as.factor(grepl(accounts[i],dataSet$accountList))
)

emotions <- strsplit(as.character(dataSet$emotionsList), split = ", ")
emotions <- unique(unlist(emotions))
for (i in 1:length(emotions))(
    dataSet[[paste("emotion_",gsub("-","",emotions[i]),sep="")]] <- as.factor(grepl(emotions[i],dataSet$emotionsList))
)

##Create column that indicates positive emotions, negative emotions
positiveEmotions <- c("Happy","Included","Refreshed","Confident","Excited")
negativeEmotions <- c("Tired","Left-Out","Anxious","Insecure","Jealous")
dataSet$emotion_Negative <- as.factor(rowSums(matrix(as.numeric(unlist(dataSet[,paste("emotion_",gsub("-","",negativeEmotions),sep="")]))-1,ncol=length(negativeEmotions))) > 0)
dataSet$emotion_Positive <- as.factor(rowSums(matrix(as.numeric(unlist(dataSet[,paste("emotion_",gsub("-","",positiveEmotions),sep="")]))-1,ncol=length(positiveEmotions))) > 0)
dataSet$emotion_NegativeCount <- rowSums(matrix(as.numeric(unlist(dataSet[,paste("emotion_",gsub("-","",negativeEmotions),sep="")]))-1,ncol=length(negativeEmotions)))
dataSet$emotion_PositiveCount <- rowSums(matrix(as.numeric(unlist(dataSet[,paste("emotion_",gsub("-","",positiveEmotions),sep="")]))-1,ncol=length(positiveEmotions)))
dataSet$emotion_Bias <- as.factor(ifelse(dataSet$emotion_NegativeCount > dataSet$emotion_PositiveCount,"Negative","Positive"))

##Create columns that aggregate social media platforms together
accounts_media <- c("Roblox","Minecraft","Musical.ly","You Tube")
accounts_social <- c("Instagram","Facebook","House Party","After School","Snapchat","Sinsta","Twitter","Sarahah")
dataSet$account_media <- as.factor(rowSums(matrix(as.numeric(unlist(dataSet[,paste("account_",gsub("\\.","",gsub(" ","",accounts_media)),sep="")]))-1,ncol=length(accounts_media))) > 0)
dataSet$account_social <- as.factor(rowSums(matrix(as.numeric(unlist(dataSet[,paste("account_",gsub("\\.","",gsub(" ","",accounts_social)),sep="")]))-1,ncol=length(accounts_social))) > 0)

##Create feature that denotes whether or not the individual has access to a smart device
dataSet$deviceAccess <- as.factor(dataSet$smartPhone == "Yes" | dataSet$smartPhone == "I share one" | dataSet$smartDevice == "Yes" | dataSet$smartDevice == "I share One")

##Create onehot encoded data frame
oneHotColumns <- c("smartPhone","smartDevice","textAnxiety","mediaAnxiety","locationComfort","mediaBullied","mediaBully","inappropriateShare","inappropriateReceived","inappropriateKnown","chargeRoom","gender")
dataSetOneHot <- dataSet
for (i in 1:length(oneHotColumns)){
    dfCol <- dataSetOneHot[oneHotColumns[i]]
    dfCol[] <- addNA(dfCol[,1])
    levels(dfCol[,1])[length(levels(dfCol[,1]))] <- "NA"
    temp <- sort(table(dfCol),decreasing=FALSE)[1:length(table(dfCol))-1]
    for (j in 1:length(temp)){
        dataSetOneHot[paste(oneHotColumns[i],"_",names(temp)[j],sep="")] <- as.numeric(dfCol==gsub(" ","_",names(temp)[j]))
    }
}

##Construct numeric data set
dataSetNumeric <- dataSet
dataSetNumeric[] <- lapply(dataSetNumeric, function(x) as.numeric(factor(x)))

##Use only rows with all non-null values, remove unnecessary columns; eliminates some 25% of records
completeData <- na.omit(dataSet)
completeData <- completeData[, !(names(completeData) %in% c("ID","surveyName","surveyDate","accountList","emotionsList"))]

##Trying to construct different groupings of factors to create an appropriate feature set 
factorColumnsWide <- c("smartPhone","smartDevice","textAnxiety","mediaAnxiety","locationComfort","mediaBullied","mediaBully","inappropriateShare","inappropriateReceived","inappropriateKnown","chargeRoom","gender")
factorColumnsWide <- c("account_media","account_social","deviceAccess","emotion_Bias")
completeDataFactors <- dataSet[,factorColumnsWide]

##MCA calculation
m <- mca(completeDataFactors, nf=5)
catg <- apply(completeDataFactors, 2, function(x) nlevels(as.factor(x)))
m$d^2

##MCA plot
m_vars_df <- data.frame(m$cs, Variable = rep(names(catg), catg))
ggplot(data = m_vars_df, 
       aes(x = X1, y = X2, label = rownames(m_vars_df))) +
  geom_hline(yintercept = 0, colour = "gray70") +
  geom_vline(xintercept = 0, colour = "gray70") +
  geom_text(aes(colour = Variable)) +
  ggtitle("MCA plot of variables using R package MASS")

##Need to look in to agglomerative hierarchial clustering...
##Need to incorporate distance function to the row-values from the mca algorithm first?
km <- kmeans(m$rs,centers=4,nstart=5)
print(km)
