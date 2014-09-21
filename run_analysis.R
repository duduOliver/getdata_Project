featureNum <- read.table("features.txt")

trainSet_x <- read.table("./train/X_train.txt")
trainSet_y <- read.table("./train/y_train.txt")
trainSet_subject <- read.table("./train/subject_train.txt")
names(trainSet_x) <- featureNum$V2
names(trainSet_y) <- "y"
names(trainSet_subject) <- "subject"
trainSet <- cbind(trainSet_x, trainSet_y, trainSet_subject)

testSet_x <- read.table("./test/X_test.txt") 
testSet_y <- read.table("./test/y_test.txt")
testSet_subject <- read.table("./test/subject_test.txt") 
names(testSet_x) <- featureNum$V2
names(testSet_y) <- "y"
names(testSet_subject) = "subject"
testSet <- cbind(testSet_x, testSet_y, testSet_subject)

rowbindData <- rbind(trainSet,testSet)

toMatch <- c("mean", "Mean", "std")
col <- c(grep(paste(toMatch,collapse="|"), featureNum$V2, value=TRUE), "y", "subject")

meanANDstd <- rowbindData[,col]
attach(meanANDstd)
aggdata <- aggregate(meanANDstd, by=list(y,subject), FUN=mean, na.rm=TRUE)
detach(meanANDstd)

for(i in 1:180){
    if(aggdata[["y"]][i] == 1){
        aggdata[["y"]][i] <- "WALKING"
    }
    else if(aggdata[["y"]][i] == 2){
        aggdata[["y"]][i] <- "WALKING_UPSTAIRS"
    }
    else if(aggdata[["y"]][i] == 3){
        aggdata[["y"]][i] <- "WALKING_DOWNSTAIRS"
    }
    else if(aggdata[["y"]][i] == 4){
        aggdata[["y"]][i] <- "SITTING"
    }
    else if(aggdata[["y"]][i] == 5){
        aggdata[["y"]][i] <- "STANDING"
    }
    else if(aggdata[["y"]][i] == 6){
        aggdata[["y"]][i] <- "LAYING"
    }
}

write.table(aggdata, "./data.txt", sep="\t", row.name=FALSE)
