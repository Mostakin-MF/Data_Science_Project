dataset <- read.csv("E:/Dataset_MIdterm_sectoin(D).csv",header = TRUE,sep = ",")
dataset[dataset == ""] <- NA #replace all "" with NA
install.packages("dplyr")
library(dplyr)

summary(dataset)
vars<-select(dataset,Age,Gender,BloodPressure,Cholesterol,Heart_Rate,QuantumPatternFeature,)
summary(vars)

is.na(dataset)
colSums(is.na(dataset))
missingValue<-colSums(is.na(dataset))
sum(is.na(dataset))
which(is.na(dataset$Age))
which(is.na(dataset$Gender))
which(is.na(dataset$BloodPressure))
which(is.na(dataset$Cholesterol))
which(is.na(dataset$Heart_Rate))
which(is.na(dataset$QuantumPatternFeature))
which(is.na(dataset$HeartDisease))
barplot(missingValue,main = "Missing Values per Column",
        col = "red", las = 2)

noMissing<-na.omit(dataset)
colSums(is.na(noMissing))

meanData<-dataset
meanValAge<- round(mean(meanData$Age, na.rm = TRUE))
meanData$Age[is.na(meanData$Age)] <- meanValAge

medianData<-dataset
medianValAge<- round(median(medianData$Age, na.rm = TRUE))
medianData$Age[is.na(medianData$Age)] <- medianValAge

modeData<-dataset
modeValAge <- as.numeric(names(sort(table(modeData$Age), decreasing = TRUE))[1])
modeData$Age[is.na(modeData$Age)] <- modeValAge

modeValGender <- as.numeric(names(sort(table(modeData$Gender), decreasing = TRUE))[1])
modeData$Gender[is.na(modeData$Gender)] <- modeValGender




# Step 2: Copy dataset
idata <- dataset

# Step 3: Check class distribution
class_dist <- table(idata$HeartDisease)
class_dist

# Step 4: Split into majority and minority
if (class_dist[1] > class_dist[2]) {
  majority <- filter(idata, HeartDisease == 0)
  minority <- filter(idata, HeartDisease == 1)
} else {
  majority <- filter(idata, HeartDisease == 1)
  minority <- filter(idata, HeartDisease == 0)
}

# ---------- Oversampling ----------
set.seed(123)
os_minority <- minority %>% sample_n(nrow(majority), replace = TRUE)
os_balanced_data <- bind_rows(majority, os_minority)
table(os_balanced_data$HeartDisease)

# ---------- Undersampling ----------
set.seed(123)
us_majority <- majority %>% sample_n(nrow(minority))
us_balanced_data <- bind_rows(minority, us_majority)
table(us_balanced_data$HeartDisease)

