install.packages("dplyr")
library(readxl)
library(dplyr)

dataset <- read_excel("G:/#AIUB 8th/INTRODUCTION TO DATA SCIENCE/Mid Project/Dataset_MIdterm_sectoin(D).xlsx")

dim(dataset)      #dimension of dataset

names(dataset)     # variable name

View(dataset)      # view dataset

unique_data <- unique(dataset)     # removing duplicate

dataset[dataset == ""] <- NA    #replace all "" with NA

# summary(dataset)

vars <- select(dataset,Age,Gender,Cholesterol,QuantumPatternFeature,)
summary(vars)

is.na(dataset)
colSums(is.na(dataset))
sum(is.na(dataset))

missingValue<-colSums(is.na(dataset))

# only the below column have missing value
# which return the index of the missing value
which(is.na(dataset$Age))
which(is.na(dataset$Gender))
which(is.na(dataset$BloodPressure))
which(is.na(dataset$Heart_Rate))

barplot(missingValue,main = "Missing Values per Column",
        col = "red", las = 2)

# discard missing value
noMissing<-na.omit(dataset)
colSums(is.na(noMissing))
dim(noMissing)

meanData <- dataset  # Step 1: Copy the dataset
meanValAge <- round(mean(meanData$Age, na.rm = TRUE))  # Step 2: Calculate mean without NA
meanData$Age[is.na(meanData$Age)] <- meanValAge  # Step 3: Replace NAs with mean
sum(is.na(meanData$Age))

medianData<-dataset
medianValAge<- round(median(medianData$Age, na.rm = TRUE))
medianData$Age[is.na(medianData$Age)] <- medianValAge

modeData<-dataset
modeValAge <- as.numeric(names(sort(table(modeData$Age), decreasing = TRUE))[1])
modeData$Age[is.na(modeData$Age)] <- modeValAge

modeValGender <- as.numeric(names(sort(table(modeData$Gender), decreasing = TRUE))[1])
modeData$Gender[is.na(modeData$Gender)] <- modeValGender

# outlier in age column 
# Using IQR method
Q1 <- quantile(noMissing$Age , 0.25, na.rm = TRUE)
Q3 <- quantile(noMissing$Age, 0.75, na.rm = TRUE)
IQR_value <- Q3 - Q1
lower_bound <- Q1 - 1.5 * IQR_value
upper_bound <- Q3 + 1.5 * IQR_value

# Identify and replace outliers with median
outlier_indices <- which(noMissing$Age < lower_bound | noMissing$Age > upper_bound)

cleaned_data_without_outlier <- noMissing[-outlier_indices, ]

median_value <- median(noMissing$Age, na.rm = TRUE)
noMissing$Age[outliers] <- median_value

sort(noMissing$Age)

unique_data <- unique(cleaned_data_without_outlier)     # removing duplicate

# Convert BloodPressure to character first if it's not already
dataset$BloodPressure <- as.character(dataset$BloodPressure)

# Identify rows with non-numeric values in BloodPressure
invalid_bp <- grep("[^0-9]", dataset$BloodPressure, value = TRUE)
invalid_rows <- which(dataset$BloodPressure %in% invalid_bp)

# Print invalid values found
print(paste("Found", length(invalid_rows), "invalid BloodPressure values:"))
print(invalid_bp)

# Method 1: Remove invalid rows
cleaned_data_removed <- dataset[-invalid_rows, ]

# Convert to numeric
cleaned_data_removed$BloodPressure <- as.numeric(cleaned_data_removed$BloodPressure)

# Verification
print("Summary after cleaning:")
print(cleaned_data_removed$BloodPressure)

# Convert Heart_Rate to numerical using factor levels
cleaned_data_without_outlier$Heart_Rate <- factor(cleaned_data_without_outlier$Heart_Rate,
                             levels = c("Low", "High"),
                             labels = c(0, 1))


# Convert Gender (0,1) to categorical ("male","female")
cleaned_data_without_outlier$Gender <- factor(cleaned_data_without_outlier$Gender,
                         levels = c(0, 1),
                         labels = c("male", "female"))

# balacing the dataset
# Step 2: Copy dataset
idata <- cleaned_data_without_outlier

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

