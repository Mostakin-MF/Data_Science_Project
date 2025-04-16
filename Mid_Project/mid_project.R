dataset <- read.csv("E:/Dataset_MIdterm_sectoin(D).csv", header = TRUE, sep = ",")
summary(dataset)
install.packages("dplyr")
library(dplyr)

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

