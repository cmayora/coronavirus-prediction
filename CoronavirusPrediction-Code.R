#######################################################################
## HarvardX: PH125.9x - Capstone Project - Choose Your Own
## Coronavirus Prediction Project
## Author: Carlos Mayora
## May 19, 2020
#######################################################################

#################################################
# Coronavirus Prediction Project 
################################################

###################################
## Data Analysis ##
###################################

###################################
##### Data Ingestion #####

################################
# Load libraries and data
################################

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(glmnet)) install.packages("glmnet", repos = "http://cran.us.r-project.org")
if(!require(rpart)) install.packages("rpart", repos = "http://cran.us.r-project.org")
if(!require(rattle)) install.packages("rattle", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")

# To make our coronavirus predictions we will use the data
# repository for Colombia COVID-19 cases at
# https://www.datos.gov.co/Salud-y-Protecci-n-Social/Casos-positivos-de-COVID-19-en-Colombia/gt2j-8ykr

coronavirus <- read.csv('Casos_positivos_de_COVID-19_en_Colombia.csv', stringsAsFactors = FALSE, na.strings='')

###################################
##### Data Exploration #####

# coronavirus dataset rows and columns
dim(coronavirus)

# coronavirus structure
str(coronavirus)

# Change column names
names(coronavirus) <- c('id','record_date','municipality_code','city','state','outcome',
                        'age','sex', 'contagion_type','severity','origin_country', "symptoms_date",
                        'date_of_death','diagnosis_date','recovery_date','web_date','recovery_type',
                        'origin_country_code')

# coronavirus structure
str(coronavirus)

# 6 first rows of coronavirus dataset including column names
head(coronavirus)

# Changing columns class to date and translating levels values
coronavirus$record_date <- as.Date(coronavirus$record_date)
coronavirus$municipality_code <- as.integer(coronavirus$municipality_code)
coronavirus$date_of_death <- as.Date(coronavirus$date_of_death)
coronavirus$city <- as.factor(coronavirus$city)
coronavirus$state <- as.factor(coronavirus$state)
coronavirus[,'outcome'] <- ifelse(coronavirus[,'outcome'] == "Recuperado", "recovered", 
                                  ifelse(coronavirus[,'outcome'] == "Fallecido", "deceased", 
                                         ifelse(coronavirus[,'outcome'] == "Hospital UCI", "icu", 
                                                ifelse(coronavirus[,'outcome'] == "Hospital", "hospitalized", 
                                                       ifelse(coronavirus[,'outcome'] == "Casa", "outpatientCare", 'unknown')))))

coronavirus$outcome <- as.factor(coronavirus$outcome)
coronavirus$sex <- as.factor(coronavirus$sex)
coronavirus[,'contagion_type'] <- ifelse(coronavirus[,'contagion_type'] == "Importado", "travel","contact")
coronavirus$contagion_type <- as.factor(coronavirus$contagion_type)
coronavirus[,'severity'] <- ifelse(coronavirus[,'severity'] == "Leve", "low", 
                                   ifelse(coronavirus[,'severity'] == "Moderado", "medium", 
                                          ifelse(coronavirus[,'severity'] == "Grave", "high", 
                                                 ifelse(coronavirus[,'severity'] == "Fallecido", "death", 
                                                        ifelse(coronavirus[,'severity'] == "Asintomático", "asymptomatic", NA)))))
coronavirus$severity <- as.factor(coronavirus$severity)
coronavirus$origin_country <- as.factor(coronavirus$origin_country)
coronavirus$symptoms_date <- as.Date(coronavirus$symptoms_date)
coronavirus$date_of_death <- as.Date(coronavirus$date_of_death)
coronavirus$diagnosis_date <- as.Date(coronavirus$diagnosis_date)
coronavirus$recovery_date  <- as.Date(coronavirus$recovery_date)
coronavirus$web_date  <- as.Date(coronavirus$web_date)
coronavirus[,'recovery_type'] <- ifelse(coronavirus[,'recovery_type'] == "PCR", "negTest", 
                                        ifelse(coronavirus[,'recovery_type'] == "Tiempo" | 
                                                 coronavirus[,'recovery_type'] == "TIEMPO", "time", NA))
coronavirus$recovery_type  <- as.factor(coronavirus$recovery_type)
coronavirus$origin_country_code <- as.integer(coronavirus$origin_country_code)

# coronavirus structure after tyding
str(coronavirus)

# Basic summary statistics
summary(coronavirus)

###################################
###### COVID-19 Situation In Colombia ######


# Let's get the number of cases per status
cat("The total number of confirmed cases is: ", nrow(coronavirus))
cat("The total number of deaths is: ", sum(coronavirus$outcome == "deceased"))
cat("The total number of recovered is: ", sum(coronavirus$outcome == "recovered"))
cat("The total number of active cases is: ", nrow(coronavirus) - sum(coronavirus$outcome == "deceased") - 
      sum(coronavirus$outcome == "recovered") - sum(coronavirus$outcome == "unknown"))

# Object with the total cases by state and type
totals_state <- coronavirus %>%
  group_by(state, outcome) %>%
  summarize(total = n())

# Let's get the list of the 15 most impacted states,
# those with most confirmed cases
totals_state %>% group_by(state) %>%
  summarize(total_cases = sum(total)) %>% arrange(-total_cases) %>% head(15)

# Variable with colos we will use for the following plots
colors <- data.frame(
  type = c("confirmed", "active", "deceased", "recovered"),
  fill = c("#2E9AFE", "#F2F5A9", "#FA5858", "#81F79F"),
  color = c("#0404B4", "#E2D303", "#B40404", "#088A08"))

totals_state %>% group_by(state) %>%
  summarize(total_cases = sum(total)) %>% arrange(-total_cases) %>% head(15) %>%
  ggplot(aes(x = reorder(state,total_cases), y = total_cases)) +
  geom_bar(stat = "identity", fill  = colors$fill[colors$type == "confirmed"], width = 0.8) +
  scale_y_continuous(breaks = seq(0, 14000, by = 2000)) +
  coord_flip() +
  theme_light(base_size = 10) +
  labs(x = "", y = "", title = "Top 15 States: Most Confirmed Cases") +
  theme(axis.title = element_text(size = 14, colour = "black"),
        axis.text.y = element_text(size = 11, face = "bold"),
        plot.title = element_text(size= 16, hjust=0.1, color = "#4e4d47"))

# Let's get the list of the 15 states with most deaths
totals_state %>% filter(outcome == "deceased") %>% 
  arrange(-total) %>% head(15)

totals_state %>% filter(outcome == "deceased") %>% 
  arrange(-total) %>% head(15) %>%
  ggplot(aes(x = reorder(state,total), y = total )) +
  geom_bar(stat = "identity", fill  = colors$fill[colors$type == "deceased"], width = 0.8) +
  scale_y_continuous(breaks = seq(0, 400, by = 50)) +
  coord_flip() +
  theme_light(base_size = 10) +
  labs(x = "", y = "", title = "Top 15 States: Most Deceased") +
  theme(axis.title = element_text(size = 14, colour = "black"),
        axis.text.y = element_text(size = 11, face = "bold"),
        plot.title = element_text(size= 16, hjust=0.1, color = "#4e4d47"))

# Let's get the list of the 15 states with most recovered cases
totals_state %>% filter(outcome == "recovered") %>% 
  arrange(-total) %>% head(15)

totals_state %>% filter(outcome == "recovered") %>% 
  arrange(-total) %>% head(15) %>%
  ggplot(aes(x = reorder(state,total), y = total )) +
  geom_bar(stat = "identity", fill  = colors$fill[colors$type == "recovered"], width = 0.8) +
  scale_y_continuous(breaks = seq(0, 5000, by = 500)) +
  coord_flip() +
  theme_light(base_size = 10) +
  labs(x = "", y = "", title = "Top 15 States: Most Recovered") +
  theme(axis.title = element_text(size = 14, colour = "black"),
        axis.text.y = element_text(size = 11, face = "bold"),
        plot.title = element_text(size= 16, hjust=0.1, color = "#4e4d47"))

# Let's get the list of the 15 states with most active cases
totals_state %>% filter(outcome != "deceased" & outcome != "recovered" & outcome != "unknown") %>% 
  group_by(state) %>% summarize(total_cases = sum(total)) %>% 
  arrange(-total_cases) %>% head(15)

totals_state %>% filter(outcome != "deceased" & outcome != "recovered" & outcome != "unknown") %>% 
  group_by(state) %>% summarize(total_cases = sum(total)) %>% 
  arrange(-total_cases) %>% head(15) %>%
  ggplot(aes(x = reorder(state,total_cases), y = total_cases )) +
  geom_bar(stat = "identity", fill  = colors$fill[colors$type == "active"], width = 0.8) +
  scale_y_continuous(breaks = seq(0, 7000, by = 1000)) +
  coord_flip() +
  theme_light(base_size = 10) +
  labs(x = "", y = "", title = "Top 15 States: Most Active Cases") +
  theme(axis.title = element_text(size = 14, colour = "black"),
        axis.text.y = element_text(size = 11, face = "bold"),
        plot.title = element_text(size= 16, hjust=0.1, color = "#4e4d47"))

# Object with total cases by date
totals <- coronavirus %>% filter(!is.na(symptoms_date)) %>%
  group_by(outcome, symptoms_date) %>%
  summarise(total_cases = n()) %>%
  ungroup() %>%
  arrange(symptoms_date) %>%
  pivot_wider(names_from = outcome, values_from = total_cases, values_fill = list(total_cases = 0)) %>%
  mutate(confirmed = rowSums(.[,-1]), active = icu+hospitalized+outpatientCare)

# Daily COVID-19 cases
ggplot(data = totals, aes(x = symptoms_date)) +
  geom_bar(aes(y = confirmed, color = "confirmed_col", fill = "confirmed_col"), 
           position = "identity", stat = "identity") +
  geom_bar(aes(y = active, color = "active_col", fill = "active_col"), 
           position = "identity", stat = "identity") +
  geom_bar(aes(y = recovered, color = "recovered_col", fill = "recovered_col"), 
           position = "identity", stat = "identity") +
  geom_bar(aes(y = deceased, color = "deceased_col", fill = "deceased_col"), 
           position = "identity", stat = "identity") +
  theme_light(base_size = 10) +
  scale_y_continuous(breaks = seq(0, 1200, by = 200)) +
  scale_color_manual(name = "Type",
                     values = c( "confirmed_col"=paste(colors$color[colors$type == "confirmed"], sep=""), 
                                 "active_col"=paste(colors$color[colors$type == "active"], sep=""), 
                                 "deceased_col"=paste(colors$color[colors$type == "deceased"], sep=""), 
                                 "recovered_col"=paste(colors$color[colors$type == "recovered"], sep="")),
                     labels = c("Active", "Confirmed", "Deceased", "Recovered")) +
  scale_fill_manual(name = "Type",
                    values = c( "confirmed_col"=paste(colors$fill[colors$type == "confirmed"], sep=""), 
                                "active_col"=paste(colors$fill[colors$type == "active"], sep=""), 
                                "deceased_col"=paste(colors$fill[colors$type == "deceased"], sep=""), 
                                "recovered_col"=paste(colors$fill[colors$type == "recovered"], sep="")),
                    labels = c("Active", "Confirmed", "Deceased", "Recovered")) +
  theme( #plot.margin = margin(0, 0, 0, 0, "pt"),
         legend.position = c(0.1, 0.8),
         plot.title = element_text(size= 16, hjust=0.1, color = "#4e4d47")
  ) +
  xlab("") +
  ylab("Number of Cases") +
  ggtitle("Daily COVID-19 Cases")

# Updating totals object with cumulative total cases by date
totals <- totals %>% 
  mutate(active_total = cumsum(active),
         recovered_total = cumsum(recovered),
         deceased_total = cumsum(deceased))

# Distribution of COVID-19 Cases
ggplot(data = totals, aes(x = symptoms_date)) +
  geom_density(aes(y = recovered_total, color = "recovered_col", fill = "recovered_col"), 
               position = "identity", stat = "identity") +
  geom_density(aes(y = active_total, color = "active_col", fill = "active_col"), 
               position = "identity", stat = "identity", alpha=.7) +
  geom_density(aes(y = deceased_total, color = "deceased_col", fill = "deceased_col"), 
               position = "identity", stat = "identity") +
  theme_light(base_size = 10) +
  scale_y_continuous(breaks = seq(0, 20000, by = 2000)) +
  scale_color_manual(name = "Type",
                     values = c( "active_col"=paste(colors$color[colors$type == "active"], sep=""), 
                                 "deceased_col"=paste(colors$color[colors$type == "deceased"], sep=""), 
                                 "recovered_col"=paste(colors$color[colors$type == "recovered"], sep="")),
                     labels = c("Active", "Death", "Recovered")) +
  scale_fill_manual(name = "Type",
                    values = c( "active_col"=paste(colors$fill[colors$type == "active"], sep=""), 
                                "deceased_col"=paste(colors$fill[colors$type == "deceased"], sep=""), 
                                "recovered_col"=paste(colors$fill[colors$type == "recovered"], sep="")),
                    labels = c("Active", "Death", "Recovered")) +
  theme(
    legend.position = c(0.1, 0.8),
    plot.title = element_text(size= 16, hjust=0.1, color = "#4e4d47")
  ) +
  xlab("") +
  ylab("Number of Cases") +
  ggtitle("Distribution of COVID-19 Cases")

###################################
##### Features #####

###################################
###### Age ######
# Set with total cases by age and type of outcome
totals_age <- coronavirus %>% 
  group_by(outcome, age) %>%
  summarise(total_cases = n()) %>%
  ungroup() %>%
  arrange(age) %>%
  pivot_wider(names_from = outcome, values_from = total_cases, values_fill = list(total_cases = 0)) %>%
  mutate(confirmed = rowSums(.[,-1]), death_rate = (deceased*100)/confirmed)

# List of top 15 ages with more confirmed cases
totals_age %>% arrange(-confirmed) %>% head(15)

# Age distribution
totals_age %>% arrange(-confirmed) %>% 
  ggplot(aes(x = age, y = confirmed, fill=age)) +
  geom_bar(stat = "identity", width = 0.8) +
  theme_light(base_size = 10) +
  labs(x = "Age", y = "Cases", title = "Age Distribution") +
  theme(
    axis.title = element_text(size = 14, colour = "black"),
    axis.text.y = element_text(size = 11, face = "bold"),
    plot.title = element_text(size= 16, hjust=0.1, color = "#4e4d47"))

# Top 15 ages by death rate
totals_age %>% arrange(-death_rate) %>% head(15)

# Death rate by age
totals_age %>% arrange(-death_rate) %>% 
  ggplot(aes(x = age, y = death_rate, fill=age)) +
  geom_bar(stat = "identity", width = 0.8) +
  theme_light(base_size = 10) +
  labs(x = "Age", y = "Death Rate", title = "Death Rate by Age") +
  theme(
    axis.title = element_text(size = 14, colour = "black"),
    axis.text.y = element_text(size = 11, face = "bold"),
    plot.title = element_text(size= 16, hjust=0.1, color = "#4e4d47"))

###################################
###### Gender ######

# Object with total cases by sex and type of outcome
totals_sex <- coronavirus %>% 
  group_by(outcome, sex) %>%
  summarise(total_cases = n()) %>%
  ungroup() %>%
  arrange(sex) %>%
  pivot_wider(names_from = outcome, values_from = total_cases, values_fill = list(total_cases = 0)) %>%
  mutate(confirmed = rowSums(.[,-1]), death_rate = (deceased*100)/confirmed)

# Confirmed cases by gender
totals_sex %>% arrange(-confirmed)

# Gender distribution
totals_sex %>% arrange(-confirmed) %>% 
  ggplot(aes(x = sex, y = confirmed, fill=sex)) +
  geom_bar(stat = "identity", width = 0.8) +
  theme_light(base_size = 10) +
  labs(x = "Gender", y = "Cases", title = "Gender Distribution") +
  theme(
    axis.title = element_text(size = 14, colour = "black"),
    axis.text.y = element_text(size = 11, face = "bold"),
    plot.title = element_text(size= 16, hjust=0.1, color = "#4e4d47"))

# Death rate by sex
totals_sex %>% arrange(-death_rate) %>% 
  ggplot(aes(x = sex, y = death_rate, fill=sex)) +
  geom_bar(stat = "identity", width = 0.8) +
  theme_light(base_size = 10) +
  labs(x = "Gender", y = "Death Rate", title = "Death Rate by Gender") +
  theme(
    axis.title = element_text(size = 14, colour = "black"),
    axis.text.y = element_text(size = 11, face = "bold"),
    plot.title = element_text(size= 16, hjust=0.1, color = "#4e4d47"))

# Cleaning objects we won't use
rm(colors, totals, totals_age, totals_sex, totals_state)

###################################
## Data Pre-Processing ##
###################################

###################################
##### Features Creation #####

###################################
###### Outcome Time ######

# Include outcome_time feature with number of days
# until patient gets an outcome
coronavirus <- coronavirus %>% 
  mutate(outcome_time = ifelse(!is.na(date_of_death), difftime(date_of_death, 
                                    symptoms_date, units="days"),
                            ifelse(!is.na(recovery_date), difftime(recovery_date, 
                                          symptoms_date, units="days"),NA)))

# Let's get the number of deceased and recovered with outcome_time 
# greater and smaller than 14
cat("Number of deceased with outcome_time greater than 14: ", 
    coronavirus %>% filter(outcome_time > 14 & outcome == "deceased") %>% nrow())
cat("Number of recovered with outcome_time greater than 14: ", 
    coronavirus %>% filter(outcome_time > 14 & outcome == "recovered") %>% nrow())
cat("Number of deceased with outcome_time less than 14: ", 
    coronavirus %>% filter(outcome_time < 14 & outcome == "deceased") %>% nrow())
cat("Number of recovered with outcome_time less than 14: ", 
    coronavirus %>% filter(outcome_time < 14 & outcome == "recovered") %>% nrow())

# Let's visualize the outcome_time feature with this plot
coronavirus %>% filter((outcome=="recovered" | outcome=="deceased") & !is.na(outcome_time)) %>% 
  group_by(outcome, outcome_time) %>%
  summarise(total_cases = n()) %>%
  ungroup() %>%
  arrange(outcome_time) %>%
  pivot_wider(names_from = outcome, values_from = total_cases, 
              values_fill = list(total_cases = 0)) %>%
  ggplot(aes(x = outcome_time)) +
  geom_line(aes(y = recovered, color = "recovered_col"), 
               position = "identity", stat = "identity") +
  geom_line(aes(y = deceased, color = "deceased_col"), 
               position = "identity", stat = "identity") +
  theme_light(base_size = 10) +
  scale_color_manual(name = "Type",
                     values = c( "deceased_col"="#FA5858", 
                                 "recovered_col"="#81F79F"),
                     labels = c("Death", "Recovered")) +
  theme(
    plot.title = element_text(size= 16, hjust=0.1, color = "#4e4d47")
  ) +
  xlab("Outcome Time") +
  ylab("Number of Cases") +
  ggtitle("Outcome Time")

###################################
###### Diagnosis Time ######

# Include diagnosis_time feature with number of days
# until patient gets the lab test results
coronavirus <- coronavirus %>% 
  mutate(diagnosis_time = ifelse(!is.na(diagnosis_date), 
                                 difftime(diagnosis_date, symptoms_date, units="days"),NA))

# Subset with recovered and deceased cases
# grouped by outcome and diagnosis_time
totals_diagnosis_time <- coronavirus %>% 
  filter((outcome=="recovered" | outcome=="deceased") & !is.na(diagnosis_time)) %>% 
  group_by(outcome, diagnosis_time) %>%
  summarise(total_cases = n()) %>%
  ungroup() %>%
  arrange(diagnosis_time) %>%
  pivot_wider(names_from = outcome, values_from = total_cases, values_fill = list(total_cases = 0))

# Top 6 diagnosis_time by recovered
top_recovered <- totals_diagnosis_time %>% arrange(-recovered) %>% head()
top_recovered

# Top 6 diagnosis_time by deceased
top_deceased <- totals_diagnosis_time %>% arrange(-deceased) %>% head()
top_deceased

# Let's visualize the diagnosis_time feature with this plot
ggplot(data = totals_diagnosis_time, aes(x = diagnosis_time)) +
  geom_line(aes(y = recovered, color = "recovered_col"), 
            position = "identity", stat = "identity") +
  geom_line(aes(y = deceased, color = "deceased_col"), 
            position = "identity", stat = "identity") +
  theme_light(base_size = 10) +
  #scale_y_continuous(breaks = seq(0, 20000, by = 2000)) +
  scale_color_manual(name = "Type",
                     values = c( "deceased_col"="#FA5858", 
                                 "recovered_col"="#81F79F"),
                     labels = c("Death", "Recovered")) +
  theme(
    plot.title = element_text(size= 16, hjust=0.1, color = "#4e4d47")
  ) +
  xlab("Diagnosis Time") +
  ylab("Number of Cases") +
  ggtitle("Diagnosis Time")

# Cleaning objects we won't use
rm(totals_diagnosis_time, top_recovered, top_deceased)

###################################
###### Data Preparation ######

###################################
###### Data Cleaning ######

# Select only the columns we are interested in,
# the columns we will use for predicting
coronavirus <- coronavirus %>% 
  select(id, state, age, sex, contagion_type, symptoms_date, diagnosis_date, diagnosis_time, outcome_time, outcome)

# Mutate outcome to change to NA those cases we don't know the outcome yet
coronavirus[,'outcome'] <- ifelse(coronavirus[,'outcome'] == "recovered", "recovered", 
                                  ifelse(coronavirus[,'outcome'] == "deceased", "deceased", 
                                         ifelse(coronavirus[,'outcome'] == "icu", NA, 
                                                ifelse(coronavirus[,'outcome'] == "hospitalized", NA, 
                                                       ifelse(coronavirus[,'outcome'] == "outpatientCare", NA, NA)))))

# Factorizing outcome to the 2 only possible values
coronavirus[,'outcome'] <- factor(coronavirus$outcome, levels = c("recovered","deceased"))

# Let's check the dataset dimension and structure
# before any cleanup
str(coronavirus)
dim(coronavirus)

# Number of rows before cleaning
dim <- dim(coronavirus)[1]

# Remove NAs from all columns but outcome_time and outcome,
# since we will keep them to predict
coronavirus <- coronavirus[complete.cases(coronavirus[ , c(1:8)]),]

dim(coronavirus)

cat("Removed rows after cleaning NAs: ", 
    dim - dim(coronavirus)[1])

# Number of rows after cleaning NAs
dim <- dim(coronavirus)[1]

# Filter out rows with and outcome and no outcome_time nor diagnosis_time
coronavirus <- coronavirus %>% 
  filter((!is.na(outcome) & !is.na(outcome_time) & !is.na(diagnosis_time)) |
           is.na(outcome))

dim(coronavirus)

cat("Removed rows after filtering outcome_time NAs: ", 
    dim - dim(coronavirus)[1])

# Cleaning objects we won't use
rm(dim)

###################################
###### Train and Test Sets ######

# training & prediction sets
# Predict outcome of Active cases is.na(outcome) prediction set
training <- coronavirus %>% filter(!is.na(outcome))
prediction <- coronavirus %>% filter(is.na(outcome))

dim(training)
dim(prediction)

# train and test sets
# train 80% of training set
# test 20% of training set
set.seed(19, sample.kind="Rounding")
index <- createDataPartition(training$outcome, times = 1, p = 0.2, list=FALSE)
train_set <- training[-index,]
test_set  <- training[index,]

# Cleaning objects we won't use
rm(index)

# Remove symptoms_date and diagnosis_date
# we won't use the dates for prediction,
# we use instead outcome_time and diagnosis_time
train_set <- train_set %>% select(-id,-symptoms_date,-diagnosis_date)

test_set <- test_set %>% select(-id,-symptoms_date,-diagnosis_date)

dim(train_set)
dim(test_set)

###################################
## Modeling ##
###################################

###################################
##### Logistic regression #####

# Change outcome to binary, 0 and 1
train_set_glm <- train_set %>% mutate(outcome = as.numeric(outcome == "recovered"))

# Fit logistic regression model
train_glm <- glm(outcome ~ ., data = train_set_glm, family = "binomial")

# Variable of importance
imp <- as.data.frame(varImp(train_glm))
imp <- data.frame(overall = imp$Overall,
                  names   = rownames(imp))
imp %>% arrange(-overall) %>% head()

p_hat <- predict(train_glm, test_set) 
y_hat <- factor(ifelse(p_hat > 0.5, "recovered", "deceased"))%>% factor(levels = c("recovered","deceased"))

# Confusion Matrix
cm <- confusionMatrix(y_hat, test_set$outcome)

# Results
cm_results <- bind_rows(tibble(Model = "Logistic Regression", 
                                  Accuracy = cm$overall["Accuracy"], 
                                  Sensitivity = cm$byClass["Sensitivity"],
                                  Specificity = cm$byClass["Specificity"]))
# Print the results
cm_results

# Cleaning objects we won't use
rm(train_set_glm, p_hat)

###################################
##### k-nearest neighbors #####

set.seed(19, sample.kind="Rounding")

# Fit knn model
control <- trainControl(method = "cv", number = 10, p = .9)
train_knn <- train(outcome ~ ., 
                   data = train_set,
                   method = "knn", 
                   tuneGrid = data.frame(k = seq(3, 21, 2)),
                   trControl = control)

# Final value of k used in the model
train_knn$bestTune

ggplot(train_knn)

y_hat <- predict(train_knn, test_set, type="raw")

# Confusion Matrix
cm <- confusionMatrix(y_hat, test_set$outcome)

# Results
cm_results <- bind_rows(cm_results,
                        tibble(Model = "k-nearest neighbors", 
                               Accuracy = cm$overall["Accuracy"], 
                               Sensitivity = cm$byClass["Sensitivity"],
                               Specificity = cm$byClass["Specificity"]))
# Print the results
cm_results

###################################
##### Classification and Regression Trees #####

set.seed(19, sample.kind="Rounding")

# Fit rpart model
train_rpart <- train(outcome ~ ., 
                     method = "rpart",
                     tuneGrid = data.frame(cp = seq(0, 0.05, len = 10)),
                     data = train_set)

# Best tuning parameter and plot
train_rpart$bestTune

ggplot(train_rpart)

#To see the resulting tree we access the finalModel and plot it:
fancyRpartPlot(train_rpart$finalModel, space=0, yspace=0, tweak=1, under.cex=1, gap=0)

# Variable of importance
imp <- as.data.frame(varImp(train_rpart)$importance)
imp <- data.frame(overall = imp$Overall,
                  names   = rownames(imp))
imp %>% arrange(-overall) %>% head()

y_hat = predict(train_rpart, test_set)

# Confusion Matrix
cm <- confusionMatrix(y_hat, test_set$outcome)

# Results
cm_results <- bind_rows(cm_results,
                        tibble(Model = "Regression Trees", 
                               Accuracy = cm$overall["Accuracy"], 
                               Sensitivity = cm$byClass["Sensitivity"],
                               Specificity = cm$byClass["Specificity"]))
# Print the results
cm_results

###################################
##### Random Forest #####

# Fit Random Forest model
train_rf <- randomForest(outcome ~ ., data=train_set)

plot(train_rf)

# Variable of importance
imp <- as.data.frame(varImp(train_rf))
imp <- data.frame(overall = imp$Overall,
                  names   = rownames(imp))
imp[order(imp$overall,decreasing = T),]
varImpPlot(train_rf, main = "Random Forest Variable Importance")

y_hat <- predict(train_rf, test_set)

# Confusion Matrix
cm <- confusionMatrix(y_hat, test_set$outcome)

# Results
cm_results <- bind_rows(cm_results,
                        tibble(Model = "Random Forest", 
                               Accuracy = cm$overall["Accuracy"], 
                               Sensitivity = cm$byClass["Sensitivity"],
                               Specificity = cm$byClass["Specificity"]))
# Print the results
cm_results

# Cleaning objects we won't use
rm(train_glm, control, train_knn, train_rpart, train_rf, y_hat)

###################################
## Final Prediction ##
###################################

# Remove id, symptoms_date and diagnosis_date
training <- training %>% select(-id, -symptoms_date, -diagnosis_date)

# Remove all the cases with an outcom_time and no outcome
# as this could be bad data
# calculate the outcome_time with current system date
prediction <- prediction %>% filter(is.na(outcome_time)) %>% 
  mutate(outcome_time = as.integer(difftime(Sys.Date(), symptoms_date, units="days")))

# prediction str
str(prediction)

# Remove id, symptoms_date and diagnosis_date
prediction_f <- prediction %>% select(-id,-symptoms_date,-diagnosis_date)

# Random Forest was selected 
# train with whole training dataset
# removing the outcome column
final_train_rf <- randomForest(outcome ~ ., data=training)

# Variable of importance
imp <- as.data.frame(varImp(final_train_rf))
imp <- data.frame(overall = imp$Overall,
                  names   = rownames(imp))
imp[order(imp$overall,decreasing = T),]
varImpPlot(final_train_rf, main = "Final Prediction Variable Importance")

prediction$outcome <- factor(levels(prediction_f$outcome)[predict(final_train_rf, prediction_f[,-7])])

# Cleaning objects we won't use
rm(prediction_f, imp)

# 6 first rows of prediction dataset including column names
head(prediction)

# Prediction analysis
cat("Death rate in training data: ", 
    round((sum(training$outcome == "deceased")*100)/nrow(training), digits=2), "%", sep="")
cat("Death rate in prediction data: ", 
    round((sum(prediction$outcome == "deceased")*100)/nrow(prediction), digits=2), "%", sep="")
cat("Recovered rate in training data: ", 
    round((sum(training$outcome == "recovered")*100)/nrow(training), digits=2), "%", sep="")
cat("Recovered rate in prediction data: ", 
    round((sum(prediction$outcome == "recovered")*100)/nrow(prediction), digits=2), "%", sep="")