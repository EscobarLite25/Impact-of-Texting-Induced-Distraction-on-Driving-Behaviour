library(ggplot2)
library(tidyverse)
library(dplyr)
library(ez)
library(ggpubr)
library(sjstats)
library(lsr)
library(effsize)
library(pwr)
library(psych)
library(MASS)
library(poLCA)
library(moments)
library(dgof)


data <-Per_Survey
data <- subset(data , select = -c(Timestamp, `Email Address`))

num_columns <- ncol(data)

## Loop through the columns and rename them
for (i in 1:num_columns) {
  new_name <- paste0("r", i)  # Generate the new column name
  names(data)[i] <- new_name  # Rename the column
}

data$r125 <- ifelse(data$r125 < 4.743195, "LowEQ", "HighEQ")

median(data$r125)

data <- data %>% mutate(r121 = case_when(
  r121 == "I have no car driving experience" ~ "No Experience",
  TRUE ~ r121  # Keep other entries unchanged
))


data <- data %>% mutate(r46 = case_when(
  r46 == "Yes, the accident involved injury to a co-passenger or driver" ~ "Yes",
  r46 == "Yes, the accident involved person injury and property damage" ~ "Yes",
  r46 == "Yes, the accident involved personal injury and property damage" ~ "Yes",
  r46 == "Yes, the accident only involved personal injury" ~ "Yes",
  r46 == "Yes, the accident only involved property damage" ~ "Yes",
  r46 == "Yes, the accident only involved property damage." ~ "Yes",
  TRUE ~ r46  # Keep other entries unchanged
))



data  <- data %>% 
  mutate(r121  = case_when(
    r121== '10 to 15 years' ~ 'More than 10 years',
    r121 =='More than 15 years'~'More than 10 years',
    TRUE ~ r121
  ))

data  <- data %>% 
  mutate(r121  = case_when(
    r121== '1 to 2 years' ~ 'Less than 5 years',
    r121 =='2 to 5 years'~'Less than 5 years',
    r121 =='Less than one year'~'Less than 5 years',
    TRUE ~ r121
  ))

data$r121
########################################################################################

data  <- data %>% 
  mutate(r99  = case_when(
    r99== 'Yes, a little bit.'~'Yes, a little bit',
    r99 =='Yes, to a good extent.'~'Yes, to a good extent',
    TRUE ~ r99
  ))
#######################################################################################3
data  <- data %>% 
  mutate(r105  = case_when(
    r105== '31 to 40'~'31 to 50',
    r105 =='41 to 50'~'31 to 50',
    TRUE ~ r105
  ))
data$r105
#############################################################################

####################just for checking count###################################
agg_tbl2 <- data %>% group_by(r117) %>%    #response105- Age  #response106-Gender
  summarise(total_count=n(),
            .groups = 'drop')
df3 <- agg_tbl2 %>% as.data.frame()
df3

############################Mann-Whitney U Test################################

variable_names <- paste0("r", 82:85)

# Perform Mann-Whitney test for each variable
results <- lapply(variable_names, function(variable) {
  wilcox.test(data[[variable]] ~ data$r106, data = data)
})

# Accessing and printing the results for each variable
for (i in seq_along(variable_names)) {
  variable <- variable_names[i]
  result <- results[[i]]
  cat("Variable:", variable, "\n")
  print(result)
  cat("\n")
}

#############################KRUSTAL-WALLIS TEST###################################################

variable_names <- paste0("r", 82:85)

# Perform Kruskal-Wallis test for each variable
results <- lapply(variable_names, function(variable) {
  kruskal.test(get(variable) ~ r105, data = data)
})

# Accessing and printing the results for each variable
for (i in seq_along(variable_names)) {
  variable <- variable_names[i]
  result <- results[[i]]
  cat("Variable:", variable, "\n")
  print(result)
  cat("\n")
}
############################################################################################3333
kruskal.test(r3 ~ r106, data = data)

aov_results <- aov(r3 ~ r106, data= data)
summary(aov_results)  

etaSquared(aov_results)

############################################################################################

q1<-(mean(data$r82)/7)*100
q2<- (mean(data$re83)/7)*100
q3<-(mean(data$r84)/7)*100
q4<-(mean(data$r85)/7)*100


sem1 <- sd(data$re82) / sqrt(length(data$r82))
sem2 <- sd(data$re83) / sqrt(length(data$r83))
sem3 <- sd(data$re84) / sqrt(length(data$r84))
sem4 <- sd(data$r85) / sqrt(length(data$r85))

s1 <- (sem1/mean(data$r82))*100
s2 <- (sem2/mean(data$r83))*100
s3 <- (sem3/mean(data$r84))*100
s4 <- (sem4/mean(data$r85))*100




df <- data.frame(Category = c("Phone\nCall", "Text\nMessage", "Mental\nAnalysis\nTask", "Pedestrian"),
                 Value = c(q1,q2,q3,q4),
                 SEM = c(s1,s2,s3,s4))

df <- df %>% mutate(Lower = Value - SEM, Upper = Value + SEM)

set.seed(10)  # Set seed for reproducibility
num_categories <- nrow(df)
random_colors <- sample(colors(), num_categories)

# Create the bar plot
ggplot(df, aes(x = Category)) +
  geom_bar(aes(y=Value), stat = "identity",  width=0.5,fill =random_colors )+
  #geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.2, color = "black")+
  ylab("Mean Rating:How distracting?(in %)") +
  theme(axis.text.x = element_text(family = "Arial",face="bold", size=10),
        axis.text.y = element_text(family = "Arial", face = "bold",size=13),
        plot.title = element_text(family = "Arial", face = "bold"))+
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10))+
  ggtitle("How Distracting are these activities?")

#################################################################################################

data$response83 <- (data$r83/7)*100
graph<-ggbarplot(data=data, x="r125",xlab="EQ",y="r83",ylab="Mean Rating: Distraction by text message(in %)", add="mean",fill="response106",ylim=c(0,100), position=position_dodge(0.7))
graph <- graph + labs(fill = "Gender") 
custom_colors <- c("Male" = "blue", "Female" = "pink")
graph <- graph + scale_fill_manual(values = custom_colors)
print(graph)
###########################################################################################3

graph<-ggbarplot(data=data, x="r106",xlab="Gender",y="r125",ylab="EQ Rating", add="mean",fill="r105",ylim=c(0,200), position=position_dodge(0.7))
graph <- graph + labs(fill = "Age") 
print(graph)
###########################################################################################
graph<-ggbarplot(data=data, x="r121",xlab="Driving Experience",y="r125",ylab="EQ Rating", add="mean",fill="r105",ylim=c(0,200), position=position_dodge(0.7))
graph <- graph + labs(fill = "Age") + theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(graph)
###########################################################################################

data$r1<-ifelse(data$r1 == "Yes",1,0)
data$r2<-ifelse(data$r2 == "Yes",1,0)
data$r1
###########################################################################################
response_columns <- c("r41", "r42", "r43", "r44")

for (column in response_columns) {
  data <- data %>% 
    mutate({{column}} := case_when(
      {{column}} == "0 to 30 kmph" ~ 1,
      {{column}} == "30 to 50 kmph" ~ 2,
      {{column}} == "50 to 70 kmph" ~ 3,
      {{column}} == "70 to 100 kmph" ~ 4,
      {{column}} == "100 to 120 kmph" ~ 5
      {{column}} == "more than 120 kmph" ~ 6
    ))
  
}

data$r42
########################################################################################

#Cronbach's Alpha for checking reliability

selected_columns <- data[, 24:27]
selected_columns
alpha(selected_columns,check.keys=TRUE)

########################################################################################
# Create empty vectors to store the values
q <- vector("numeric", length = 9)
sem <- vector("numeric", length = 9)
s <- vector("numeric", length = 9)

# Loop through the data columns and calculate the values
for (i in 3:11) {
  col <- data[[i]]
  q[i-2] <- (mean(col) / 10) * 100
  sem[i-2] <- sd(col) / sqrt(length(col))
  s[i-2] <- (sem[i-2] / mean(col)) * 100
}

# Access the calculated values
q1 <- q[1]
q2 <- q[2]
q3 <- q[3]
q4 <- q[4]
q5 <- q[5]
q6 <- q[6]
q7 <- q[7]
q8 <- q[8]
q9 <- q[9]

sem1 <- sem[1]
sem2 <- sem[2]
sem3 <- sem[3]
sem4 <- sem[4]
sem5 <- sem[5]
sem6 <- sem[6]
sem7 <- sem[7]
sem8 <- sem[8]
sem9 <- sem[9]

s1 <- s[1]
s2 <- s[2]
s3 <- s[3]
s4 <- s[4]
s5 <- s[5]
s6 <- s[6]
s7 <- s[7]
s8 <- s[8]
s9 <- s[9]


df <- data.frame(Category = c("Music", "Convo\nwith\npassenger", "Phone\nCall", "Hand-Call","Handsfree\nCall","Noisy\nPassenger","Reading\nText\nMessage", "Sending\nText\nMessage","Eating\nor\nDrinking"),
                 Value = c(q1,q2,q3,q4,q5,q6,q7,q8,q9),
                 SEM = c(s1,s2,s3,s4,s5,s6,s7,s8,s9))

df <- df %>% mutate(Lower = Value - SEM, Upper = Value + SEM)



set.seed(10)  # Set seed for reproducibility
num_categories <- nrow(df)
random_colors <- sample(colors(), num_categories)


ggplot(df, aes(x = Category)) +
  geom_bar(aes(y=Value), stat = "identity",  width=0.5,fill = random_colors)+
  geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.2, color = "black")+
  ylab("Mean Rating:How distracting?(in %)") +
  theme(axis.text.x = element_text(family = "Arial",face="bold", size=10),
        axis.text.y = element_text(family = "Arial", face = "bold",size=13),
        plot.title = element_text(family = "Arial", face = "bold"))+
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10))+
  ggtitle("How Distracting are these activities?")


#################################################################################
q <- vector("numeric", length = 4)
sem <- vector("numeric", length = 4)
s <- vector("numeric", length = 4)

# Loop through the data columns and calculate the values
for (i in 24:27) {
  col <- data[[i]]
  q[i-23] <- (mean(col) / 10) * 100
  sem[i-23] <- sd(col) / sqrt(length(col))
  s[i-23] <- (sem[i-23] / mean(col)) * 100
}

# Access the calculated values
q1 <- q[1]
q2 <- q[2]
q3 <- q[3]
q4 <- q[4]


sem1 <- sem[1]
sem2 <- sem[2]
sem3 <- sem[3]
sem4 <- sem[4]

s1 <- s[1]
s2 <- s[2]
s3 <- s[3]
s4 <- s[4]



df <- data.frame(Category = c("At\nHigher\nElevation", "With\nFlashing\nLights", "At\nRoad", "On\nMedian"),
                 Value = c(q1,q2,q3,q4),
                 SEM = c(s1,s2,s3,s4))

df <- df %>% mutate(Lower = Value - SEM, Upper = Value + SEM)
set.seed(40)  # Set seed for reproducibility
num_categories <- nrow(df)
random_colors <- sample(colors(), num_categories)


ggplot(df, aes(x = Category)) +
  xlab("Location of Sign-Boards")+
  geom_bar(aes(y=Value), stat = "identity",  width=0.5,fill = random_colors)+
  #geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.2, color = "black")+
  ylab("Mean Rating:How distracting?(in %)") +
  theme(axis.text.x = element_text(family = "Arial",face="bold", size=10),
        axis.text.y = element_text(family = "Arial", face = "bold",size=13),
        plot.title = element_text(family = "Arial", face = "bold"))+
  scale_y_continuous(limits = c(0, 80), breaks = seq(0, 80, by = 10))+
  ggtitle("How Distracting are these Sign-Boards?")
##########################################################################################
q <- vector("numeric", length = 4)
sem <- vector("numeric", length = 4)
s <- vector("numeric", length = 4)

# Loop through the data columns and calculate the values
for (i in 41:44) {
  col <- data[[i]]
  q[i-40] <- (mean(col) / 6) * 100
  sem[i-40] <- sd(col) / sqrt(length(col))
  s[i-40] <- (sem[i-40] / mean(col)) * 100
}


# Access the calculated values
q1 <- q[1]
q2 <- q[2]
q3 <- q[3]
q4 <- q[4]


sem1 <- sem[1]
sem2 <- sem[2]
sem3 <- sem[3]
sem4 <- sem[4]

s1 <- s[1]
s2 <- s[2]
s3 <- s[3]
s4 <- s[4]


df <- data.frame(Category = c("City\nRoads", "Rural\nRoads", "NH/SH", "Expressway"),
                 Value = c(q1,q2,q3,q4),
                 SEM = c(s1,s2,s3,s4))
df <- df %>% mutate(Lower = Value - SEM, Upper = Value + SEM)


set.seed(50)  # Set seed for reproducibility
num_categories <- nrow(df)
random_colors <- sample(colors(), num_categories)

ggplot(df, aes(x = Category)) +
  xlab("Roads")+
  geom_bar(aes(y=Value), stat = "identity",  width=0.5,fill = random_colors)+
  #geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.2, color = "black")+
  theme(axis.text.x = element_text(family = "Arial",face="bold", size=10),
        axis.text.y = element_text(family = "Arial", face = "bold",size=13),
        plot.title = element_text(family = "Arial", face = "bold"))+
  ylab("Mean Max Speed(kmph)") +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10))+
  ggtitle("Max Comfortable Speed")   

##########################################################################################
q <- vector("numeric", length = 12)
sem <- vector("numeric", length = 12)
s <- vector("numeric", length = 12)

# Loop through the data columns and calculate the values
for (i in 12:23) {
  col <- data[[i]]
  q[i-11] <- (mean(col) / 10) * 100
  sem[i-11] <- sd(col) / sqrt(length(col))
  s[i-11] <- (sem[i-11] / mean(col)) * 100
}


# Access the calculated values
q1 <- q[1]
q2 <- q[2]
q3 <- q[3]
q4 <- q[4]
q5 <- q[5]
q6 <- q[6]
q7 <- q[7]
q8 <- q[8]
q9 <- q[9]
q10 <- q[10]
q11<- q[12]
q12<- q[12]

sem1 <- sem[1]
sem2 <- sem[2]
sem3 <- sem[3]
sem4 <- sem[4]
sem5 <- sem[5]
sem6 <- sem[6]
sem7 <- sem[7]
sem8 <- sem[8]
sem9 <- sem[9]
sem10 <- sem[10]
sem11<- sem[11]
sem12<- sem[12]

s1 <- s[1]
s2 <- s[2]
s3 <- s[3]
s4 <- s[4]
s5 <- s[5]
s6 <- s[6]
s7 <- s[7]
s8 <- s[8]
s9 <- s[9]
s10 <- s[10]
s11<- s[11]
s12<- s[12]


df <- data.frame(Category = c("Crashed\nCar", "Advertisement", "Flashing\nLights", "Traffic\nSign","Traffic\nSignals","Crossing\nAnimals","People\non\nfootpath","Crossing\nPeople", "Construction\nWork", "Front\nVehicle","Parallel\nVehicle","Behind\nVehicle"),
                 Value = c(q1,q2,q3,q4,q5,q6,q7,q8,q9,q10,q11,q12),
                 SEM = c(s1,s2,s3,s4,s5,s6,s7,s8,s9,s10,s11,s12))
df <- df %>% mutate(Lower = Value - SEM, Upper = Value + SEM)

set.seed(250)  # Set seed for reproducibility
num_categories <- nrow(df)
random_colors <- sample(colors(), num_categories)


ggplot(df, aes(x = Category)) +
  xlab("Situations/Road Elements")+
  geom_bar(aes(y=Value), stat = "identity",  width=0.5,fill = random_colors)+
  #geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.2, color = "black")+
  theme(axis.text.x = element_text(family = "Arial",face="bold", size=10),
        axis.text.y = element_text(family = "Arial", face = "bold",size=13),
        plot.title = element_text(family = "Arial", face = "bold"))+
  ylab("Mean Rating: How distracting? (in %)") +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10))+
  ggtitle("How distracting are these things?") 
#############################################################################

chisq.test(data$r106, data$r121)

cont_table <- table(data$r106, data$r121)

# Perform the chi-square test
chi_square <- chisq.test(cont_table)

# Create a mosaic plot
mosaicplot(cont_table, main = "Mosaic Plot", color = TRUE)

# Add the chi-square test result as a caption
caption <- paste("Chi-Square Test:", "Chi-Square =", round(chi_square$statistic, 2),
                 "p-value =", format.pval(chi_square$p.value, digits = 3))
title(main = "Mosaic Plot with Chi-Square Test", sub = caption)

#####################################################################################
library(vcd)

# Create a contingency table
cont_table <- table(data$r106, data$r121)

# Perform the chi-square test
chi_square <- chisq.test(cont_table)

# Create a mosaic plot with horizontal labels on the y-axis
mosaicplot(t(cont_table), main = "", color = TRUE,
           labeling_args = list(rot_labels = 0))

# Add the chi-square test result as a caption
caption <- paste("Chi-Square Test:", "Chi-Square =", round(chi_square$statistic, 2),
                 "p-value =", format.pval(chi_square$p.value, digits = 3))
title(main = "Mosaic Plot with \n Chi-Square Test", sub = caption)
##########################################################
effect_size <- 0.5       # Expected effect size
n <- NULL               # Sample size (to be determined)
alpha <- 0.05           # Significance level
power <- 0.8            # Desired power

# Perform power analysis
pwr.t.test(d = effect_size, n = n, sig.level = alpha, power = power)
##################################################################################
shapiro.test(data$r125)
ks.test(data$r22, pnorm)
ks.t
####################################################################################3
column_indices <- 82:85
new_data <- data[, column_indices]
column_names <- colnames(new_data)

for (i in 1:(length(column_names) - 1)) {
  for (j in (i + 1):length(column_names)) {
    # Extract the two columns for t-test
    col1 <- data[[column_names[i]]]
    col2 <- data[[column_names[j]]]
    
    # Perform t-test
    result <- wilcox.test(col1, col2)
    
    #Cohen.d Test
    n1 <- length(col1)
    n2 <- length(col2)
    pooled_sd <- sqrt(((n1 - 1) * var(col1) + (n2 - 1) * var(col2)) / (n1 + n2 - 2))
    cohen_d <- (mean(col1) - mean(col2)) / pooled_sd
    
    # Print results if p-value is less than 0.05
    if (result$p.value < 0.05/6) {
      cat("Comparison between", column_names[i], "and", column_names[j], "\n")
      cat("p-value:", result$p.value, "\n")
      cat("Cohen's d:", cohen_d, "\n")
      cat("\n")
    }
  }
}

##############################################################################

ks.test(data$r10, "pnorm")
###################################################################################

skewness(data$r125)
data$r3 <- scale(data$r3)
boxcox(data$r3)
hist(data$r125)
ks.test(data$r125, pnorm)
ggqqplot(data$r125)
class(data)

###################################################################################3
wilcox.test(data$r3, data$r4)
################################################################################
corr.test(data$r10, data$r83)
#############################################################################


model <- poLCA(cbind(r82, r83, r84, r85) ~ 1, data = data, nclass = 3)
plot(model)  

table<- table(data$r89, data$r91)

ggplot(table, aes(x = Cate, y = Count, fill = Value)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(x = "Category", y = "Count", title = "Count of Values by Category") +
  theme_minimal()

#############################################################################################

count <- table(data$r121)
df <- data.frame(Value = names(count), Count = as.numeric(count))
ggplot(df, aes(x = Value, y = Count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = Count), vjust = -0.5, color = "black") +
  xlab("Driving Experience") +
  ylab("Count") +
  scale_y_continuous(limits = c(0, 40), breaks = seq(0, 40, by = 5))+
  ggtitle("Count of Values") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))+
  theme_minimal()

###########################################################################################################
data  <- data %>% 
  mutate(r121  = case_when(
    r121== 'More than 10 years'~ 6,
    r121 =='1 to 2 years'~3,
    r121 =='5 to 10 years'~5,
    r121 =='Less than one year'~2,
    r121 =='No Experience'~1,
    r121 =='2 to 5 years'~4,
  ))
data$r121
################################################################################################################
count <- table(data$r99)
df <- data.frame(Value = names(count), Count = as.numeric(count))
ggplot(df, aes(x = Value, y = Count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = Count), vjust = -0.5, color = "black") +
  xlab("Knowledge of AVs") +
  ylab("Count") +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10))+
  ggtitle("Count of Values") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))+
  theme_minimal()
###########################################################
column_names <- paste0("r", 3:27)

# Create an empty list to store the ANOVA results
anova_results <- list()

# Perform the ANOVA tests for each column
for (column in column_names) {
  formula <- as.formula(paste(column, "~ r121"))
  anova_results[[column]] <- aov(formula, data = data)
}

# Print the ANOVA results
for (i in seq_along(anova_results)) {
  column <- column_names[i]
  cat("Column:", column, "\n")
  print(summary(anova_results[[i]]))
  cat("\n")
}
###########################################################################################
######################################################################################
#######################################################################################3
#######################################################################################
library(ggplot2)

# Calculate the mean rating for males for r3 to r11
male_stats <- data %>%
  filter(r106 == "Male") %>%
  summarise(across(r3:r11, list(mean_rating = mean)))

# Calculate the mean rating for females for r3 to r11
female_stats <- data %>%
  filter(r106 == "Female") %>%
  summarise(across(r3:r11, list(mean_rating = mean)))

# Combine the male and female statistics
df <- bind_rows(
  male_stats %>% mutate(Gender = "Male"),
  female_stats %>% mutate(Gender = "Female")
)

# Reshape the data to long format
df_long <- df %>%
  pivot_longer(cols = -Gender, names_to = "Variable", values_to = "Value")

# Set seed for reproducibility
set.seed(50)


# Plot grouped bars with modified x-axis labels and patterns
ggplot(df_long, aes(x = Variable, y = Value, fill = Gender, pattern = Gender)) +
  geom_bar(position = position_dodge(), stat = "identity", width = 0.6) +
  xlab("Category") +
  ylab("Mean Rating") +
  ggtitle("How distracting are these activities?") +
  scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, by = 1)) +
  theme(axis.text.x = element_text(family = "Arial", face = "bold", size = 10),
        axis.text.y = element_text(family = "Arial", face = "bold", size = 13),
        plot.title = element_text(family = "Arial", face = "bold"),
        legend.position = "top")
  
#################################################################################
library(ggplot2)

# Define the custom labels for the x-axis
custom_labels <- c( "Sending\nText\nMessage","Eating\nor\nDrinking","Music", "Convo\nwith\npassenger", "Phone\nCall", "Hand\nCall",
                   "Handsfree\nCall", "Noisy\nPassenger", "Reading\nText\nMessage")

# Calculate the mean rating for males for r3 to r11
male_stats <- data %>%
  filter(r106 == "Male") %>%
  summarise(across(r3:r11, list(mean_rating = mean)))

# Calculate the mean rating for females for r3 to r11
female_stats <- data %>%
  filter(r106 == "Female") %>%
  summarise(across(r3:r11, list(mean_rating = mean)))

# Combine the male and female statistics
df <- bind_rows(
  male_stats %>% mutate(Gender = "Male"),
  female_stats %>% mutate(Gender = "Female")
)

# Reshape the data to long format
df_long <- df %>%
  pivot_longer(cols = -Gender, names_to = "Variable", values_to = "Value")

# Set seed for reproducibility
set.seed(50)

# Plot grouped bars with modified x-axis labels and patterns
ggplot(df_long, aes(x = Variable, y = Value, fill = Gender, pattern = Gender)) +
  geom_bar(position = position_dodge(), stat = "identity", width = 0.6) +
  xlab("Category") +
  ylab("Mean Rating") +
  ggtitle("How distracting are these activities?") +
  scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, by = 1)) +
  theme(axis.text.x = element_text(family = "Arial", face = "bold", size = 10),
        axis.text.y = element_text(family = "Arial", face = "bold", size = 13),
        plot.title = element_text(family = "Arial", face = "bold"),
        legend.position = "top") +
  scale_x_discrete(labels = custom_labels)

#####################################################################################
library(ggplot2)

new_labels <- c("Sending\nText\nMessage","Eating\nor\nDrinking","Music", "Convo\nwith\npassenger", "Phone\nCall", "Hand\nCall",
                "Handsfree\nCall", "Noisy\nPassenger", "Reading\nText\nMessage")

# Calculate the mean rating for each driving experience group (A, B, C, D) for variables r3 to r11
experience_stats <- data %>%
  filter(r121 %in% c("Less than 5 years", "5 to 10 years", "More than 10 years", "No Experience")) %>%
  group_by(r121) %>%
  summarise(across(r3:r11, list(mean_rating = mean)))

# Reshape the data to long format
df_long <- experience_stats %>%
  pivot_longer(cols = -r121, names_to = "Variable", values_to = "Value")

# Set seed for reproducibility
set.seed(50)

# Plot grouped bars with modified x-axis labels and patterns
ggplot(df_long, aes(x = Variable, y = Value, fill = r121, pattern = r121)) +
  geom_bar(position = position_dodge(), stat = "identity", width = 0.6) +
  xlab("Category") +
  ylab("Mean Rating") +
  ggtitle("How distracting are these activities?") +
  scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, by = 1)) +
  theme(axis.text.x = element_text(family = "Arial", face = "bold", size = 10),
        axis.text.y = element_text(family = "Arial", face = "bold", size = 13),
        plot.title = element_text(family = "Arial", face = "bold"),
        legend.position = "top") +
  labs(fill = "Driving Experience")+
  scale_x_discrete(labels = new_labels)
 
#######################################################################################

library(ggplot2)

new_labels <- c("Sending\nText\nMessage","Eating\nor\nDrinking","Music", "Convo\nwith\npassenger", "Phone\nCall", "Hand\nCall",
                "Handsfree\nCall", "Noisy\nPassenger", "Reading\nText\nMessage")

# Calculate the mean rating for each driving experience group (A, B, C, D) for variables r3 to r11
experience_stats <- data %>%
  filter(r105 %in% c("18 to 30", "31 to 50")) %>%
  group_by(r105) %>%
  summarise(across(r3:r11, list(mean_rating = mean)))

# Reshape the data to long format
df_long <- experience_stats %>%
  pivot_longer(cols = -r105, names_to = "Variable", values_to = "Value")

# Set seed for reproducibility
num_groups <- length(unique(df_long$r105))
random_colors <- sample(colors(), num_groups)


# Plot grouped bars with modified x-axis labels and patterns
ggplot(df_long, aes(x = Variable, y = Value, fill = r105)) +
  geom_bar(position = position_dodge(), stat = "identity", width = 0.6) +
  xlab("Category") +
  ylab("Mean Rating") +
  ggtitle("How distracting are these activities?") +
  scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, by = 1)) +
  theme(axis.text.x = element_text(family = "Arial", face = "bold", size = 10),
        axis.text.y = element_text(family = "Arial", face = "bold", size = 13),
        plot.title = element_text(family = "Arial", face = "bold"),
        legend.position = "top") +
  labs(fill = "Age") +
  #scale_fill_manual(values = random_colors, guide = FALSE)+
  scale_x_discrete(labels = new_labels)
#############################################################################
rating_columns <- names(data)[3:11]

# Create an empty list to store the results
results_list <- list()

# Iterate over each pair of columns and perform the Wilcoxon test for female group
for (i in 1:(length(rating_columns) - 1)) {
  for (j in (i + 1):length(rating_columns)) {
    column1 <- rating_columns[i]
    column2 <- rating_columns[j]
    
    # Subset the data for female group
    female_data <- subset(data, data$r106 == "Female", select = c("r106", column1, column2))
    
    # Perform Wilcoxon test for the two columns
    result <- wilcox.test(female_data[[column1]], female_data[[column2]], paired = TRUE)
    
    # Create a unique key for the result list
    result_key <- paste(column1, "-", column2, sep = "")
    
    # Store the result in the list
    results_list[[result_key]] <- result
  }
}

# Access the test results for each column pair
for (key in names(results_list)) {
  result <- results_list[[key]]
  cat("Columns:", key, "\n")
  cat("Wilcoxon test statistic:", result$statistic, "\n")
  cat("Wilcoxon p-value:", result$p.value, "\n")
  cat("\n")
}

###############################################################################
rating_columns <- names(data)[3:11]

# Create an empty list to store the results
results_list <- list()

# Iterate over each pair of columns and perform the Wilcoxon test for female group
for (i in 1:(length(rating_columns) - 1)) {
  for (j in (i + 1):length(rating_columns)) {
    column1 <- rating_columns[i]
    column2 <- rating_columns[j]
    
    # Subset the data for female group
    female_data <- subset(data, data$r106 == "Female", select = c("r106", column1, column2))
    
    # Perform Wilcoxon test for the two columns
    result <- wilcox.test(female_data[[column1]], female_data[[column2]], paired = TRUE)
    
    # Calculate the effect size (Z statistic)
    n <- length(female_data[[column1]])
    z <- abs(result$statistic) / sqrt(n * (n + 1) / 2)
    effect_size <- 2 * pnorm(-abs(z))
    
    # Create a unique key for the result list
    result_key <- paste(column1, "-", column2, sep = "")
    
    # Store the result in the list
    results_list[[result_key]] <- result
    
    # Store the effect size in the result object itself
    results_list[[result_key]]$effect_size <- effect_size
  }
}

# Access the test results and effect sizes for each column pair
for (key in names(results_list)) {
  result <- results_list[[key]]
  effect_size <- result$effect_size
  
  cat("Columns:", key, "\n")
  cat("Wilcoxon test statistic:", result$statistic, "\n")
  cat("Wilcoxon p-value:", result$p.value, "\n")
  #cat("Effect size (Z statistic):", effect_size, "\n")
  cat("\n")
}

################################################################################3

######################################################################3
library(ggplot2)


#new_labels <- c("Crashed\nCar", "Advertisement", "Flashing\nLights", "Traffic\nSign","Traffic\nSignals","Crossing\nAnimals","People\non\nfootpath","Crossing\nPeople", "Construction\nWork", "Front\nVehicle","Parallel\nVehicle","Behind\nVehicle")
# Calculate the mean rating for males for r3 to r11
male_stats <- data %>%
  filter(r106 == "Male") %>%
  summarise(across(r12:r23, list(mean_rating = mean)))

# Calculate the mean rating for females for r3 to r11
female_stats <- data %>%
  filter(r106 == "Female") %>%
  summarise(across(r12:r23, list(mean_rating = mean)))

# Combine the male and female statistics
df <- bind_rows(
  male_stats %>% mutate(Gender = "Male"),
  female_stats %>% mutate(Gender = "Female")
)

# Reshape the data to long format
df_long <- df %>%
  pivot_longer(cols = -Gender, names_to = "Variable", values_to = "Value")

# Set seed for reproducibility
set.seed(50)


# Plot grouped bars with modified x-axis labels and patterns
ggplot(df_long, aes(x = Variable, y = Value, fill = Gender, pattern = Gender)) +
  geom_bar(position = position_dodge(), stat = "identity", width = 0.6) +
  xlab("Category") +
  ylab("Mean Rating") +
  ggtitle("How distracting are these activities?") +
  scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, by = 1)) +
  theme(axis.text.x = element_text(family = "Arial", face = "bold", size = 10),
        axis.text.y = element_text(family = "Arial", face = "bold", size = 13),
        plot.title = element_text(family = "Arial", face = "bold"),
        legend.position = "top") 
  #scale_x_discrete(labels = new_labels)

#########################################################################
library(ggplot2)


new_labels <- c("At\nHigher\nElevation", "With\nFlashing\nLights", "At\nRoad", "On\nMedian")
# Calculate the mean rating for males for r3 to r11
male_stats <- data %>%
  filter(r106 == "Male") %>%
  summarise(across(r24:r27, list(mean_rating = mean)))

# Calculate the mean rating for females for r3 to r11
female_stats <- data %>%
  filter(r106 == "Female") %>%
  summarise(across(r24:r27, list(mean_rating = mean)))

# Combine the male and female statistics
df <- bind_rows(
  male_stats %>% mutate(Gender = "Male"),
  female_stats %>% mutate(Gender = "Female")
)

# Reshape the data to long format
df_long <- df %>%
  pivot_longer(cols = -Gender, names_to = "Variable", values_to = "Value")

# Set seed for reproducibility
set.seed(50)


# Plot grouped bars with modified x-axis labels and patterns
ggplot(df_long, aes(x = Variable, y = Value, fill = Gender, pattern = Gender)) +
  geom_bar(position = position_dodge(), stat = "identity", width = 0.6) +
  xlab("Category") +
  ylab("Mean Rating") +
  ggtitle("How distracting are these activities?") +
  scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, by = 1)) +
  theme(axis.text.x = element_text(family = "Arial", face = "bold", size = 10),
        axis.text.y = element_text(family = "Arial", face = "bold", size = 13),
        plot.title = element_text(family = "Arial", face = "bold"),
        legend.position = "top") +
  scale_x_discrete(labels = new_labels)
##########################################################################################

################################################################################3

library(ggplot2)

new_labels <- c("Crashed\nCar", "Advertisement", "Flashing\nLights", "Traffic\nSign","Traffic\nSignals","Crossing\nAnimals","People\non\nfootpath","Crossing\nPeople", "Construction\nWork", "Front\nVehicle","Parallel\nVehicle","Behind\nVehicle")

# Calculate the mean rating for each driving experience group (A, B, C, D) for variables r3 to r11
experience_stats <- data %>%
  filter(r121 %in% c("Less than 5 years", "5 to 10 years", "More than 10 years", "No Experience")) %>%
  group_by(r121) %>%
  summarise(across(r12:r23, list(mean_rating = mean)))

# Reshape the data to long format
df_long <- experience_stats %>%
  pivot_longer(cols = -r121, names_to = "Variable", values_to = "Value")

# Set seed for reproducibility
set.seed(50)

# Plot grouped bars with modified x-axis labels and patterns
ggplot(df_long, aes(x = Variable, y = Value, fill = r121, pattern = r121)) +
  geom_bar(position = position_dodge(), stat = "identity", width = 0.6) +
  xlab("Category") +
  ylab("Mean Rating") +
  ggtitle("How distracting are these activities?") +
  scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, by = 1)) +
  theme(axis.text.x = element_text(family = "Arial", face = "bold", size = 10),
        axis.text.y = element_text(family = "Arial", face = "bold", size = 13),
        plot.title = element_text(family = "Arial", face = "bold"),
        legend.position = "top") +
  labs(fill = "Driving Experience")+
  scale_x_discrete(labels = new_labels)
############################################################################
library(ggplot2)

new_labels <- c("At\nHigher\nElevation", "With\nFlashing\nLights", "At\nRoad", "On\nMedian")

# Calculate the mean rating for each driving experience group (A, B, C, D) for variables r3 to r11
experience_stats <- data %>%
  filter(r121 %in% c("Less than 5 years", "5 to 10 years", "More than 10 years", "No Experience")) %>%
  group_by(r121) %>%
  summarise(across(r24:r27, list(mean_rating = mean)))

# Reshape the data to long format
df_long <- experience_stats %>%
  pivot_longer(cols = -r121, names_to = "Variable", values_to = "Value")

# Set seed for reproducibility
set.seed(50)

# Plot grouped bars with modified x-axis labels and patterns
ggplot(df_long, aes(x = Variable, y = Value, fill = r121, pattern = r121)) +
  geom_bar(position = position_dodge(), stat = "identity", width = 0.6) +
  xlab("Category") +
  ylab("Mean Rating") +
  ggtitle("How distracting are these activities?") +
  scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, by = 1)) +
  theme(axis.text.x = element_text(family = "Arial", face = "bold", size = 10),
        axis.text.y = element_text(family = "Arial", face = "bold", size = 13),
        plot.title = element_text(family = "Arial", face = "bold"),
        legend.position = "top") +
  labs(fill = "Driving Experience")+
  scale_x_discrete(labels = new_labels)
###########################################################################3
library(ggplot2)

new_labels <- c("Crashed\nCar", "Advertisement", "Flashing\nLights", "Traffic\nSign","Traffic\nSignals","Crossing\nAnimals","People\non\nfootpath","Crossing\nPeople", "Construction\nWork", "Front\nVehicle","Parallel\nVehicle","Behind\nVehicle")

# Calculate the mean rating for each driving experience group (A, B, C, D) for variables r3 to r11
experience_stats <- data %>%
  filter(r105 %in% c("18 to 30", "31 to 50")) %>%
  group_by(r105) %>%
  summarise(across(r12:r23, list(mean_rating = mean)))

# Reshape the data to long format
df_long <- experience_stats %>%
  pivot_longer(cols = -r105, names_to = "Variable", values_to = "Value")

# Set seed for reproducibility
num_groups <- length(unique(df_long$r105))
random_colors <- sample(colors(), num_groups)


# Plot grouped bars with modified x-axis labels and patterns
ggplot(df_long, aes(x = Variable, y = Value, fill = r105)) +
  geom_bar(position = position_dodge(), stat = "identity", width = 0.6) +
  xlab("Category") +
  ylab("Mean Rating") +
  ggtitle("How distracting are these activities?") +
  scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, by = 1)) +
  theme(axis.text.x = element_text(family = "Arial", face = "bold", size = 10),
        axis.text.y = element_text(family = "Arial", face = "bold", size = 13),
        plot.title = element_text(family = "Arial", face = "bold"),
        legend.position = "top") +
  labs(fill = "Age") +
  #scale_fill_manual(values = random_colors, guide = FALSE)+
  scale_x_discrete(labels = new_labels)
#########################################################################
library(ggplot2)

new_labels <- c("At\nHigher\nElevation", "With\nFlashing\nLights", "At\nRoad", "On\nMedian")

# Calculate the mean rating for each driving experience group (A, B, C, D) for variables r3 to r11
experience_stats <- data %>%
  filter(r105 %in% c("18 to 30", "31 to 50")) %>%
  group_by(r105) %>%
  summarise(across(r24:r27, list(mean_rating = mean)))

# Reshape the data to long format
df_long <- experience_stats %>%
  pivot_longer(cols = -r105, names_to = "Variable", values_to = "Value")

# Set seed for reproducibility
num_groups <- length(unique(df_long$r105))
random_colors <- sample(colors(), num_groups)


# Plot grouped bars with modified x-axis labels and patterns
ggplot(df_long, aes(x = Variable, y = Value, fill = r105)) +
  geom_bar(position = position_dodge(), stat = "identity", width = 0.6) +
  xlab("Category") +
  ylab("Mean Rating") +
  ggtitle("How distracting are these activities?") +
  scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, by = 1)) +
  theme(axis.text.x = element_text(family = "Arial", face = "bold", size = 10),
        axis.text.y = element_text(family = "Arial", face = "bold", size = 13),
        plot.title = element_text(family = "Arial", face = "bold"),
        legend.position = "top") +
  labs(fill = "Age") +
  #scale_fill_manual(values = random_colors, guide = FALSE)+
  scale_x_discrete(labels = new_labels)
#######################################################################

#########################################################################
group_A_data <- subset(data, data$r121 == "5 to 10 years", select = c("r121", "r24", "r25", "r26", "r27"))
group_B_data <- subset(data, data$r121 == "Less than 5 years", select = c("r121", "r24", "r25", "r26", "r27"))
group_C_data <- subset(data, data$r121 == "More than 10 years", select = c("r121", "r24", "r25", "r26", "r27"))
group_D_data <- subset(data, data$r121 == "No Experience", select = c("r121", "r24", "r25", "r26", "r27"))

# Define the column names for distraction ratings
rating_columns <- c( "r24", "r25", "r26", "r27")

# Create an empty list to store the results
results_list <- list()

# Iterate over each column and perform the Wilcoxon signed-rank test for paired samples
for (column in rating_columns) {
  # Subset the data for the specific column and driving experience group
  group_A_ratings <- group_A_data[[column]]
  group_B_ratings <- group_B_data[[column]]
  group_C_ratings <- group_C_data[[column]]
  group_D_ratings <- group_D_data[[column]]
  
  # Identify the smallest sample size among the driving experience groups
  n <- min(length(group_A_ratings), length(group_B_ratings), length(group_C_ratings), length(group_D_ratings))
  
  # Perform the Wilcoxon signed-rank test with aligned pairs for each driving experience group
  result_AB <- wilcox.test(group_A_ratings[1:n], group_B_ratings[1:n], paired = TRUE)
  result_AC <- wilcox.test(group_A_ratings[1:n], group_C_ratings[1:n], paired = TRUE)
  result_AD <- wilcox.test(group_A_ratings[1:n], group_D_ratings[1:n], paired = TRUE)
  result_BC <- wilcox.test(group_B_ratings[1:n], group_C_ratings[1:n], paired = TRUE)
  result_BD <- wilcox.test(group_B_ratings[1:n], group_D_ratings[1:n], paired = TRUE)
  result_CD <- wilcox.test(group_C_ratings[1:n], group_D_ratings[1:n], paired = TRUE)
  
  # Create a unique key for the result list
  result_key <- column
  
  # Store the results in the list
  results_list[[result_key]] <- list(result_AB, result_AC, result_AD, result_BC, result_BD, result_CD)
}

# Access the test results for each column
for (key in names(results_list)) {
  result <- results_list[[key]]
  
  cat("Column:", key, "\n")
  #cat("Comparison AB - Wilcoxon test statistic:", result[[1]]$statistic, "\n")
  cat("Comparison AB - Wilcoxon p-value:", result[[1]]$p.value, "\n")
  #cat("Comparison AC - Wilcoxon test statistic:", result[[2]]$statistic, "\n")
  cat("Comparison AC - Wilcoxon p-value:", result[[2]]$p.value, "\n")
  #cat("Comparison AD - Wilcoxon test statistic:", result[[3]]$statistic, "\n")
  cat("Comparison AD - Wilcoxon p-value:", result[[3]]$p.value, "\n")
  #cat("Comparison BC - Wilcoxon test statistic:", result[[4]]$statistic, "\n")
  cat("Comparison BC - Wilcoxon p-value:", result[[4]]$p.value, "\n")
  #cat("Comparison BD - Wilcoxon test statistic:", result[[5]]$statistic, "\n")
  cat("Comparison BD - Wilcoxon p-value:", result[[5]]$p.value, "\n")
  #cat("Comparison CD - Wilcoxon test statistic:", result[[6]]$statistic, "\n")
  cat("Comparison CD - Wilcoxon p-value:", result[[6]]$p.value, "\n")
  cat("\n")
}
##########################################################################
library(ggplot2)

new_labels <- c("Crashed\nCar", "Advertisement", "Flashing\nLights", "Traffic\nSign","Traffic\nSignals","Crossing\nAnimals","People\non\nfootpath","Crossing\nPeople", "Construction\nWork", "Front\nVehicle","Parallel\nVehicle","Behind\nVehicle")

# Calculate the mean rating for each driving experience group (A, B, C, D) for variables r3 to r11
experience_stats <- data %>%
  filter(r117 %in% c("Yes", "No")) %>%
  group_by(r117) %>%
  summarise(across(r12:r23, list(mean_rating = mean)))

# Reshape the data to long format
df_long <- experience_stats %>%
  pivot_longer(cols = -r117, names_to = "Variable", values_to = "Value")

# Set seed for reproducibility
num_groups <- length(unique(df_long$r117))
random_colors <- sample(colors(), num_groups)


# Plot grouped bars with modified x-axis labels and patterns
ggplot(df_long, aes(x = Variable, y = Value, fill = r117)) +
  geom_bar(position = position_dodge(), stat = "identity", width = 0.6) +
  xlab("Category") +
  ylab("Mean Rating") +
  ggtitle("How distracting are these activities?") +
  scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, by = 1)) +
  theme(axis.text.x = element_text(family = "Arial", face = "bold", size = 10),
        axis.text.y = element_text(family = "Arial", face = "bold", size = 13),
        plot.title = element_text(family = "Arial", face = "bold"),
        legend.position = "top") +
  labs(fill = "Accident while driving a car, caused due to human error") +
  #scale_fill_manual(values = random_colors, guide = FALSE)+
  scale_x_discrete(labels = new_labels)
##############################################################################
library(ggplot2)

new_labels <- c("Sending\nText\nMessage","Eating\nor\nDrinking","Music", "Convo\nwith\npassenger", "Phone\nCall", "Hand\nCall",
                "Handsfree\nCall", "Noisy\nPassenger", "Reading\nText\nMessage")

# Calculate the mean rating for each driving experience group (A, B, C, D) for variables r3 to r11
experience_stats <- data %>%
  filter(r117 %in% c("Yes", "No")) %>%
  group_by(r117) %>%
  summarise(across(r3:r11, list(mean_rating = mean)))

# Reshape the data to long format
df_long <- experience_stats %>%
  pivot_longer(cols = -r117, names_to = "Variable", values_to = "Value")

# Set seed for reproducibility
num_groups <- length(unique(df_long$r117))
random_colors <- sample(colors(), num_groups)


# Plot grouped bars with modified x-axis labels and patterns
ggplot(df_long, aes(x = Variable, y = Value, fill = r117)) +
  geom_bar(position = position_dodge(), stat = "identity", width = 0.6) +
  xlab("Category") +
  ylab("Mean Rating") +
  ggtitle("How distracting are these activities?") +
  scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, by = 1)) +
  theme(axis.text.x = element_text(family = "Arial", face = "bold", size = 10),
        axis.text.y = element_text(family = "Arial", face = "bold", size = 13),
        plot.title = element_text(family = "Arial", face = "bold"),
        legend.position = "top") +
  labs(fill = "Accident while driving a car, caused due to human error") +
  #scale_fill_manual(values = random_colors, guide = FALSE)+
  scale_x_discrete(labels = new_labels)
###############################################################################3
library(ggplot2)

new_labels <- c("Sending\nText\nMessage","Eating\nor\nDrinking","Music", "Convo\nwith\npassenger", "Phone\nCall", "Hand\nCall",
                "Handsfree\nCall", "Noisy\nPassenger", "Reading\nText\nMessage")

# Calculate the mean rating for each driving experience group (A, B, C, D) for variables r3 to r11
experience_stats <- data %>%
  filter(r86 %in% c("Yes", "No")) %>%
  group_by(r86) %>%
  summarise(across(r3:r11, list(mean_rating = mean)))

# Reshape the data to long format
df_long <- experience_stats %>%
  pivot_longer(cols = -r86, names_to = "Variable", values_to = "Value")

# Set seed for reproducibility
num_groups <- length(unique(df_long$r86))
random_colors <- sample(colors(), num_groups)


# Plot grouped bars with modified x-axis labels and patterns
ggplot(df_long, aes(x = Variable, y = Value, fill = r86)) +
  geom_bar(position = position_dodge(), stat = "identity", width = 0.6) +
  xlab("Category") +
  ylab("Mean Rating") +
  ggtitle("How distracting are these activities?") +
  scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, by = 1)) +
  theme(axis.text.x = element_text(family = "Arial", face = "bold", size = 10),
        axis.text.y = element_text(family = "Arial", face = "bold", size = 13),
        plot.title = element_text(family = "Arial", face = "bold"),
        legend.position = "top") +
  labs(fill = "ADAS Experience") +
  #scale_fill_manual(values = random_colors, guide = FALSE)+
  scale_x_discrete(labels = new_labels)
###########################################################################
library(ggplot2)

new_labels <- c("Sending\nText\nMessage","Eating\nor\nDrinking","Music", "Convo\nwith\npassenger", "Phone\nCall", "Hand\nCall",
                "Handsfree\nCall", "Noisy\nPassenger", "Reading\nText\nMessage")

# Calculate the mean rating for each driving experience group (A, B, C, D) for variables r3 to r11
experience_stats <- data %>%
  filter(r117 %in% c("Yes", "No")) %>%
  group_by(r117) %>%
  summarise(across(r3:r11, list(mean_rating = mean)))

# Reshape the data to long format
df_long <- experience_stats %>%
  pivot_longer(cols = -r117, names_to = "Variable", values_to = "Value")

# Set seed for reproducibility
num_groups <- length(unique(df_long$r117))
random_colors <- sample(colors(), num_groups)


# Plot grouped bars with modified x-axis labels and patterns
ggplot(df_long, aes(x = Variable, y = Value, fill = r117)) +
  geom_bar(position = position_dodge(), stat = "identity", width = 0.6) +
  xlab("Category") +
  ylab("Mean Rating") +
  ggtitle("How distracting are these activities?") +
  scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, by = 1)) +
  theme(axis.text.x = element_text(family = "Arial", face = "bold", size = 10),
        axis.text.y = element_text(family = "Arial", face = "bold", size = 13),
        plot.title = element_text(family = "Arial", face = "bold"),
        legend.position = "top") +
  labs(fill = "Accident while driving a car, caused due to human error") +
  #scale_fill_manual(values = random_colors, guide = FALSE)+
  scale_x_discrete(labels = new_labels)
###############################################################################3
library(ggplot2)

#new_labels <- c("At\nHigher\nElevation", "With\nFlashing\nLights", "At\nRoad", "On\nMedian")

# Calculate the mean rating for each driving experience group (A, B, C, D) for variables r3 to r11
experience_stats <- data %>%
  filter(r86 %in% c("Yes", "No")) %>%
  group_by(r86) %>%
  summarise(across(r24:r27, list(mean_rating = mean)))

# Reshape the data to long format
df_long <- experience_stats %>%
  pivot_longer(cols = -r86, names_to = "Variable", values_to = "Value")

# Set seed for reproducibility
num_groups <- length(unique(df_long$r86))
random_colors <- sample(colors(), num_groups)


# Plot grouped bars with modified x-axis labels and patterns
ggplot(df_long, aes(x = Variable, y = Value, fill = r86)) +
  geom_bar(position = position_dodge(), stat = "identity", width = 0.6) +
  xlab("Category") +
  ylab("Mean Rating") +
  ggtitle("How distracting are these activities?") +
  scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, by = 1)) +
  theme(axis.text.x = element_text(family = "Arial", face = "bold", size = 10),
        axis.text.y = element_text(family = "Arial", face = "bold", size = 13),
        plot.title = element_text(family = "Arial", face = "bold"),
        legend.position = "top") +
  labs(fill = "ADAS Experience") 
  #scale_fill_manual(values = random_colors, guide = FALSE)+
  #scale_x_discrete(labels = new_labels)
##################################################################
library(ggplot2)

new_labels <- c("At\nHigher\nElevation", "With\nFlashing\nLights", "At\nRoad", "On\nMedian")

# Calculate the mean rating for each driving experience group (A, B, C, D) for variables r3 to r11
experience_stats <- data %>%
  filter(r100 %in% c("Not safe", "Not sure about it","Safe to some extent","Very safe")) %>%
  group_by(r100) %>%
  summarise(across(r24:r27, list(mean_rating = mean)))

# Reshape the data to long format
df_long <- experience_stats %>%
  pivot_longer(cols = -r100, names_to = "Variable", values_to = "Value")

# Set seed for reproducibility
num_groups <- length(unique(df_long$r100))
random_colors <- sample(colors(), num_groups)


# Plot grouped bars with modified x-axis labels and patterns
ggplot(df_long, aes(x = Variable, y = Value, fill = r100)) +
  geom_bar(position = position_dodge(), stat = "identity", width = 0.6) +
  xlab("Category") +
  ylab("Mean Rating") +
  ggtitle("How distracting are these activities?") +
  scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, by = 1)) +
  theme(axis.text.x = element_text(family = "Arial", face = "bold", size = 10),
        axis.text.y = element_text(family = "Arial", face = "bold", size = 13),
        plot.title = element_text(family = "Arial", face = "bold"),
        legend.position = "top") +
  labs(fill = "Is AV safe?") +
  #scale_fill_manual(values = random_colors, guide = FALSE)+
  scale_x_discrete(labels = new_labels)
####################################################################
library(ggplot2)

#new_labels <- c("Music", "Convo\nwith\npassenger", "Phone\nCall", "Hand-Call", "Handsfree\nCall", "Noisy\nPassenger", "Reading\nText\nMessage", "Sending\nText\nMessage", "Eating\nor\nDrinking")

# Calculate the mean rating for each driving experience group (A, B, C, D) for variables r3 to r11
experience_stats <- data %>%
  filter(r100 %in% c("Not safe", "Not sure about it","Safe to some extent","Very safe")) %>%
  group_by(r100) %>%
  summarise(across(r3:r11, list(mean_rating = mean)))

# Reshape the data to long format
df_long <- experience_stats %>%
  pivot_longer(cols = -r100, names_to = "Variable", values_to = "Value")

# Set seed for reproducibility
num_groups <- length(unique(df_long$r100))
random_colors <- sample(colors(), num_groups)


# Plot grouped bars with modified x-axis labels and patterns
ggplot(df_long, aes(x = Variable, y = Value, fill = r100)) +
  geom_bar(position = position_dodge(), stat = "identity", width = 0.6) +
  xlab("Category") +
  ylab("Mean Rating") +
  ggtitle("How distracting are these activities?") +
  scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, by = 1)) +
  theme(axis.text.x = element_text(family = "Arial", face = "bold", size = 10),
        axis.text.y = element_text(family = "Arial", face = "bold", size = 13),
        plot.title = element_text(family = "Arial", face = "bold"),
        legend.position = "top") +
  labs(fill = "Is AV safe?") +
  #scale_fill_manual(values = random_colors, guide = FALSE)+
  #scale_x_discrete(labels = new_labels)
###########################################################################
library(ggplot2)

new_labels <- c("Crashed\nCar", "Advertisement", "Flashing\nLights", "Traffic\nSign","Traffic\nSignals","Crossing\nAnimals","People\non\nfootpath","Crossing\nPeople", "Construction\nWork", "Front\nVehicle","Parallel\nVehicle","Behind\nVehicle")

# Calculate the mean rating for each driving experience group (A, B, C, D) for variables r3 to r11
experience_stats <- data %>%
  filter(r100 %in% c("Not safe", "Not sure about it","Safe to some extent","Very safe")) %>%
  group_by(r100) %>%
  summarise(across(r12:r23, list(mean_rating = mean)))

# Reshape the data to long format
df_long <- experience_stats %>%
  pivot_longer(cols = -r100, names_to = "Variable", values_to = "Value")

# Set seed for reproducibility
num_groups <- length(unique(df_long$r100))
random_colors <- sample(colors(), num_groups)


# Plot grouped bars with modified x-axis labels and patterns
ggplot(df_long, aes(x = Variable, y = Value, fill = r100)) +
  geom_bar(position = position_dodge(), stat = "identity", width = 0.6) +
  xlab("Category") +
  ylab("Mean Rating") +
  ggtitle("How distracting are these activities?") +
  scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, by = 1)) +
  theme(axis.text.x = element_text(family = "Arial", face = "bold", size = 10),
        axis.text.y = element_text(family = "Arial", face = "bold", size = 13),
        plot.title = element_text(family = "Arial", face = "bold"),
        legend.position = "top") +
  labs(fill = "Is AV safe?") +
  #scale_fill_manual(values = random_colors, guide = FALSE)+
  scale_x_discrete(labels = new_labels)
#######################################################################
group_A_data <- subset(data, data$r86 == "Yes", select = c("r121", "r3", "r4", "r5", "r6","r7","r8","r9","r10","r11"))
group_B_data <- subset(data, data$r86 == "No", select = c("r121", "r3", "r4", "r5", "r6","r7","r8","r9","r10","r11"))
#group_C_data <- subset(data, data$r121 == "More than 10 years", select = c("r121", "r24", "r25", "r26", "r27"))
#group_D_data <- subset(data, data$r121 == "No Experience", select = c("r121", "r24", "r25", "r26", "r27"))

# Define the column names for distraction ratings
rating_columns <- c( "r3", "r4", "r5", "r6","r7","r8","r9","r10","r11")

# Create an empty list to store the results
results_list <- list()

# Iterate over each column and perform the Wilcoxon signed-rank test for paired samples
for (column in rating_columns) {
  # Subset the data for the specific column and driving experience group
  group_A_ratings <- group_A_data[[column]]
  group_B_ratings <- group_B_data[[column]]
  #group_C_ratings <- group_C_data[[column]]
  #group_D_ratings <- group_D_data[[column]]
  
  # Identify the smallest sample size among the driving experience groups
  n <- min(length(group_A_ratings), length(group_B_ratings)) #length(group_C_ratings), length(group_D_ratings))
  
  # Perform the Wilcoxon signed-rank test with aligned pairs for each driving experience group
  result_AB <- wilcox.test(group_A_ratings[1:n], group_B_ratings[1:n], paired = TRUE)
  #result_AC <- wilcox.test(group_A_ratings[1:n], group_C_ratings[1:n], paired = TRUE)
  #result_AD <- wilcox.test(group_A_ratings[1:n], group_D_ratings[1:n], paired = TRUE)
  #result_BC <- wilcox.test(group_B_ratings[1:n], group_C_ratings[1:n], paired = TRUE)
  #result_BD <- wilcox.test(group_B_ratings[1:n], group_D_ratings[1:n], paired = TRUE)
  #result_CD <- wilcox.test(group_C_ratings[1:n], group_D_ratings[1:n], paired = TRUE)
  
  # Create a unique key for the result list
  result_key <- column
  
  # Store the results in the list
  results_list[[result_key]] <- list(result_AB)# result_AC, result_AD, result_BC, result_BD, result_CD)
}

# Access the test results for each column
for (key in names(results_list)) {
  result <- results_list[[key]]
  
  cat("Column:", key, "\n")
  #cat("Comparison AB - Wilcoxon test statistic:", result[[1]]$statistic, "\n")
  cat("Comparison AB - Wilcoxon p-value:", result[[1]]$p.value, "\n")
  #cat("Comparison AC - Wilcoxon test statistic:", result[[2]]$statistic, "\n")
  #cat("Comparison AC - Wilcoxon p-value:", result[[2]]$p.value, "\n")
  #cat("Comparison AD - Wilcoxon test statistic:", result[[3]]$statistic, "\n")
  #cat("Comparison AD - Wilcoxon p-value:", result[[3]]$p.value, "\n")
  #cat("Comparison BC - Wilcoxon test statistic:", result[[4]]$statistic, "\n")
  #cat("Comparison BC - Wilcoxon p-value:", result[[4]]$p.value, "\n")
  #cat("Comparison BD - Wilcoxon test statistic:", result[[5]]$statistic, "\n")
  #cat("Comparison BD - Wilcoxon p-value:", result[[5]]$p.value, "\n")
  #cat("Comparison CD - Wilcoxon test statistic:", result[[6]]$statistic, "\n")
  #cat("Comparison CD - Wilcoxon p-value:", result[[6]]$p.value, "\n")
  cat("\n")
}
############################################################
group_A_data <- subset(data, data$r86 == "Yes", select = c("r121", "r12", "r13", "r14", "r15","r16","r17","r18","r19","r20","r21","r23"))
group_B_data <- subset(data, data$r86 == "No", select = c("r121", "r12", "r13", "r14", "r15","r16","r17","r18","r19","r20","r21","r23"))
#group_C_data <- subset(data, data$r121 == "More than 10 years", select = c("r121", "r24", "r25", "r26", "r27"))
#group_D_data <- subset(data, data$r121 == "No Experience", select = c("r121", "r24", "r25", "r26", "r27"))

# Define the column names for distraction ratings
rating_columns <- c( "r12", "r13", "r14", "r15","r16","r17","r18","r19","r20","r21","r23")

# Create an empty list to store the results
results_list <- list()

# Iterate over each column and perform the Wilcoxon signed-rank test for paired samples
for (column in rating_columns) {
  # Subset the data for the specific column and driving experience group
  group_A_ratings <- group_A_data[[column]]
  group_B_ratings <- group_B_data[[column]]
  #group_C_ratings <- group_C_data[[column]]
  #group_D_ratings <- group_D_data[[column]]
  
  # Identify the smallest sample size among the driving experience groups
  n <- min(length(group_A_ratings), length(group_B_ratings)) #length(group_C_ratings), length(group_D_ratings))
  
  # Perform the Wilcoxon signed-rank test with aligned pairs for each driving experience group
  result_AB <- wilcox.test(group_A_ratings[1:n], group_B_ratings[1:n], paired = TRUE)
  #result_AC <- wilcox.test(group_A_ratings[1:n], group_C_ratings[1:n], paired = TRUE)
  #result_AD <- wilcox.test(group_A_ratings[1:n], group_D_ratings[1:n], paired = TRUE)
  #result_BC <- wilcox.test(group_B_ratings[1:n], group_C_ratings[1:n], paired = TRUE)
  #result_BD <- wilcox.test(group_B_ratings[1:n], group_D_ratings[1:n], paired = TRUE)
  #result_CD <- wilcox.test(group_C_ratings[1:n], group_D_ratings[1:n], paired = TRUE)
  
  # Create a unique key for the result list
  result_key <- column
  
  # Store the results in the list
  results_list[[result_key]] <- list(result_AB)# result_AC, result_AD, result_BC, result_BD, result_CD)
}

# Access the test results for each column
for (key in names(results_list)) {
  result <- results_list[[key]]
  
  cat("Column:", key, "\n")
  #cat("Comparison AB - Wilcoxon test statistic:", result[[1]]$statistic, "\n")
  cat("Comparison AB - Wilcoxon p-value:", result[[1]]$p.value, "\n")
  #cat("Comparison AC - Wilcoxon test statistic:", result[[2]]$statistic, "\n")
  #cat("Comparison AC - Wilcoxon p-value:", result[[2]]$p.value, "\n")
  #cat("Comparison AD - Wilcoxon test statistic:", result[[3]]$statistic, "\n")
  #cat("Comparison AD - Wilcoxon p-value:", result[[3]]$p.value, "\n")
  #cat("Comparison BC - Wilcoxon test statistic:", result[[4]]$statistic, "\n")
  #cat("Comparison BC - Wilcoxon p-value:", result[[4]]$p.value, "\n")
  #cat("Comparison BD - Wilcoxon test statistic:", result[[5]]$statistic, "\n")
  #cat("Comparison BD - Wilcoxon p-value:", result[[5]]$p.value, "\n")
  #cat("Comparison CD - Wilcoxon test statistic:", result[[6]]$statistic, "\n")
  #cat("Comparison CD - Wilcoxon p-value:", result[[6]]$p.value, "\n")
  cat("\n")
}

##########################################################################

group_A_data <- subset(data, data$r86 == "Yes", select = c("r121", "r24", "r25", "r26", "r27"))
group_B_data <- subset(data, data$r86 == "No", select = c("r121", "r24", "r25", "r26", "r27"))
#group_C_data <- subset(data, data$r121 == "More than 10 years", select = c("r121", "r24", "r25", "r26", "r27"))
#group_D_data <- subset(data, data$r121 == "No Experience", select = c("r121", "r24", "r25", "r26", "r27"))

# Define the column names for distraction ratings
rating_columns <- c( "r24", "r25", "r26", "r27")

# Create an empty list to store the results
results_list <- list()

# Iterate over each column and perform the Wilcoxon signed-rank test for paired samples
for (column in rating_columns) {
  # Subset the data for the specific column and driving experience group
  group_A_ratings <- group_A_data[[column]]
  group_B_ratings <- group_B_data[[column]]
  #group_C_ratings <- group_C_data[[column]]
  #group_D_ratings <- group_D_data[[column]]
  
  # Identify the smallest sample size among the driving experience groups
  n <- min(length(group_A_ratings), length(group_B_ratings)) #length(group_C_ratings), length(group_D_ratings))
  
  # Perform the Wilcoxon signed-rank test with aligned pairs for each driving experience group
  result_AB <- wilcox.test(group_A_ratings[1:n], group_B_ratings[1:n], paired = TRUE)
  #result_AC <- wilcox.test(group_A_ratings[1:n], group_C_ratings[1:n], paired = TRUE)
  #result_AD <- wilcox.test(group_A_ratings[1:n], group_D_ratings[1:n], paired = TRUE)
  #result_BC <- wilcox.test(group_B_ratings[1:n], group_C_ratings[1:n], paired = TRUE)
  #result_BD <- wilcox.test(group_B_ratings[1:n], group_D_ratings[1:n], paired = TRUE)
  #result_CD <- wilcox.test(group_C_ratings[1:n], group_D_ratings[1:n], paired = TRUE)
  
  # Create a unique key for the result list
  result_key <- column
  
  # Store the results in the list
  results_list[[result_key]] <- list(result_AB)# result_AC, result_AD, result_BC, result_BD, result_CD)
}

# Access the test results for each column
for (key in names(results_list)) {
  result <- results_list[[key]]
  
  cat("Column:", key, "\n")
  #cat("Comparison AB - Wilcoxon test statistic:", result[[1]]$statistic, "\n")
  cat("Comparison AB - Wilcoxon p-value:", result[[1]]$p.value, "\n")
  #cat("Comparison AC - Wilcoxon test statistic:", result[[2]]$statistic, "\n")
  #cat("Comparison AC - Wilcoxon p-value:", result[[2]]$p.value, "\n")
  #cat("Comparison AD - Wilcoxon test statistic:", result[[3]]$statistic, "\n")
  #cat("Comparison AD - Wilcoxon p-value:", result[[3]]$p.value, "\n")
  #cat("Comparison BC - Wilcoxon test statistic:", result[[4]]$statistic, "\n")
  #cat("Comparison BC - Wilcoxon p-value:", result[[4]]$p.value, "\n")
  #cat("Comparison BD - Wilcoxon test statistic:", result[[5]]$statistic, "\n")
  #cat("Comparison BD - Wilcoxon p-value:", result[[5]]$p.value, "\n")
  #cat("Comparison CD - Wilcoxon test statistic:", result[[6]]$statistic, "\n")
  #cat("Comparison CD - Wilcoxon p-value:", result[[6]]$p.value, "\n")
  cat("\n")
}
############################################################################
group_A_data <- subset(data, data$r100 == "Not safe", select = c("r121", "r3", "r4", "r5", "r6","r7","r8","r9","r10","r11"))
group_B_data <- subset(data, data$r100 == "Not sure about it", select = c("r121", "r3", "r4", "r5", "r6","r7","r8","r9","r10","r11"))
group_C_data <- subset(data, data$r100 == "Safe to some extent", select = c("r121", "r3", "r4", "r5", "r6","r7","r8","r9","r10","r11"))
group_D_data <- subset(data, data$r100 == "Very safe", select = c("r121", "r12", "r3", "r4", "r5", "r6","r7","r8","r9","r10","r11"))

# Define the column names for distraction ratings
rating_columns <- c( "r3", "r4", "r5", "r6","r7","r8","r9","r10","r11")

# Create an empty list to store the results
results_list <- list()

# Iterate over each column and perform the Wilcoxon signed-rank test for paired samples
for (column in rating_columns) {
  # Subset the data for the specific column and driving experience group
  group_A_ratings <- group_A_data[[column]]
  group_B_ratings <- group_B_data[[column]]
  group_C_ratings <- group_C_data[[column]]
  group_D_ratings <- group_D_data[[column]]
  
  # Identify the smallest sample size among the driving experience groups
  n <- min(length(group_A_ratings), length(group_B_ratings),length(group_C_ratings), length(group_D_ratings))
  
  # Perform the Wilcoxon signed-rank test with aligned pairs for each driving experience group
  result_AB <- wilcox.test(group_A_ratings[1:n], group_B_ratings[1:n], paired = TRUE)
  result_AC <- wilcox.test(group_A_ratings[1:n], group_C_ratings[1:n], paired = TRUE)
  result_AD <- wilcox.test(group_A_ratings[1:n], group_D_ratings[1:n], paired = TRUE)
  result_BC <- wilcox.test(group_B_ratings[1:n], group_C_ratings[1:n], paired = TRUE)
  result_BD <- wilcox.test(group_B_ratings[1:n], group_D_ratings[1:n], paired = TRUE)
  result_CD <- wilcox.test(group_C_ratings[1:n], group_D_ratings[1:n], paired = TRUE)
  
  # Create a unique key for the result list
  result_key <- column
  
  # Store the results in the list
  results_list[[result_key]] <- list(result_AB, result_AC, result_AD, result_BC, result_BD, result_CD)
}

# Access the test results for each column
for (key in names(results_list)) {
  result <- results_list[[key]]
  
  cat("Column:", key, "\n")
  #cat("Comparison AB - Wilcoxon test statistic:", result[[1]]$statistic, "\n")
  cat("Comparison AB - Wilcoxon p-value:", result[[1]]$p.value, "\n")
  #cat("Comparison AC - Wilcoxon test statistic:", result[[2]]$statistic, "\n")
  cat("Comparison AC - Wilcoxon p-value:", result[[2]]$p.value, "\n")
  #cat("Comparison AD - Wilcoxon test statistic:", result[[3]]$statistic, "\n")
  cat("Comparison AD - Wilcoxon p-value:", result[[3]]$p.value, "\n")
  #cat("Comparison BC - Wilcoxon test statistic:", result[[4]]$statistic, "\n")
  cat("Comparison BC - Wilcoxon p-value:", result[[4]]$p.value, "\n")
  #cat("Comparison BD - Wilcoxon test statistic:", result[[5]]$statistic, "\n")
  cat("Comparison BD - Wilcoxon p-value:", result[[5]]$p.value, "\n")
  #cat("Comparison CD - Wilcoxon test statistic:", result[[6]]$statistic, "\n")
  cat("Comparison CD - Wilcoxon p-value:", result[[6]]$p.value, "\n")
  cat("\n")
}
#############################################################################3
eq_scores <- data$r125
# Define the number of desired bins/groups
num_bins <- 3

# Calculate the interval width
eq_range <- range(eq_scores)
interval_width <- diff(eq_range) / num_bins

# Create the EQ score groups
eq_groups <- cut(eq_scores, breaks = seq(eq_range[1], eq_range[2], by = interval_width), labels = FALSE, include.lowest = TRUE)

# Calculate the values at which EQ scores are divided
group_values <- unique(eq_scores[eq_groups])

# Print the EQ score groups and their corresponding values
cat("EQ Score Groups and Values:\n")
for (i in 1:num_bins) {
  group <- paste("Group", i, ":", sep = " ")
  values <- paste(group_values[i], "-", group_values[i+1], sep = "")
  cat(group, values, "\n")
}
#######################################################################
library(cluster)

# Example EQ scores
eq_scores <- data$r125

# Define the range of k (number of clusters) to consider
k_values <- 1:10

# Initialize an empty vector to store the WCSS values
wcss <- vector()

# Calculate the WCSS for each value of k
for (k in k_values) {
  kmeans_model <- kmeans(eq_scores, centers = k)
  wcss[k] <- kmeans_model$tot.withinss
}

# Plot the elbow curve
plot(k_values, wcss, type = "b", pch = 19, frame = FALSE,
     xlab = "Number of Clusters (k)", ylab = "Within-Cluster Sum of Squares (WCSS)",
     main = "Elbow Plot")

# Add a line indicating the elbow point
elbow_point <- k_values[which(diff(wcss) == min(diff(wcss)))]
abline(v = elbow_point, lty = 2, col = "red")

# Print the optimal number of clusters
cat("Optimal Number of Clusters (Elbow Point):", elbow_point, "\n")
########################################################################
eq_scores <- data$r125

# Define the number of clusters
k <- 2

# Perform k-means clustering
kmeans_model <- kmeans(eq_scores, centers = k)

# Calculate the silhouette scores
silhouette_scores <- silhouette(kmeans_model$cluster, dist(eq_scores))

# Print the silhouette scores
cat("Silhouette Scores:\n")


# Plot the silhouette plot
plot(silhouette_scores, main = "Silhouette Plot")
print(silhouette_scores)
###################
library(cluster)

# Example EQ scores
eq_scores <- data$r125

# Define the number of clusters
k <- 2

# Perform k-means clustering
kmeans_model <- kmeans(eq_scores, centers = k)

# Get the cluster centroids
cluster_centroids <- kmeans_model$centers

# Determine the threshold as the midpoint between the cluster centroids
threshold <- mean(cluster_centroids)

# Print the threshold
cat("Threshold:", threshold, "\n")

# Divide the scores into two groups based on the threshold
group1 <- eq_scores[eq_scores <= threshold]
group2 <- eq_scores[eq_scores > threshold]

# Print the groups
cat("Group 1 (EQ scores <= threshold):\n")
print(group1)

cat("Group 2 (EQ scores > threshold):\n")
print(group2)
#############################################################################
library(ggplot2)

new_labels <- c("Music", "Convo\nwith\npassenger", "Phone\nCall", "Hand-Call", "Handsfree\nCall", "Noisy\nPassenger", "Reading\nText\nMessage", "Sending\nText\nMessage", "Eating\nor\nDrinking")

# Calculate the mean rating for each driving experience group (A, B, C, D) for variables r3 to r11
experience_stats <- data %>%
  filter(r125 %in% c("HighEQ", "LowEQ")) %>%
  group_by(r125) %>%
  summarise(across(r3:r11, list(mean_rating = mean)))

# Reshape the data to long format
df_long <- experience_stats %>%
  pivot_longer(cols = -r125, names_to = "Variable", values_to = "Value")

# Set seed for reproducibility
num_groups <- length(unique(df_long$r125))
random_colors <- sample(colors(), num_groups)


# Plot grouped bars with modified x-axis labels and patterns
ggplot(df_long, aes(x = Variable, y = Value, fill = r125)) +
  geom_bar(position = position_dodge(), stat = "identity", width = 0.6) +
  xlab("Category") +
  ylab("Mean Rating") +
  ggtitle("How distracting are these activities?") +
  scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, by = 1)) +
  theme(axis.text.x = element_text(family = "Arial", face = "bold", size = 10),
        axis.text.y = element_text(family = "Arial", face = "bold", size = 13),
        plot.title = element_text(family = "Arial", face = "bold"),
        legend.position = "top") +
  labs(fill = "EQ Distribution") +
  #scale_fill_manual(values = random_colors, guide = FALSE)+
  scale_x_discrete(labels = new_labels)
###################################################################################

library(ggplot2)

new_labels <- c("Crashed\nCar", "Advertisement", "Flashing\nLights", "Traffic\nSign","Traffic\nSignals","Crossing\nAnimals","People\non\nfootpath","Crossing\nPeople", "Construction\nWork", "Front\nVehicle","Parallel\nVehicle","Behind\nVehicle")

# Calculate the mean rating for each driving experience group (A, B, C, D) for variables r3 to r11
experience_stats <- data %>%
  filter(r125 %in% c("HighEQ", "LowEQ")) %>%
  group_by(r125) %>%
  summarise(across(r12:r23, list(mean_rating = mean)))

# Reshape the data to long format
df_long <- experience_stats %>%
  pivot_longer(cols = -r125, names_to = "Variable", values_to = "Value")

# Set seed for reproducibility
num_groups <- length(unique(df_long$r125))
random_colors <- sample(colors(), num_groups)


# Plot grouped bars with modified x-axis labels and patterns
ggplot(df_long, aes(x = Variable, y = Value, fill = r125)) +
  geom_bar(position = position_dodge(), stat = "identity", width = 0.6) +
  xlab("Category") +
  ylab("Mean Rating") +
  ggtitle("How distracting are these activities?") +
  scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, by = 1)) +
  theme(axis.text.x = element_text(family = "Arial", face = "bold", size = 10),
        axis.text.y = element_text(family = "Arial", face = "bold", size = 13),
        plot.title = element_text(family = "Arial", face = "bold"),
        legend.position = "top") +
  labs(fill = "EQ Distribution") +
  #scale_fill_manual(values = random_colors, guide = FALSE)+
  scale_x_discrete(labels = new_labels)
######################################################################
library(ggplot2)

new_labels <- c("At\nHigher\nElevation", "With\nFlashing\nLights", "At\nRoad", "On\nMedian")

# Calculate the mean rating for each driving experience group (A, B, C, D) for variables r3 to r11
experience_stats <- data %>%
  filter(r125 %in% c("HighEQ", "LowEQ")) %>%
  group_by(r125) %>%
  summarise(across(r24:r27, list(mean_rating = mean)))

# Reshape the data to long format
df_long <- experience_stats %>%
  pivot_longer(cols = -r125, names_to = "Variable", values_to = "Value")

# Set seed for reproducibility
num_groups <- length(unique(df_long$r125))
random_colors <- sample(colors(), num_groups)


# Plot grouped bars with modified x-axis labels and patterns
ggplot(df_long, aes(x = Variable, y = Value, fill = r125)) +
  geom_bar(position = position_dodge(), stat = "identity", width = 0.6) +
  xlab("Category") +
  ylab("Mean Rating") +
  ggtitle("How distracting are these activities?") +
  scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, by = 1)) +
  theme(axis.text.x = element_text(family = "Arial", face = "bold", size = 10),
        axis.text.y = element_text(family = "Arial", face = "bold", size = 13),
        plot.title = element_text(family = "Arial", face = "bold"),
        legend.position = "top") +
  labs(fill = "EQ Distribution") +
  scale_fill_manual(values = random_colors, guide = FALSE) +
  scale_x_discrete(labels = new_labels)
################################################################################3
group_A_data <- subset(data, data$r125 == "HighEQ", select = c("r125", "r24", "r25", "r26", "r27"))
group_B_data <- subset(data, data$r125 == "LowEQ", select = c("r125", "r24", "r25", "r26", "r27"))
#group_C_data <- subset(data, data$r121 == "More than 10 years", select = c("r121", "r24", "r25", "r26", "r27"))
#group_D_data <- subset(data, data$r121 == "No Experience", select = c("r121", "r24", "r25", "r26", "r27"))

# Define the column names for distraction ratings
rating_columns <- c( "r24", "r25", "r26", "r27")

# Create an empty list to store the results
results_list <- list()

# Iterate over each column and perform the Wilcoxon signed-rank test for paired samples
for (column in rating_columns) {
  # Subset the data for the specific column and driving experience group
  group_A_ratings <- group_A_data[[column]]
  group_B_ratings <- group_B_data[[column]]
  #group_C_ratings <- group_C_data[[column]]
  #group_D_ratings <- group_D_data[[column]]
  
  # Identify the smallest sample size among the driving experience groups
  n <- min(length(group_A_ratings), length(group_B_ratings)) #length(group_C_ratings), length(group_D_ratings))
  
  # Perform the Wilcoxon signed-rank test with aligned pairs for each driving experience group
  result_AB <- wilcox.test(group_A_ratings[1:n], group_B_ratings[1:n], paired = TRUE)
  #result_AC <- wilcox.test(group_A_ratings[1:n], group_C_ratings[1:n], paired = TRUE)
  #result_AD <- wilcox.test(group_A_ratings[1:n], group_D_ratings[1:n], paired = TRUE)
  #result_BC <- wilcox.test(group_B_ratings[1:n], group_C_ratings[1:n], paired = TRUE)
  #result_BD <- wilcox.test(group_B_ratings[1:n], group_D_ratings[1:n], paired = TRUE)
  #result_CD <- wilcox.test(group_C_ratings[1:n], group_D_ratings[1:n], paired = TRUE)
  
  # Create a unique key for the result list
  result_key <- column
  
  # Store the results in the list
  results_list[[result_key]] <- list(result_AB)# result_AC, result_AD, result_BC, result_BD, result_CD)
}

# Access the test results for each column
for (key in names(results_list)) {
  result <- results_list[[key]]
  
  cat("Column:", key, "\n")
  #cat("Comparison AB - Wilcoxon test statistic:", result[[1]]$statistic, "\n")
  cat("Comparison AB - Wilcoxon p-value:", result[[1]]$p.value, "\n")
  #cat("Comparison AC - Wilcoxon test statistic:", result[[2]]$statistic, "\n")
  #cat("Comparison AC - Wilcoxon p-value:", result[[2]]$p.value, "\n")
  #cat("Comparison AD - Wilcoxon test statistic:", result[[3]]$statistic, "\n")
  #cat("Comparison AD - Wilcoxon p-value:", result[[3]]$p.value, "\n")
  #cat("Comparison BC - Wilcoxon test statistic:", result[[4]]$statistic, "\n")
  #cat("Comparison BC - Wilcoxon p-value:", result[[4]]$p.value, "\n")
  #cat("Comparison BD - Wilcoxon test statistic:", result[[5]]$statistic, "\n")
  #cat("Comparison BD - Wilcoxon p-value:", result[[5]]$p.value, "\n")
  #cat("Comparison CD - Wilcoxon test statistic:", result[[6]]$statistic, "\n")
  #cat("Comparison CD - Wilcoxon p-value:", result[[6]]$p.value, "\n")
  cat("\n")
}
########################################################################
group_A_data <- subset(data, data$r125 == "HighEQ", select = c("r125", "r3", "r4", "r5", "r6","r7","r8","r9","r10","r11"))
group_B_data <- subset(data, data$r125 == "LowEQ", select = c("r125", "r3", "r4", "r5", "r6","r7","r8","r9","r10","r11"))
#group_C_data <- subset(data, data$r121 == "More than 10 years", select = c("r121", "r24", "r25", "r26", "r27"))
#group_D_data <- subset(data, data$r121 == "No Experience", select = c("r121", "r24", "r25", "r26", "r27"))

# Define the column names for distraction ratings
rating_columns <- c( "r3", "r4", "r5", "r6","r7","r8","r9","r10","r11")

# Create an empty list to store the results
results_list <- list()

# Iterate over each column and perform the Wilcoxon signed-rank test for paired samples
for (column in rating_columns) {
  # Subset the data for the specific column and driving experience group
  group_A_ratings <- group_A_data[[column]]
  group_B_ratings <- group_B_data[[column]]
  #group_C_ratings <- group_C_data[[column]]
  #group_D_ratings <- group_D_data[[column]]
  
  # Identify the smallest sample size among the driving experience groups
  n <- min(length(group_A_ratings), length(group_B_ratings)) #length(group_C_ratings), length(group_D_ratings))
  
  # Perform the Wilcoxon signed-rank test with aligned pairs for each driving experience group
  result_AB <- wilcox.test(group_A_ratings[1:n], group_B_ratings[1:n], paired = TRUE)
  #result_AC <- wilcox.test(group_A_ratings[1:n], group_C_ratings[1:n], paired = TRUE)
  #result_AD <- wilcox.test(group_A_ratings[1:n], group_D_ratings[1:n], paired = TRUE)
  #result_BC <- wilcox.test(group_B_ratings[1:n], group_C_ratings[1:n], paired = TRUE)
  #result_BD <- wilcox.test(group_B_ratings[1:n], group_D_ratings[1:n], paired = TRUE)
  #result_CD <- wilcox.test(group_C_ratings[1:n], group_D_ratings[1:n], paired = TRUE)
  
  # Create a unique key for the result list
  result_key <- column
  
  # Store the results in the list
  results_list[[result_key]] <- list(result_AB)# result_AC, result_AD, result_BC, result_BD, result_CD)
}

# Access the test results for each column
for (key in names(results_list)) {
  result <- results_list[[key]]
  
  cat("Column:", key, "\n")
  #cat("Comparison AB - Wilcoxon test statistic:", result[[1]]$statistic, "\n")
  cat("Comparison AB - Wilcoxon p-value:", result[[1]]$p.value, "\n")
  #cat("Comparison AC - Wilcoxon test statistic:", result[[2]]$statistic, "\n")
  #cat("Comparison AC - Wilcoxon p-value:", result[[2]]$p.value, "\n")
  #cat("Comparison AD - Wilcoxon test statistic:", result[[3]]$statistic, "\n")
  #cat("Comparison AD - Wilcoxon p-value:", result[[3]]$p.value, "\n")
  #cat("Comparison BC - Wilcoxon test statistic:", result[[4]]$statistic, "\n")
  #cat("Comparison BC - Wilcoxon p-value:", result[[4]]$p.value, "\n")
  #cat("Comparison BD - Wilcoxon test statistic:", result[[5]]$statistic, "\n")
  #cat("Comparison BD - Wilcoxon p-value:", result[[5]]$p.value, "\n")
  #cat("Comparison CD - Wilcoxon test statistic:", result[[6]]$statistic, "\n")
  #cat("Comparison CD - Wilcoxon p-value:", result[[6]]$p.value, "\n")
  cat("\n")
}
################################################################################
library(ggplot2)

new_labels <- c("Sending\nText\nMessage","Eating\nor\nDrinking","Music", "Convo\nwith\npassenger", "Phone\nCall", "Hand\nCall",
                "Handsfree\nCall", "Noisy\nPassenger", "Reading\nText\nMessage")

# Calculate the mean rating for each driving experience group (A, B, C, D) for variables r3 to r11
experience_stats <- data %>%
  filter(r117 %in% c("Yes", "No")) %>%
  group_by(r117) %>%
  summarise(across(r3:r11, list(mean_rating = mean)))

# Reshape the data to long format
df_long <- experience_stats %>%
  pivot_longer(cols = -r117, names_to = "Variable", values_to = "Value")

# Set seed for reproducibility
num_groups <- length(unique(df_long$r117))
random_colors <- sample(colors(), num_groups)


# Plot grouped bars with modified x-axis labels and patterns
ggplot(df_long, aes(x = Variable, y = Value, fill = r117)) +
  geom_bar(position = position_dodge(), stat = "identity", width = 0.6) +
  xlab("Category") +
  ylab("Mean Rating") +
  ggtitle("How distracting are these activities?") +
  scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, by = 1)) +
  theme(axis.text.x = element_text(family = "Arial", face = "bold", size = 10),
        axis.text.y = element_text(family = "Arial", face = "bold", size = 13),
        plot.title = element_text(family = "Arial", face = "bold"),
        legend.position = "top") +
  labs(fill = "Accident while driving a car, caused due to human error") +
  #scale_fill_manual(values = random_colors, guide = FALSE)+
  scale_x_discrete(labels = new_labels)
######################################################################

