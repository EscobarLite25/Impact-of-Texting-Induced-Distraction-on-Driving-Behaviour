library(ggplot2)
library(tidyverse)
library(dplyr)
library(ez)
library(ggpubr)
library(sjstats)
library(lsr)
library(effsize)
library(pwr)
library(viridis)
library(showtext)
library(ggplot2)
library(extrafont)
library(compute.es)
library(metafor)
library(meta)
library(car)
library(rstatix)

data <-Text_Data

#data <- data.frame(lapply(data, factor))
num_columns <- ncol(data)

## Loop through the columns and rename them
for (i in 1:num_columns) {
  new_name <- paste0("Response", i)  # Generate the new column name
  names(data)[i] <- new_name  # Rename the column
}
data
###############################################################################
data$Response27 <- ifelse(data$Response27 <= 25, "Less than 25", "More than 25")
########################################################################################3
data <- data %>% 
  mutate(Response24  = case_when(
    Response24 == '10 to 15 years' ~'More than 5 years',
    Response24 =='5 to 10 years'~'More than 5 years',
    Response24 =='1 to 2 years'~ 'Less than 2 years',
    TRUE~Response24
  ))
data$Response24
#############################################################################################

columns_to_fill <- c("Response2", "Response3", "Response4","Response5","Response6","Response7","Response8","Response9","Response10","Response11","Response12","Response13","Response14","Response15","Response16","Response17","Response18","Response18","Response19","Response20","Response21","Response22")

for (col in columns_to_fill) {
  mean_value <- mean(data[[col]], na.rm = TRUE)
  data[[col]] <- ifelse(is.na(data[[col]]), mean_value, data[[col]])
}
###############################################################################################


##############################################################################################
column_indices <- 20:21
new_data <- data[, column_indices]
column_names <- colnames(new_data)

for (i in 1:(length(column_names) - 1)) {
  for (j in (i + 1):length(column_names)) {
    # Extract the two columns for t-test
    col1 <- data[[column_names[i]]]
    col2 <- data[[column_names[j]]]
    
    # Perform t-test
    t_test_result <- t.test(col1, col2, pair=TRUE)
    
    #Cohen.d Test
    n1 <- length(col1)
    n2 <- length(col2)
    pooled_sd <- sqrt(((n1 - 1) * var(col1) + (n2 - 1) * var(col2)) / (n1 + n2 - 2))
    cohen_d <- (mean(col1) - mean(col2)) / pooled_sd
    
    # Print results if p-value is less than 0.05
    if (t_test_result$p.value > 0.05) {
      cat("Comparison between", column_names[i], "and", column_names[j], "\n")
      cat("p-value:", t_test_result$p.value, "\n")
      cat("Cohen's d:", cohen_d, "\n")
      cat("\n")
    }
  }
}
################################################################################################
data %>%
  aov(Response28 ~ Response24, data= .)%>%  
  summary()
model <- aov(Response28 ~ Response24, data = data)
anova_results <- summary(model)

# Perform Tukey HSD test
tukey_results <- TukeyHSD(model)

# View the ANOVA summary
print(anova_results)

# View the Tukey HSD results
print(tukey_results)
###########################################################################3
shapiro.test(data$Response7)
t.test(data$Response14,data$Response15,pair=TRUE)
wilcox.test(data$Response29, data$Response30, paired = TRUE)
 ###############################################################################################

agg_tbl2 <- data %>% group_by(Response24) %>% 
  summarise(total_count=n(),
            .groups = 'drop')
df3 <- agg_tbl2 %>% as.data.frame()
df3

#################################################################################################

data$Response24 <- factor( data$Response24, levels = c("Less than 2 years", "2 to 5 years", "More than 5 years"))

mean_ratings <- aggregate(Response28 ~ Response24, data = data, FUN = mean)

# Plot the mean distraction rating as a bar plot
ggplot(mean_ratings, aes(x = Response24, y = Response28)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(x = "Driving Experience", y = "Mean Distraction Rating") +
  ggtitle("Mean Distraction Rating by Driving Experience")
#######################################################################################################
my_colors <- c("Base Condition" = "blue", "Text Condition" = "orange")

# Create the data frame
df <- data.frame(
  Category = c(rep("Base Condition", length(data$Response14)), rep("Text Condition", length(data$Response15))),
  Velocity = c(data$Response14, data$Response15)
)

# Create box plot with different colors
ggplot(df, aes(x = Category, y = Velocity, fill = Category)) +
  geom_boxplot() +
  labs(x = "Condition", y = "Velocity", title = "Box Plot") +
  scale_fill_manual(values = my_colors) +  # Set custom colors
  theme(
    axis.text.x = element_text(family = "Arial", face = "bold", size = 12),
    axis.text.y = element_text(family = "Arial", face = "bold", size = 13),
    plot.title = element_text(family = "Arial", face = "bold")
  ) +
  theme_minimal() +
  # Label box plots with numerical values
  stat_summary(
    fun = median, 
    geom = "text", 
    size=3,
    aes(label = round(..y.., 2)), 
    position = position_dodge(width = 0.75),
    vjust = -0.5
  )+
  # Label 1st quartile values
  stat_summary(
    fun = function(x) quantile(x, 0.25), 
    geom = "text", 
    size=3,
    aes(label = paste("", round(..y.., 2))), 
    position = position_dodge(width = 0.75),
    vjust = 1
  )+
  # Label 3rd quartile values
  stat_summary(
    fun = function(x) quantile(x, 0.75), 
    geom = "text", 
    aes(label = paste("", round(..y.., 2))), 
    position = position_dodge(width = 0.75),
    vjust = -0.5,
    size = 3)
##################################################################################
my_colors <- c("Base Condition" = "blue", "Text Condition" = "orange")

# Create the data frame
df <- data.frame(
  Category = c(rep("Base Condition", length(data$Response11)), rep("Text Condition", length(data$Response7))),
  Velocity = c(data$Response11, data$Response7)
)

# Create box plot with different colors
ggplot(df, aes(x = Category, y = Velocity, fill = Category)) +
  geom_boxplot() +
  labs(x = "Condition", y = "Fixation Duration", title = "Box Plot") +
  scale_fill_manual(values = my_colors) +  # Set custom colors
  theme(
    axis.text.x = element_text(family = "Arial", face = "bold", size = 12),
    axis.text.y = element_text(family = "Arial", face = "bold", size = 13),
    plot.title = element_text(family = "Arial", face = "bold")
  ) +
  theme_minimal() +
  # Label box plots with numerical values
  stat_summary(
    fun = median, 
    geom = "text", 
    size=3,
    aes(label = round(..y.., 3)), 
    position = position_dodge(width = 0.75),
    vjust = -0.5
  )+
  # Label 1st quartile values
  stat_summary(
    fun = function(x) quantile(x, 0.25), 
    geom = "text", 
    size=3,
    aes(label = paste("", round(..y.., 3))), 
    position = position_dodge(width = 0.75),
    vjust = 1
  )+
  # Label 3rd quartile values
  stat_summary(
    fun = function(x) quantile(x, 0.75), 
    geom = "text", 
    aes(label = paste("", round(..y.., 3))), 
    position = position_dodge(width = 0.75),
    vjust = -0.5,
    size = 3)
#################################################################################
df <- data.frame(
  Category = c(rep("Base Condition", length(data$Response14)), rep("Text Condition", length(data$Response15))),
  Velocity = c(data$Response14, data$Response15)
)

# Create box plot
p <- ggplot(df, aes(x = Category, y = Velocity, fill = Category)) +
  geom_boxplot() +
  labs(x = "Condition", y = "Velocity", title = "Box Plot of Velocity") +
  theme(
    axis.text.x = element_text(family = "Arial", face = "bold", size = 12),
    axis.text.y = element_text(family = "Arial", face = "bold", size = 13),
    plot.title = element_text(family = "Arial", face = "bold")
  ) +
  theme_minimal()

# Add color to the box plot
p <- p + scale_fill_manual(values = c("steelblue", "darkorange"))

# Add labels to whiskers and median
p <- p + geom_text(data = df, aes(x = Category, y = max(Velocity), label = paste("Upper Whisker:", round(max(Velocity), 2))), vjust = -1.5, size = 4) +
  geom_text(data = df, aes(x = Category, y = min(Velocity), label = paste("Lower Whisker:", round(min(Velocity), 2))), vjust = -1.5, size = 4) +
  geom_text(data = df, aes(x = Category, y = median(Velocity), label = paste("Median:", round(median(Velocity), 2))), vjust = 2, size = 4)

p
##########################################################
df <- data.frame(
  Category = c(rep("Base Condition", length(data$Response14)), rep("Text Condition", length(data$Response15))),
  Velocity = c(data$Response14, data$Response15)
)

# Create box plot
p <- ggplot(df, aes(x = Category, y = Velocity)) +
  geom_boxplot() +
  labs(x = "Condition", y = "Velocity", title = "Box Plot of Velocity") +
  theme(
    axis.text.x = element_text(family = "Arial", face = "bold", size = 12),
    axis.text.y = element_text(family = "Arial", face = "bold", size = 13),
    plot.title = element_text(family = "Arial", face = "bold")
  ) +
  theme_minimal()

# Add labels for percentile values and median
p <- p + stat_boxplot(
  geom = "text",
  aes(label = paste0("Median: ", round(median, 2))),
  vjust = -1.5,
  position = position_nudge(y = -1),
  width = 0.4,
  size = 4,
  outlier.shape = NA
) +
  stat_boxplot(
    geom = "text",
    aes(label = paste0("Q1: ", round( ymin, 2))),
    vjust = -1.5,
    position = position_nudge(y = -1),
    width = 0.4,
    size = 4,
    outlier.shape = NA
  ) +
  stat_boxplot(
    geom = "text",
    aes(label = paste0("Q3: ", round( ymax, 2))),
    vjust = -1.5,
    position = position_nudge(y = -1),
    width = 0.4,
    size = 4,
    outlier.shape = NA
  )

p
#############################################################################################3
mean_data1 <- mean(data$Response14)
mean_data2 <- mean(data$Response15)


df <- data.frame(Condition = c("Base Condition", "During Texting"), MeanCount = c(mean_data1, mean_data2))
ggplot(df, aes(x = Condition, y = MeanCount, fill = Condition)) +
  geom_bar(stat = "identity", width = 0.5) +
  labs(x = "", y = "Mean Velocity (in kmph)", title = "Mean Values Bar Plot") +
  theme(axis.text.x = element_text(family = "Arial",face="bold", size=12),
        axis.text.y = element_text(family = "Arial", face = "bold",size=13),
        plot.title = element_text(family = "Arial", face = "bold"))+
  #scale_y_continuous(limits = c(0, 450), breaks = seq(0, 450, by = 50))
  theme_minimal()
#####################################################################################

  column_indices <- 14:18
  new_data <- data[, column_indices]
  column_names <- colnames(new_data)
  
  results <- data.frame(Comparison = character(),
                        EffectSize = numeric(),
                        stringsAsFactors = FALSE)
  
  for (i in 1:(length(column_names) - 1)) {
    for (j in (i + 1):length(column_names)) {
      # Extract the two columns for t-test
      col1 <- data[[column_names[i]]]
      col2 <- data[[column_names[j]]]
      
      # Perform t-test
      t_test_result <- t.test(col1, col2, pair = TRUE)
      
      # Calculate Cohen's d
      n1 <- length(col1)
      n2 <- length(col2)
      pooled_sd <- sqrt(((n1 - 1) * var(col1) + (n2 - 1) * var(col2)) / (n1 + n2 - 2))
      cohen_d <- (mean(col1) - mean(col2)) / pooled_sd
      
      # Add results to dataframe if p-value is less than 0.05
      if (t_test_result$p.value < 0.05) {
        result <- data.frame(Comparison = paste(column_names[i], "-", column_names[j]),
                             EffectSize = cohen_d)
        results <- rbind(results, result)
      }
    }
  }
  
  # Plot the effect sizes
  ggplot(results, aes(x = Comparison, y = EffectSize)) +
    geom_bar(stat = "identity", fill = "steelblue", width = 0.5) +
    labs(x = "Comparison", y = "Effect Size (Cohen's d)", title = "Effect Size Comparison") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  #############################################################################################
  
  column_names <- c("Mean\nVelocity", "Mean\nLateral\nAcceleration", "Mean\nFixation\nPoints")
  effect_sizes <- c(1.782386, 1.109982,  1.219223  )
  
  # Create a data frame
  df <- data.frame(Column = column_names, EffectSize = effect_sizes)
  
  set.seed(50)  # Set seed for reproducibility
  num_categories <- nrow(df)
  random_colors <- sample(colors(), num_categories)
  # Plot the effect sizes
  ggplot(df, aes(x = Column, y = EffectSize)) +
    geom_bar(stat = "identity", width = 0.5, fill=random_colors) +
    labs(x = "", y = "Effect Size (Cohen's d)", title = "Effect Size Comparison") +
    theme(axis.text.x = element_text(family = "Arial",face="bold", size=12),
          axis.text.y = element_text(family = "Arial", face = "bold",size=13),
          plot.title = element_text(family = "Arial", face = "bold"))+
    scale_y_continuous(limits = c(0, 2, breaks = seq(0, 2, by = 0.50)))
    theme_minimal() 
    #theme(axis.text.x = element_text(angle = 45, hjust = 1))
  ############################################################################################
    
    effect_sizes <- c(1.782386, 1.109982, 1.219223, 1.036948)
    standard_errors <- c(0.2, 0.15, 0.18, 0.12)
    
    # Create a data frame
    df <- data.frame(EffectSize = effect_sizes, StandardError = standard_errors)
    
    # Calculate the limits for the funnel plot
    upper_limit <- df$EffectSize + 1.96 * df$StandardError
    lower_limit <- df$EffectSize - 1.96 * df$StandardError
    
    # Create the funnel plot
    ggplot(df, aes(x = EffectSize, y = seq_along(EffectSize))) +
      geom_point() +
      geom_errorbarh(aes(xmax = upper_limit, xmin = lower_limit), height = 0.5) +
      labs(x = "Effect Size (Cohen's d)", y = "Study") +
      theme_minimal()
    
    #####################################################################################
    
    
    data[29 : 30] <- as.data.frame(scale(data[29 : 30]))
data$Response29   
data$Response30
###########################################################################
cor.test(data$Response10, data$Response29)
#####################################################################
ggplot(data, aes(x = log(data$Response10), y = log(data$Response6))) +
  geom_point(aes(color = factor(data$Response25))) +
  stat_smooth(method = "lm",
              col = "#C42126", se = FALSE, size = 1
  )

plot(x = log(data$Response10), y = log(data$Response6),
     xlab = "RB_Fixation Point",
     ylab = "R_Fixation Point",
     main = "Weight vs Milage"
)
################################################################################################
subset_data <- data[, 2:13]

# Create a correlation matrix
cor_matrix <- cor(subset_data)

# Customize and plot the correlation matrix
par(bg = "Orange")
corrplot(cor_matrix, method = "color", type = "upper",
         tl.col = "black", tl.srt = 45, bg="black" ,diag = FALSE)




mean_ratings <- aggregate(Response28 ~ Response24, data = data, FUN = mean)

# Rename the columns
colnames(mean_ratings) <- c("Driving Experience", "Mean Distraction Rating")

# Print the table
print(mean_ratings)


mean_ratings <- aggregate(Response28 ~ Response24, data = data, FUN = mean)

# Plot the mean distraction rating as a bar plot
ggplot(mean_ratings, aes(x = Response24, y = Response28)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(x = "Driving Experience", y = "Mean Distraction Rating") +
  ggtitle("Mean Distraction Rating by Driving Experience")


corr.test(data$Response28,data$Response16)
ggplot(data = data, aes(x = Response8, y = Response6)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Saccade Count On Road(Text)", y = "Fixation Point on Road(Text)", title = "Correlation Plot")

####################################################################################################

mean_data1 <- mean(data$Response20)
mean_data2 <- mean(data$Response21)


df <- data.frame(Condition = c("Base Condition", "During Texting"), MeanCount = c(mean_data1, mean_data2))
ggplot(df, aes(x = Condition, y = MeanCount, fill = Condition)) +
  geom_bar(stat = "identity", width = 0.5) +
  labs(x = "", y = "Mean Longitudinal Acceleration", title = "Mean Values Bar Plot") +
  theme(axis.text.x = element_text(family = "Arial",face="bold", size=12),
        axis.text.y = element_text(family = "Arial", face = "bold",size=13),
        plot.title = element_text(family = "Arial", face = "bold"))
  scale_y_continuous(limits = c(0, ), breaks = seq(0, 450, by = 50))
theme_minimal()###
#########################################################
library(ggplot2)

mean_data1 <- mean(data$Response14)
mean_data2 <- mean(data$Response15)
sem_data1 <- sd(data$Response14) / sqrt(length(data$Response14))
sem_data2 <- sd(data$Response15) / sqrt(length(data$Response15))

df <- data.frame(Condition = c("Base Condition", "During Texting"), MeanCount = c(mean_data1, mean_data2))

ggplot(df, aes(x = Condition, y = MeanCount, fill = Condition)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_errorbar(aes(ymin = MeanCount - sem_data1, ymax = MeanCount + sem_data1),
                width = 0.2, color = "black",position="dodge") +
  geom_errorbar(aes(ymin = MeanCount - sem_data2, ymax = MeanCount + sem_data2),
                width = 0.2, color = "black",position="dodge") +
  labs(x = "", y = "Mean Velocity (in kmph)", title = "Mean Values Bar Plot") +
  theme(axis.text.x = element_text(family = "Arial", face = "bold", size = 12),
        axis.text.y = element_text(family = "Arial", face = "bold", size = 13),
        plot.title = element_text(family = "Arial", face = "bold")) +
  #scale_y_continuous(limits = c(0, 450), breaks = seq(0, 450, by = 50)) 
  theme_minimal()
###########################################################################
library(ggplot2)

mean_data1 <- mean(data$Response12)
mean_data2 <- mean(data$Response8)
sem_data1 <- sd(data$Response12) / sqrt(length(data$Response12))
sem_data2 <- sd(data$Response8) / sqrt(length(data$Response8))

df <- data.frame(Condition = c("Base Condition", "During Texting"), MeanCount = c(mean_data1, mean_data2))

ggplot(df, aes(x = Condition, y = MeanCount, fill = Condition)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_errorbar(aes(ymin = MeanCount - sem_data1, ymax = MeanCount + sem_data1),
                width = 0.2, color = "black", position="dodge") +
  geom_errorbar(aes(ymin = MeanCount - sem_data2, ymax = MeanCount + sem_data2),
                width = 0.2, color = "black",position="dodge") +
  labs(x = "", y = "Mean Saccade Counts", title = "Mean Values Bar Plot") +
  theme(axis.text.x = element_text(family = "Arial", face = "bold", size = 12),
        axis.text.y = element_text(family = "Arial", face = "bold", size = 13),
        plot.title = element_text(family = "Arial", face = "bold")) +
  scale_y_continuous(limits = c(0, 450), breaks = seq(0, 450, by = 50)) +
  theme_minimal()
##########################################################################
library(ggplot2)

mean_data1 <- mean(data$Response17)
mean_data2 <- mean(data$Response18)
sem_data1 <- sd(data$Response17) / sqrt(length(data$Response17))
sem_data2 <- sd(data$Response18) / sqrt(length(data$Response18))

df <- data.frame(Condition = c("Base Condition", "During Texting"), MeanCount = c(mean_data1, mean_data2))

ggplot(df, aes(x = Condition, y = MeanCount, fill = Condition)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_errorbar(aes(ymin = MeanCount - sem_data1, ymax = MeanCount + sem_data1),
                width = 0.2, color = "black",position="dodge") +
  geom_errorbar(aes(ymin = MeanCount - sem_data2, ymax = MeanCount + sem_data2),
                width = 0.2, color = "black",position="dodge") +
  labs(x = "", y = "Mean Lateral Acceleration", title = "Mean Values Bar Plot") +
  theme(axis.text.x = element_text(family = "Arial", face = "bold", size = 12),
        axis.text.y = element_text(family = "Arial", face = "bold", size = 13),
        plot.title = element_text(family = "Arial", face = "bold")) +
  #scale_y_continuous(limits = c(0, 450), breaks = seq(0, 450, by = 50)) 
  theme_minimal()
###########################################################################
library(ggplot2)

mean_data1 <- mean(data$Response17)
mean_data2 <- mean(data$Response18)
sem_data1 <- sd(data$Response17) / sqrt(length(data$Response17))
sem_data2 <- sd(data$Response18) / sqrt(length(data$Response18))

df <- data.frame(Condition = c("Base Condition", "During Texting"), MeanCount = c(mean_data1, mean_data2))

ggplot(df, aes(x = Condition, y = MeanCount, fill = Condition)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_errorbar(aes(ymin = MeanCount - sem_data1, ymax = MeanCount + sem_data1),
                width = 0.2, color = "black",position="dodge") +
  geom_errorbar(aes(ymin = MeanCount - sem_data2, ymax = MeanCount + sem_data2),
                width = 0.2, color = "black",position="dodge") +
  labs(x = "", y = "Mean Lateral Acceleration", title = "Mean Values Bar Plot") +
  theme(axis.text.x = element_text(family = "Arial", face = "bold", size = 12),
        axis.text.y = element_text(family = "Arial", face = "bold", size = 13),
        plot.title = element_text(family = "Arial", face = "bold")) +
  #scale_y_continuous(limits = c(0, 450), breaks = seq(0, 450, by = 50)) 
  theme_minimal()
#################################################################################
library(ggplot2)

median_data1 <- median(data$Response10)
median_data2 <- median(data$Response6)
sem_data1 <- sd(data$Response10) / sqrt(length(data$Response10))
sem_data2 <- sd(data$Response6) / sqrt(length(data$Response6))

df <- data.frame(Condition = c("Base Condition", "During Texting"), MedianCount = c(median_data1, median_data2))

ggplot(df, aes(x = Condition, y = MedianCount, fill = Condition)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_errorbar(aes(ymin = MedianCount - sem_data1, ymax = MedianCount + sem_data1),
                width = 0.2, color = "black", position = "dodge") +
  geom_errorbar(aes(ymin = MedianCount - sem_data2, ymax = MedianCount + sem_data2),
                width = 0.2, color = "black", position = "dodge") +
  labs(x = "", y = "Median Fixation Counts", title = "Median Values Bar Plot") +
  theme(axis.text.x = element_text(family = "Arial", face = "bold", size = 12),
        axis.text.y = element_text(family = "Arial", face = "bold", size = 13),
        plot.title = element_text(family = "Arial", face = "bold")) +
  theme_minimal()
#########################################################################
library(ggplot2)

df <- data.frame(Condition = c("Base Condition", "During Texting"),
                 Response = c(data$Response14, data$Response15))

ggplot(df, aes(x = Condition, y = Response, fill = Condition)) +
  geom_boxplot() +
  labs(x = "", y = "Velocity (in kmph)", title = "Box Plot of Fixation Counts") +
  theme(axis.text.x = element_text(family = "Arial", face = "bold", size = 12),
        axis.text.y = element_text(family = "Arial", face = "bold", size = 13),
        plot.title = element_text(family = "Arial", face = "bold")) +
  theme_minimal()
#################################################################

library(ggplot2)

mean_data1 <- mean(data$Response11)
mean_data2 <- mean(data$Response7)
sem_data1 <- sd(data$Response11) / sqrt(length(data$Response11))
sem_data2 <- sd(data$Response7) / sqrt(length(data$Response7))

df <- data.frame(Condition = c("Base Condition", "During Texting"), MeanCount = c(mean_data1, mean_data2))

ggplot(df, aes(x = Condition, y = MeanCount, fill = Condition)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_errorbar(aes(ymin = MeanCount - sem_data1, ymax = MeanCount + sem_data1),
                width = 0.2, color = "black",position="dodge") +
  geom_errorbar(aes(ymin = MeanCount - sem_data2, ymax = MeanCount + sem_data2),
                width = 0.2, color = "black",position="dodge") +
  labs(x = "", y = "Mean Fixation Duration", title = "Mean Values Bar Plot") +
  theme(axis.text.x = element_text(family = "Arial", face = "bold", size = 12),
        axis.text.y = element_text(family = "Arial", face = "bold", size = 13),
        plot.title = element_text(family = "Arial", face = "bold")) +
  scale_y_continuous(limits = c(0, 160), breaks = seq(0, 160, by = 40)) 
  theme_minimal()
  ##########################################################
  t.test(data$Response11, data$Response7, pair=TRUE)
  #####################################################################
  
  ######################################################################
  library(ggplot2)
  
  # Subset the data and calculate the mean velocity for each group
  mean_velocity <- data %>%
    filter(Response27 %in% c("Less than 25", "More than 25")) %>%
    group_by(Response27) %>%
    summarise(
      MeanVelocity_11 = mean(Response11),
      MeanVelocity_7 = mean(Response7)
    )
  
  # Reshape the data to long format and rename the x-axis labels
  df_long <- mean_velocity %>%
    pivot_longer(cols = starts_with("MeanVelocity"), names_to = "Condition", values_to = "MeanVelocity") %>%
    mutate(Condition = ifelse(Condition == "MeanVelocity_11", "Base Condition", "Texting Condition"))
  
  # Plot the grouped bar plot with custom labels
  ggplot(df_long, aes(x = Condition, y = MeanVelocity, fill = Response27)) +
    geom_bar(stat = "identity", position = position_dodge(), width = 0.6) +
    xlab("Condition") +
    ylab("Mean Fixation Duration (in ms)") +
    ggtitle("Mean Values Bar Plot") +
    labs(fill = "Age")+
    theme(
      axis.text.x = element_text(family = "Arial", face = "bold", size = 10),
      axis.text.y = element_text(family = "Arial", face = "bold", size = 13),
      plot.title = element_text(family = "Arial", face = "bold"),
      legend.position = "top"
    )
  ##########################################################################
  library(ggplot2)
  
  # Subset the data and calculate the mean velocity for each group
  mean_velocity <- data %>%
    filter(Response24 %in% c("Less than 2 years", "2 to 5 years", "More than 5 years")) %>%
    group_by(Response24) %>%
    summarise(
      MeanVelocity_11 = mean(Response11),
      MeanVelocity_7 = mean(Response7)
    )
  
  # Reshape the data to long format and rename the x-axis labels
  df_long <- mean_velocity %>%
    pivot_longer(cols = starts_with("MeanVelocity"), names_to = "Condition", values_to = "MeanVelocity") %>%
    mutate(Condition = ifelse(Condition == "MeanVelocity_11", "Base Condition", "Texting Condition"),
           Response24 = ifelse(Response24 == "Less than 2 years", " Less than 2 years",
                               ifelse(Response24 == "2 to 5 years", " 2 to 5 years", "More than 5 years")))
  
  # Plot the grouped bar plot with custom labels
  ggplot(df_long, aes(x = Condition, y = MeanVelocity, fill = Response24)) +
    geom_bar(stat = "identity", position = position_dodge(), width = 0.6) +
    xlab("Condition") +
    ylab("Mean Fixation Duration (in ms)") +
    ggtitle("Mean Values Bar Plot") +
    labs(fill = "Driving Experience") +
    theme(
      axis.text.x = element_text(family = "Arial", face = "bold", size = 10),
      axis.text.y = element_text(family = "Arial", face = "bold", size = 13),
      plot.title = element_text(family = "Arial", face = "bold"),
      legend.position = "top"
    )
  ################################################################
  group_A_data <- subset(data, data$Response27 == "Less than 25", select = c("Response27", "Response11", "Response7"))
  group_B_data <- subset(data, data$Response27 == "More than 25", select = c("Response27", "Response11", "Response7"))
  
  # Define the column names for distraction ratings
  rating_columns <- c( "Response11", "Response7")
  
  # Create an empty list to store the results
  results_list <- list()
  
  # Iterate over each column and perform the Wilcoxon signed-rank test for paired samples
  for (column in rating_columns) {
    # Subset the data for the specific column and driving experience group
    group_A_ratings <- group_A_data[[column]]
    group_B_ratings <- group_B_data[[column]]
   
    
    # Identify the smallest sample size among the driving experience groups
    n <- min(length(group_A_ratings), length(group_B_ratings)) #length(group_C_ratings), length(group_D_ratings))
    
    # Perform the Wilcoxon signed-rank test with aligned pairs for each driving experience group
    result_AB <- t.test(group_A_ratings[1:n], group_B_ratings[1:n])
 
    
    # Create a unique key for the result list
    result_key <- column
    
    # Store the results in the list
    results_list[[result_key]] <- list(result_AB)# result_AC, result_AD, result_BC, result_BD, result_CD)
  }
  
  # Access the test results for each column
  for (key in names(results_list)) {
    result <- results_list[[key]]
    
    cat("Column:", key, "\n")
    cat("Comparison AB - t-test p-value:", result[[1]]$p.value, "\n")
    cat("\n")
  }
  #############################################################################
  group_A_data <- subset(data, data$Response24 == "2 to 5 years5", select = c("Response24", "Response14", "Response15"))
  group_B_data <- subset(data, data$Response24 == "Less than 2 years", select = c("Response24", "Response14", "Response15"))
  group_C_data <- subset(data, data$Response24 == "More than 5 years", select = c("Response24", "Response14", "Response15"))
  #group_D_data <- subset(data, data$r121 == "No Experience", select = c("r121", "r24", "r25", "r26", "r27"))
  
  # Define the column names for distraction ratings
  rating_columns <- c( "Response14", "Response15")
  
  # Create an empty list to store the results
  results_list <- list()
  
  # Iterate over each column and perform the Wilcoxon signed-rank test for paired samples
  for (column in rating_columns) {
    # Subset the data for the specific column and driving experience group
    group_A_ratings <- group_A_data[[column]]
    group_B_ratings <- group_B_data[[column]]
    group_C_ratings <- group_C_data[[column]]
    #group_D_ratings <- group_D_data[[column]]
    
    # Identify the smallest sample size among the driving experience groups
    n <- min(length(group_A_ratings), length(group_B_ratings),length(group_C_ratings)) #length(group_D_ratings))
    
    # Perform the Wilcoxon signed-rank test with aligned pairs for each driving experience group
    result_AB <- t.test(group_A_ratings[1:n], group_B_ratings[1:n])
    result_AC <- t.test(group_A_ratings[1:n], group_C_ratings[1:n])
    result_BC <- t.test(group_B_ratings[1:n], group_C_ratings[1:n])
    
    # Create a unique key for the result list
    result_key <- column
    
    # Store the results in the list
    results_list[[result_key]] <- list(result_AB, result_AC, result_BC) #result_BC, result_BD, result_CD)
  }
  
  # Access the test results for each column
  for (key in names(results_list)) {
    result <- results_list[[key]]
    
    cat("Column:", key, "\n")
    cat("Comparison AB - t-test p-value:", result[[1]]$p.value, "\n")
    cat("Comparison AC - Wilcoxon p-value:", result[[2]]$p.value, "\n")
    cat("Comparison BC - Wilcoxon p-value:", result[[3]]$p.value, "\n")
    cat("\n")
  }
  ########################################################################
  group_a_data <- subset(data, Response27 == "Less than 25")
  group_b_data <- subset(data, Response27 == "More than 25")
  
  # Check variance for 'Response15' in each group
  variance_group_a <- var(group_a_data$Response15)
  variance_group_b <- var(group_b_data$Response15)
  
  # Print the variances
  print(paste("Variance of Group A:", variance_group_a))
  print(paste("Variance of Group B:", variance_group_b))
  
  ################################################################
  levene_result <- leveneTest(Response15 ~ Response27, data = data)
  
  # View the results
  print(levene_result)
  
  ##################################################################
  result_var_test <- var.test(data$Response10, data$Response6)
  
  # Print the result
  print(result_var_test)
  #######################################################################
  result_fligner_test <- fligner.test(data$R14, data$R15)
  
  # Print the result
  print(result_fligner_test)
  ########################################################################
  result_t_test <- t.test(data$Response20, data$Response21, var.equal = FALSE)
  
  # Print the result
  print(result_t_test)
  ######################################################################
  subset_data <- data %>%
    filter(Response24 %in% c("2 to 5 years", "Less than 2 years","More than 5 years"))
  
  # Visualize histograms and Q-Q plots for each group
  ggplot(subset_data, aes(x = Response20)) +
    geom_histogram() +
    facet_wrap(~Response27) +
    ggtitle("Histogram") +
    theme_minimal()
  
  
  ggplot(subset_data, aes(x = Response18)) +
    geom_histogram() +
    facet_wrap(~Response27) +
    ggtitle("Histogram") +
    theme_minimal()
  
  
  
  # Perform normality tests for each group
  shapiro_test <- subset_data %>%
    group_by(Response24) %>%
    summarise(
      Shapiro_Test_Response11 = shapiro.test(Response11)$p.value,
      Shapiro_Test_Response7 = shapiro.test(Response7)$p.value
    )
  
  # Print the p-values for normality tests
  print(shapiro_test)
  ######################################
  library(car)
  
  # Subset the data for the two numerical categories and driving experience groups
  subset_data <- data %>%
    filter(Response27 %in% c("Less than 25", "More than 25"))
  
  # Q-Q plot for Response14
  qqPlot(subset_data$Response11, main = "Q-Q Plot of Fixation Duration(Base)")
  
  # Q-Q plot for Response15
  qqPlot(subset_data$Response7, main = "Q-Q Plot of Fixation Duration(Texting)")
  #############################################################################
  variance_summary <- data %>%
    group_by(Response27) %>%
    summarise(
      Variance_Response14 = var(Response14),
      Variance_Response15 = var(Response15)
    )
  
  # Print the variance values for each group within Response27
  print(variance_summary)
  ##################################################
  group_A_data <- subset(data, Response24 == "2 to 5 years", select = c("Response24", "Response11", "Response7"))
  group_B_data <- subset(data, Response24 == "Less than 2 years", select = c("Response24", "Response11", "Response7"))
  group_C_data <- subset(data, Response24 == "More than 5 years", select = c("Response24", "Response11", "Response7"))
  
  # Define the column names for responses
  response_columns <- c("Response11", "Response7")
  
  # Create an empty list to store the results
  results_list <- list()
  
  # Iterate over each column and perform the Mann-Whitney U test for independent samples
  for (column in response_columns) {
    # Extract the responses for each driving experience group
    group_A_responses <- group_A_data[[column]]
    group_B_responses <- group_B_data[[column]]
    group_C_responses <- group_C_data[[column]]
    
    # Perform the Mann-Whitney U test for each pair of groups
    result_AB <- wilcox.test(group_A_responses, group_B_responses)
    result_AC <- wilcox.test(group_A_responses, group_C_responses)
    result_BC <- wilcox.test(group_B_responses, group_C_responses)
    
    # Create a unique key for the result list
    result_key <- column
    
    # Store the results in the list
    results_list[[result_key]] <- list(result_AB, result_AC, result_BC)
  }
  
  # Access the test results for each column
  for (key in names(results_list)) {
    result <- results_list[[key]]
    
    cat("Column:", key, "\n")
    cat("Comparison AB -  wilcox p-value:", result[[1]]$p.value, "\n")
    cat("Comparison AC -  wilcox p-value:", result[[2]]$p.value, "\n")
    cat("Comparison BC -  wilcox p-value:", result[[3]]$p.value, "\n")
    cat("\n")
  }
  
  #################################################################################3
  group_more_than_25 <- subset(data, Response27 == "More than 25")
  group_less_than_25 <- subset(data, Response27 == "Less than 25")
  
  # Perform a paired t-test
  paired_ttest_result_more25 <- t.test(group_more_than_25$Response10, group_more_than_25$Response6, paired = TRUE)
  paired_ttest_result <- t.test(group_less_than_25$Response10, group_less_than_25$Response6, paired = TRUE)
  # Perform a Wilcoxon signed-rank test (non-parametric alternative for paired data)
  #wilcoxon_test_result_more25 <- wilcox.test(group_more_than_25$Response14, group_more_than_25$Response15, paired = TRUE)
  
  # Print the results
  cat("Paired t-test p-value for More than 25:", paired_ttest_result_more25$p.value, "\n")
  cat("Paired t-test p-value for less than 25:", paired_ttest_result$p.value, "\n")
  
#################################################################
  library(lsr)
  
  # Subset the data for the "More than 25" and "Less than 25" groups
  group_more_than_25 <- subset(data, Response27 == "More than 25")
  group_less_than_25 <- subset(data, Response27 == "Less than 25")
  group_more_than_25$Response17
  
  # Perform a paired t-test
  paired_ttest_result_more25 <- t.test(group_more_than_25$Response17, group_more_than_25$Response18, paired = TRUE)
  paired_ttest_result_less25 <- t.test(group_less_than_25$Response17, group_less_than_25$Response18, paired = TRUE)
  
  # Calculate the effect size (Cohen's d) for paired t-tests
  effect_size_more25 <- cohensD(group_more_than_25$Response17, group_more_than_25$Response18)
  effect_size_less25 <- cohensD(group_less_than_25$Response17, group_less_than_25$Response18)
  
  # Print the results
  cat("Paired t-test p-value for More than 25:", paired_ttest_result_more25$p.value, "\n")
  cat("Effect size (Cohen's d) for More than 25:", effect_size_more25, "\n")
  
  cat("Paired t-test p-value for less than 25:", paired_ttest_result_less25$p.value, "\n")
  cat("Effect size (Cohen's d) for less than 25:", effect_size_less25, "\n")
  
  ###############################################################################
  library(lsr)
  

  A <- subset(data, Response24 == "2 to 5 years")
  B <- subset(data, Response24 == "Less than 2 years")
  C <- subset(data, Response24 == "More than 5 years")
  
  
  
  
  paired_ttest_A <- t.test(A$Response20, A$Response21, paired = TRUE)
  paired_ttest_B <- t.test(B$Response20, B$Response21, paired = TRUE)
  paired_ttest_C <- t.test(C$Response20, C$Response21, paired = TRUE)
 
  
  effect_size_A <- cohensD(A$Response20, A$Response18)
  effect_size_B <- cohensD(B$Response17, B$Response18)
  effect_size_C <- cohensD(C$Response17, C$Response18)
  # Print the results
  cat("Paired t-test p-value for A:", paired_ttest_A$p.value, "\n")
  cat("Effect size (Cohen's d) for A :", effect_size_A, "\n")
  
  cat("Paired t-test p-value for B:", paired_ttest_B$p.value, "\n")
  cat("Effect size (Cohen's d) for B :", effect_size_B, "\n")
  
  cat("Paired t-test p-value for C:", paired_ttest_C$p.value, "\n")
  cat("Effect size (Cohen's d) for C :", effect_size_C, "\n")
 