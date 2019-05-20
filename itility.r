# Can you analyse all the measurement data and give advice to the customer for which virtual servers they can lower their CPU? (number of servers is oversized)
# Please describe the advice you would be giving to the customer, including the steps you have taken to get to the advice (please include which data you have included and why)
# Do a general check on the data from the file and outline where you see strange activity (and why)
# Answer the following question: What is needed extra to perform a similar analysis using machine learning?

library(tidyverse)
library(ggcorrplot)
library(psych)
library(qwraps2)
library(knitr)
library(rmarkdown)

raw_data <- read_delim(list.files()[19], ";")
raw_data$time <- as.Date(raw_data$time, "%m-%d-%Y")
working_df <- raw_data %>% mutate(row_id = row_number())

# Split into two temp DFs, data is not tidy and discrete CPU/Memory usage vars need to be created

memory_vals <- working_df %>%
  filter(MetricId == "MemActive") %>%
  mutate(row_id = row_id + 1)

cpu_vals <- working_df %>%
  filter(MetricId == "CpuUsageMHz")

# Divide usage vals by 1M for less memory-intensive analysis and join widened dataset together

wide_df <- memory_vals %>%
  full_join(cpu_vals, by = c("row_id")) %>%
  select(1:6, 8, 15:17, 20:21) %>%
  mutate(MaxValue.x = MaxValue.x/1000000,
         AvgValue.x = AvgValue.x/1000000,
         MinValue.x = MinValue.x/1000000,
         MaxValue.y = MaxValue.y/1000000,
         AvgValue.y = AvgValue.y/1000000,
         MinValue.y = MinValue.y/1000000) %>%
  mutate(MemUsageRange = MaxValue.x - MinValue.x, CpuUsageRange = MaxValue.y - MinValue.y)
  
names <- c("VMName", "Cluster", "Date", "AvgMemUsage", "MinMemUsage", "MaxMemUsage", "MemoryMB", "AvgCpuUsage", "MinCpuUsage", "MaxCpuUsage", "CpuMHz", "NumCpu", "MemUsageRange", "CpuUsageRange")
names(wide_df) <- names

summary_stats <-
  list("Average Memory Usage" =
         list("min" = ~ min(.data$AvgMemUsage),
              "max" = ~ max(.data$AvgMemUsage),
              "median" = ~ median(.data$AvgMemUsage),
              "mean (sd)" = ~ qwraps2::mean_sd(.data$AvgMemUsage)),
       "Average CPU Usage" =
         list("min" = ~ min(.data$AvgCpuUsage),
              "max" = ~ max(.data$AvgCpuUsage),
              "median" = ~ median(.data$AvgCpuUsage),
              "mean (sd)" = ~ qwraps2::mean_sd(.data$AvgCpuUsage)),
       "Memory Usage Range" =
         list("min" = ~ min(.data$MemUsageRange),
              "max" = ~ max(.data$MemUsageRange),
              "mean (sd)" = ~ qwraps2::mean_sd(.data$MemUsageRange)),
       "CPU Usage Range" =
         list("min" = ~ min(.data$CpuUsageRange),
              "max" = ~ max(.data$CpuUsageRange),
              "mean (sd)" = ~ qwraps2::mean_sd(.data$CpuUsageRange)),
       "Observation Count & Percent of Total by Cluster" =
         list("1" = ~ qwraps2::n_perc0(.data$Cluster == 1),
              "2"  = ~ qwraps2::n_perc0(.data$Cluster == 2),
              "3"  = ~ qwraps2::n_perc0(.data$Cluster == 3),
              "4"  = ~ qwraps2::n_perc0(.data$Cluster == 4),
              "5"  = ~ qwraps2::n_perc0(.data$Cluster == 5))
    )

options(qwraps2_markup = "markdown")
summary_table(wide_df, summary_stats)

# Compute summary dataframe grouped by VM

summary_df <- wide_df %>%
  group_by(VMName) %>%
  summarize(ObservationCount = n(),
            MedianAvgVMCpu = median(AvgCpuUsage), 
            MedianAvgVmMem = median(AvgMemUsage), 
            MedianCpuUsageRange = median(CpuUsageRange),
            MedianMemUsageRange = median(MemUsageRange),
            CpuMHz = max(CpuMHz),
            NumCpu = max(NumCpu),
            Cluster = max(Cluster))

# Get descriptive statistics and save them to a data frame, check record count by VM

descriptive_stat_summary <- describe(summary_df[,2:6], ranges = FALSE, quant = c(.25,.75), IQR = TRUE)
summary_table(descriptive_stat_summary, summary_stats_grouped)
summary_stats_grouped <-
  list("Observations by VM" =
         list("mean" = ~ (mean[1]),
              "sd" = ~ (sd[1]),
              "se" = ~ (se[1]),
              "IQR" = ~ (IQR[1]),
              "Q0.25" = ~ (Q0.25[1]),
              "Q0.75" = ~ (Q0.75)[1]),
       "Average of Median CPU Usage Grouped by VM" =
         list("mean" = ~ (mean[2]),
              "sd" = ~ (sd[2]),
              "se" = ~ (se[2]),
              "IQR" = ~ (IQR[2]),
              "Q0.25" = ~ (Q0.25[2]),
              "Q0.75" = ~ (Q0.75)[2]),
       "Average of Median Memory Usage/Activity Grouped by VM" =
         list("mean" = ~ (mean[3]),
              "sd" = ~ (sd[3]),
              "se" = ~ (se[3]),
              "IQR" = ~ (IQR[3]),
              "Q0.25" = ~ (Q0.25[3]),
              "Q0.75" = ~ (Q0.75)[3])
  )

summary_df %>%
  ggplot(aes(x = ObservationCount)) +
    geom_histogram(binwidth = 1, color = "white", alpha = 0.7, fill = "red") +
    geom_vline(xintercept = 51.7, color = "blue", linetype = "longdash") +
    ggtitle("Distribution of Record Count by VM") +
    xlab("Record Count") +
    ylab("VM Count")

# Compute correlation matrix on continuous data

corr <- cor(summary_df[,2:6])
ggcorrplot(corr, hc.order = TRUE, outline.color = "white", lab = TRUE)


#Memory usage distribution by server

wide_df %>%
  ggplot(aes(x = AvgMemUsage)) +
    geom_histogram(color = "white", alpha = 0.7, fill = "blue") + 
    ggtitle("Average Memory Usage Distribution by Observation Count") +
    geom_vline(xintercept = 338.67, linetype = "dashed", color = "red", size = 2) +
    xlab("Average memory usage (x1000000)") +
    ylab("Observation Count")

#CPU usage distribution by observation

wide_df %>%
  ggplot(aes(x = AvgCpuUsage)) +
    geom_histogram(color = "white", alpha = 0.7, fill = "red") +
    ggtitle("Average CPU Usage Distribution by Observation Count") +
    geom_vline(xintercept = 364062326.14, linetype = "dashed", color = "blue", size = 2) +
    xlab("Average CPU usage (x1000000)") +
    ylab("Observation Count")

# CPU usage distribution by server

summary_df %>%
  ggplot(aes(MedianAvgVMCpu)) +
    geom_histogram(color = "white", alpha = 0.5, fill = "red") +
    ggtitle("Average CPU Usage Distribution by Server Count") +
    xlab("Median Average CPU Usage by VM (x1000000)") +
    ylab("Server Count")

# Plot CPU usage by number of cores

summary_df$NumCpu <- as.factor(summary_df$NumCpu)
mean_MedianAvgVMCpu <- mean(summary_df$MedianAvgVMCpu)
sd_MedianAvgVMCpu <- sd(summary_df$MedianAvgVMCpu)

summary_df %>%
  filter(!is.na(NumCpu)) %>%
  ggplot(aes(x=NumCpu, y=MedianAvgVMCpu, color = NumCpu)) +
    geom_boxplot(outlier.color = "red") +
    stat_summary(fun.y=mean, geom="point", shape=23, size = 4) +
    geom_hline(yintercept = 374449402.2, linetype = "dashed", color = "blue") +
    geom_hline(yintercept = 208114707.8, color = "blue") +
    geom_hline(yintercept = 508023370.4, color = "blue") +
    ggtitle("Plot of CPU Usage by Number of CPU Cores") +
    xlab("Number of CPU Cores") +
    ylab("CPU Usage (x1000000)")

# Plot CPU Usage by Cluster

summary_df$Cluster <- as.factor(summary_df$Cluster)

summary_df %>%
  filter(!is.na(Cluster)) %>%
  ggplot(aes(x=Cluster, y=MedianAvgVMCpu, color = Cluster)) +
    geom_boxplot(outlier.color = "red") +
    stat_summary(fun.y=mean, geom="point", shape=23, size = 3) +
    geom_hline(yintercept = 374449402.2, linetype = "dashed", color = "blue") +
    geom_hline(yintercept = 208114707.8, color = "blue") +
    geom_hline(yintercept = 508023370.4, color = "blue") +
    ggtitle("Plot of CPU Usage by Cluster", subtitle =  "Grouped by VM Average Median") +
    xlab("Cluster") +
    ylab("CPU Usage (x1000000)")

# Identify VMs < 0.25Q for CPU usage & VMs with low record/observation count

underutilized_VMs <- summary_df %>%
  filter(MedianAvgVMCpu < descriptive_stat_summary$Q0.25 & NumCpu > 1)

few_observation_VMs <- summary_df %>%
  filter(ObservationCount < descriptive_stat_summary$Q0.25[1])

servercount_by_cluster <- summary_df %>%
  group_by(Cluster) %>%
  summarize(ServerCount = n())

underutilized_VMs_by_cluster <- underutilized_VMs %>%
  group_by(Cluster) %>%
  summarize(ServerCount = n())

# Log model for predicting CPU utilization

wide_df_model <- wide_df %>%
  mutate(BelowQ1AvgCpuUsage = as.numeric(AvgCpuUsage < 168236689))

# Check class bias
table(wide_df_model$BelowQ1AvgCpuUsage)

# Training Data
input_ones <- wide_df_model[which(wide_df_model$BelowQ1AvgCpuUsage == 1), ]
input_zeros <- wide_df_model[which(wide_df_model$BelowQ1AvgCpuUsage == 0), ]
set.seed(100)
input_ones_training_rows <- sample(1:nrow(input_ones), 0.8*nrow(input_ones))
input_zeros_training_rows <- sample(1:nrow(input_zeros), 0.8*nrow(input_ones)) 
training_ones <- input_ones[input_ones_training_rows, ]  
training_zeros <- input_zeros[input_zeros_training_rows, ]
trainingData <- rbind(training_ones, training_zeros)

# Create Test Data
test_ones <- input_ones[-input_ones_training_rows, ]
test_zeros <- input_zeros[-input_zeros_training_rows, ]
testData <- rbind(test_ones, test_zeros)

# WOE test
wide_df$Cluster <- as.factor(wide_df$Cluster)
wide_df$NumCpu <- as.factor(wide_df$NumCpu)
factor_vars <- c("Cluster", "NumCpu")

# Use InformationValue package to calculate predicive value of categorical vars in the data set
all_iv <- data.frame(VARS=factor_vars, IV=numeric(length(factor_vars)), STRENGTH=character(length(factor_vars)), stringsAsFactors = F)
for (factor_var in factor_vars){
  all_iv[all_iv$VARS == factor_var, "IV"] <- InformationValue::IV(X=wide_df_model[, factor_var], Y=wide_df_model$BelowQ1AvgCpuUsage)
  all_iv[all_iv$VARS == factor_var, "STRENGTH"] <- attr(InformationValue::IV(X=wide_df_model[, factor_var], Y=wide_df_model$BelowQ1AvgCpuUsage), "howgood")
}

# Compute information values for remaining continuous vars
continuous_vars <- c("AvgMemUsage", "MinMemUsage", "MaxMemUsage", "AvgCpuUsage", "MinCpuUsage", "MaxCpuUsage", "MemUsageRange", "CpuUsageRange", "MemoryMB", "CpuMHz")

iv_df <- data.frame(VARS=c(factor_vars, continuous_vars), IV=numeric(12))  # init for IV results

# Recompute IV for factors
for(factor_var in factor_vars){
  smb <- smbinning.factor(trainingData, y="BelowQ1AvgCpuUsage", x=factor_var)  # WOE table
  if(class(smb) != "character"){  # any error while calculating scores.
    iv_df[iv_df$VARS == factor_var, "IV"] <- smb$iv
  }
}

for(continuous_var in continuous_vars){
  smb <- smbinning(trainingData, y="BelowQ1AvgCpuUsage", x=continuous_var)  # WOE table
  if(class(smb) != "character"){  # any error while calculating scores.
    iv_df[iv_df$VARS == continuous_var, "IV"] <- smb$iv
  }
}

## Build the model
## First pass was a bust and defined a dummy variable for Avg Cpu Usage above mean  

logitMod <- glm(BelowQ1AvgCpuUsage ~ MinCpuUsage + AvgMemUsage + CpuMHz + NumCpu + MemoryMB, data=trainingData, family=binomial(link="logit"))
predicted <- predict(logitMod, testData, type="response")

  # Define optimal cutoff
  optCutOff <- optimalCutoff(testData$BelowQ1AvgCpuUsage, predicted[1])
  
  # Get model diagnostics
  summary(logitMod)
  vif(logitMod)
  misClassError(wide_df_model$BelowQ1AvgCpuUsage, predicted, threshold = optCutOff)
  
  # Plot ROC curve
  plotROC(wide_df_model$BelowQ1AvgCpuUsage, predicted)




  

  
           
           