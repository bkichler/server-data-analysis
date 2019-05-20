---
title: "Predictive Model for Average CPU Usage"
author: "Brian Kichler"
date: "May 18, 2019"
output: html_document
---



## Setting Up the Data

Logistic regression is an appropriate method to predict the probability of an event by defining a dichotomous/dummy variable. In this case, we want to see if we can develop a model that predicts whether a VM will be underutilized and a candidate for reduction in number of CPUs.

We can start by defining a new dummy variable, which will be on AvgCpuUsage in wide_df. We'll create the variable "BelowQ1AvgCpuUsage", which is defined as true (1) when an observation falls below the first quartile value for Average CPU Usage:


```r
wide_df_model <- wide_df %>%
  mutate(BelowQ1AvgCpuUsage = as.numeric(AvgCpuUsage < 168236689))
```

There will obviously be bias due to the definition of this variable. Here's what it looks like:


```r
table(wide_df_model$BelowQ1AvgCpuUsage)
```

```
## 
##      0      1 
## 165220  55074
```

To mitigate this, this code creates a data frame called "trainingData," which equally samples ones and zeroes with a set seed, then folds the leftover data into another data frame called "testData":


```r
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
```

Next, the variables "Cluster" and "NumCpu" should be converted to factors, then we can calculate the IVs for the included variables in our wide_df_model data frame. "MemoryMB" and "CpuMHz" are treated as continuous:


```r
wide_df$Cluster <- as.factor(wide_df$Cluster)
wide_df$NumCpu <- as.factor(wide_df$NumCpu)
factor_vars <- c("Cluster", "NumCpu")
continuous_vars <- c("AvgMemUsage", "MinMemUsage", "MaxMemUsage", "AvgCpuUsage", "MinCpuUsage", "MaxCpuUsage", "MemUsageRange", "CpuUsageRange", "MemoryMB", "CpuMHz")
```

## Including Plots

You can also embed plots, for example:

![plot of chunk pressure](figure/pressure-1.png)

Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.
