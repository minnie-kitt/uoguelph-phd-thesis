## This is the R Code associated with 'Chapter 2: Thriving or Surviving? A Latent Profile Analysis of Resilience, Psychological Well-being, and Professional Quality of Life in Early-career Veterinarians in Canada
## Authors: Daniel Gillis & Tipsarp Kittisiam


## Load prerequisite packages and functions
library(dplyr)
library(tidyverse)
library(reshape2)
library(tidyLPA)
library(ggplot2)
library(tibble)
library(mclust)
source("Data Prep/Functions/dg_lpa_functions.R")

## Import data
df_indiv <- read.csv("~/Desktop/Quant/Analysis/Output/individual_cleaned_scores.csv", header = TRUE, row.names = "X")
df_indiv <- df_indiv[c(1:55,107:116)]

## assign the appropriate data columns to their corresponding psychometric measure
cdr <- c(4:13)
pwb <- c(14:55)
proqol <- c(56:64)
burnout <- 65

p_autonomy <- pwb[c(1,7,13,19,25,31,37)]
p_envi_master <- pwb[c(2,8,14,20,26,32,38)]
p_pers_growth <- pwb[c(3,9,15,21,27,33,39)]
p_pos_relation <- pwb[c(4,10,16,22,28,34,40)]
p_purpose <- pwb[c(5,11,17,23,29,35,41)]
p_self_acc <- pwb[c(6,12,18,24,30,36,42)]
  
pq_cf <- proqol[c(1,4,7)]
pq_bo <- proqol[c(2,6,8)]
pq_cs <- proqol[c(3,5,9)]
  
all_scales <- list(Resilience=cdr,Burnout=burnout,
                PWB = pwb,
                CB=pq_bo,STS=pq_cf,CS=pq_cs ## ProQOL
                )

## Create a subset of the data with only the first time-point named 'baseline'
baseline <- df_indiv[df_indiv$metadata.count==1,]
rm(df_indiv)
## Remove NA from this new 'baseline' subset, now named 'df_rm_na'
df_rm_na <- na.omit(baseline)
rm(baseline)
rownames(df_rm_na) <- NULL

## Normalize the data to have mean = 0 and SD = 1
scaled_df <- data.frame(scale(df_rm_na[,c(4:65)]))

### Set seed
set.seed(1995)
### Run 400 iterations of the mclustBIC function to produce the three highest BIC during each run
### and save the results as a .csv file
BIC<-mclustBIC(scaled_df)
model1 <- c(1,names(summary(BIC)[1]), summary(BIC)[[1]]-summary(BIC)[[1]],
            names(summary(BIC)[2]), summary(BIC)[[1]]-summary(BIC)[[2]],
            names(summary(BIC)[3]), summary(BIC)[[1]]-summary(BIC)[[3]])

### Caution: this for loop may take a while...
for(iteration in 1:399){
  BIC<-mclustBIC(scaled_df)
  print(summary(BIC))
   temp <- c(iteration+1,
             names(summary(BIC)[1]), summary(BIC)[[1]]-summary(BIC)[[1]],
             names(summary(BIC)[2]), summary(BIC)[[1]]-summary(BIC)[[2]],
             names(summary(BIC)[3]), summary(BIC)[[1]]-summary(BIC)[[3]])
   model1 <- rbind(model1,temp)
 }
 model1 <- as.data.frame(model1)
 
 colnames(model1) <- c("Iteration", "M1_Name","M1_Value",
                       "M2_Name","M2_Value","M3_Name","M3_Value")
 model1$Iteration <- as.numeric(model1$Iteration)
 row.names(model1) <- NULL
 write.csv(model1,"Output/ch2-clusters-model1-iterations.csv")

## Visualize the model configuration with the highest BIC using line plots
model1 <- read.csv("Output/ch2-clusters-model1-iterations.csv", row.names = "X")
table(model1[2])
table(model1[4])
plot(model1$M2_Value, type = "l")
table(model1[6])
plot(model1$M3_Value, type = "l")


## Plot the model configuration (volume, shape, and orientation) and number of clusters versus the BIC
plot(BIC, legendArgs = list(x = "bottomright", ncol = 5))
### The three models with the highest BIC were EII,5 ; EEI,5 ; and EII,4
summary(BIC)
### The highest BIC was EII,5
lpa.results<-LPA(scaled_df, "EII", 5, BIC)
summary(lpa.results)
### Summarize the 2nd and 3rd models for comparison
summary(LPA(scaled_df, "EEI", 5, BIC))
summary(LPA(scaled_df, "EII", 4, BIC))

## Create a new column with the classification provided by Model 1 (EII,5; n=191)
df_rm_na$class1 <- lpa.results$classification
## Visualize each question scoring individually, by group
calculateQuestionMeansScaled(lpa.results,lowlim = -2.6,highlim = 2.6)

## Remove members of groups 4 & 5 (outliers)
df2 <- df_rm_na[-c(96,163),]

## normalize the new dataset which removed outliers (n=189)
scaled_df2 <- data.frame(scale(df2[,c(4:65)]))

## Run 400 iterations of the mclustBIC function (same as above)
BIC<-mclustBIC(scaled_df2)

 model2 <- c(1,
             names(summary(BIC)[1]), summary(BIC)[[1]]-summary(BIC)[[1]],
             names(summary(BIC)[2]), summary(BIC)[[1]]-summary(BIC)[[2]],
             names(summary(BIC)[3]), summary(BIC)[[1]]-summary(BIC)[[3]])
 for(iteration in 1:399){
   BIC<-mclustBIC(scaled_df2)
   temp <- c(iteration+1,
             names(summary(BIC)[1]), summary(BIC)[[1]]-summary(BIC)[[1]],
             names(summary(BIC)[2]), summary(BIC)[[1]]-summary(BIC)[[2]],
             names(summary(BIC)[3]), summary(BIC)[[1]]-summary(BIC)[[3]])
   model2 <- rbind(model2,temp)
 }
 model2 <- as.data.frame(model2)
 
 colnames(model2)  <- c("Iteration", "M1_Name","M1_Value",
                       "M2_Name","M2_Value","M3_Name","M3_Value")
 model2$Iteration <- as.numeric(model2$Iteration)
 row.names(model2) <- NULL
 write.csv(model2,"Output/ch2-clusters-model2-iterations.csv")
 ## Visualize the model configuration with the highest BIC using line plots
model2 <- read.csv("Output/ch2-clusters-model2-iterations.csv", row.names = "X")
table(model2[2])
plot(model2$M2_Value, type = "l")
table(model2[4])
plot(model2$M3_Value, type = "l")
table(model2[6])

## Plot the model configuration (volume, shape, and orientation) and number of clusters versus the BIC
plot(BIC, legendArgs = list(x = "bottomright", ncol = 5))
### The three models with the highest BIC were EII,2 ; EEI,4 ; and EII,5
summary(BIC)
### The highest BIC was EII,2
lpa.results<-LPA(scaled_df2, "EII", 2, BIC)
summary(lpa.results)
### Summarize the 2nd and 3rd models for comparison
summary(LPA(scaled_df2, "EEI", 4, BIC))
summary(LPA(scaled_df2, "EII", 5, BIC))

## Create a new column with the classification provided by Model 2 (EII,2; n=189)
df2$class2 <- lpa.results$classification

## Combine the classification from Model 1 and Model 2 into the same dataframe
df_results <- merge(df_rm_na,df2,all.x = TRUE)
### Observations that were removed are assigned Group = 0
df_results[is.na(df_results)] <- 0
### Create a normalized version of the combined dataframe
scaled_df_results <- df_results
scaled_df_results[,c(4:65)] <- scale(df_results[,c(4:65)])

## Compare group membership according to Model 1 vs Model 2
table(df_results$class1,df_results$class2)

## Visualize each question scoring individually, by group
calculateQuestionMeansScaled(lpa.results)

## Write a function to format the results to display Mean (95%CI)
format_results <- function(results_df,measurement = "Dimension", with_ci = TRUE){
  if(with_ci==TRUE){
    out <- data.frame(cbind(results_df[measurement],results_df["Group"],results_df["Mean"]))
    colnames(out) <- c(measurement, "Group","Mean (95%CI)")
    all_measurement <- unique(results_df[[measurement]])
    all_group <- unique(results_df[["Group"]])
    for(group in all_group){
      for(measure in all_measurement){
        temp_row <- results_df[results_df[[measurement]]==measure & results_df[["Group"]]==group,]
        row_num <- row.names(results_df[results_df[[measurement]]==measure & results_df[["Group"]]==group,])
        group_mean <- round(temp_row[["Mean"]],2)
        group_CILow <- round(temp_row[["CILow"]],2)
        group_CIHigh <- round(temp_row[["CIHigh"]],2)
        output <- paste0(group_mean," (95%CI ",group_CILow," - ",group_CIHigh,")")
        out[row_num,3] <- output
      }}}
  return(out)
  }

temp <- df2
temp[cdr]<- temp[cdr]*10
temp[proqol] <- temp[proqol]*3
temp[pwb] <- temp[pwb]
results_all <- summarizeDimensionMeansScaled(temp,"class2",dimension = "all", calculate_ci = TRUE)

## Display the formatted results of Group 1 according to Model 2
format_results(results_all[results_all$Group==1,])
## Display the formatted results of Group 2 according to Model 2
format_results(results_all[results_all$Group==2,])

# End of file

