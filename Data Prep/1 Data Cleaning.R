# 1. Import and data prep ####

## Remove all objects in the environment and set working directory
rm(list=ls())
setwd("~/Desktop/Quant/Analysis")

## Background: The data was collected using four different Qualtrics Surveys: 2022 English, 2022 French, 2023 English, and 2023 French.
## Use the 'import_data' function to extract and reorganize the data from the different surveys into a consistent pattern.

source("Functions/import_data.R")

c22_en <- import_data("~/Desktop/Quant/C2022/CSV/rawdata/en-2022t4-2024may14-rawdata.csv",1,2022,"en")
c22_fr <- import_data("~/Desktop/Quant/C2022/CSV/rawdata/fr-2022t4-2024aug22-rawdata.csv",1,2022,"fr")
c23_en <- import_data("~/Desktop/Quant/C2023/CSV/en-2023t4-2025mar20-rawdata.csv",1:3,2023,"en")
c23_fr <- import_data("~/Desktop/Quant/C2023/CSV/fr-2023t4-2025mar20-rawdata.csv",1:3,2023,"fr")

rm(import_data)

## The data has duplicate responses from the same participant (ID) during the same time-point.
## We remove this using the 'drop_dupe_responses' function.

source("Functions/drop_dupe_responses.R")

c22_en <- drop_dupe_responses(c22_en)
c22_fr <- drop_dupe_responses(c22_fr)
c23_en <- drop_dupe_responses(c23_en)
c23_fr <- drop_dupe_responses(c23_fr)

rm(drop_dupe_responses)

## Now, we have four raw dataframes from the Qualtrics survey.
## The 'extract_col' function will take the raw dataframes and extract the relevant columns for further cleaning.
## We will do this by separating the demographic section from the psychometric scores.
## Thus, the 'choice' variable in the function will determine which section will be extracted.

source("Functions/extract_cols.R")

## Use the 'extract_cols' function to extract the demographic section into dataframes.

demo_c22_en <- extract_cols(c22_en, "demographic")
demo_c22_fr <- extract_cols(c22_fr, "demographic")
demo_c23_en <- extract_cols(c23_en, "demographic")
demo_c23_fr <- extract_cols(c23_fr, "demographic")

## Use the 'extract_cols' function to extract the psychometric scores into dataframes.

scores_c22_en <- extract_cols(c22_en, "scores")
scores_c22_fr <- extract_cols(c22_fr, "scores")
scores_c23_en <- extract_cols(c23_en, "scores")
scores_c23_fr <- extract_cols(c23_fr, "scores")

rm(extract_cols)

## The 'combine_files' function ensures that the combined dataframe is sorted appropriately based on the cohort and time-point
source("Functions/combine_files.R")

## Combine the four demographic dataframes into one and remove the separate ones.
demo_all <- combine_files(demo_c22_en,demo_c22_fr,demo_c23_en,demo_c23_fr) 
rm(demo_c22_en)
rm(demo_c22_fr)
rm(demo_c23_en)
rm(demo_c23_fr)

## Combine the four psychometric score dataframes into one and remove the separate ones.
scores_all <- combine_files(scores_c22_en,scores_c22_fr,scores_c23_en,scores_c23_fr) 
rm(scores_c22_en)
rm(scores_c22_fr)
rm(scores_c23_en)
rm(scores_c23_fr)

## Remove raw dataframes
rm(c22_en)
rm(c22_fr)
rm(c23_en)
rm(c23_fr)

rm(combine_files)


## The 'split_cols' function separates each variable of interest into a dataframe and combines them into a list of dataframes
source("Functions/split_cols.R")

demo_all_list <- split_cols(demo_all,"demographic")
rm(demo_all)

scores_all_list <- split_cols(scores_all,"scores")
rm(scores_all)

# 2. Recode Demographics ####

source("Functions/Demographics/1-ethnicity.R")
cleaned_demo_list <- recode_ethnicity(demo_all_list)
rm(demo_all_list)
rm(recode_ethnicity)

source("Functions/Demographics/2-cohab.R")
cleaned_demo_list <- recode_cohab(cleaned_demo_list)
rm(recode_cohab)

source("Functions/Demographics/3-disab.R")
cleaned_demo_list <- recode_disab(cleaned_demo_list)
rm(recode_disab)

source("Functions/Demographics/4-treatment.R")
cleaned_demo_list <- recode_treatment(cleaned_demo_list)
rm(recode_treatment)

source("Functions/Demographics/5-demo1.R")
cleaned_demo_list <- recode_demo1(cleaned_demo_list)
rm(recode_demo1)

source("Functions/Demographics/6-career.R")
cleaned_demo_list <- recode_career(cleaned_demo_list)
rm(recode_career)

source("Functions/Demographics/7-lifestyle.R")
cleaned_demo_list <- recode_lifestyle(cleaned_demo_list)
rm(recode_lifestyle)

source("Functions/summarize_demographic.R")

cleaned_demographic <- summarize_demographic(cleaned_demo_list)
#rm(cleaned_demo_list)
rm(summarize_demographic)

write.csv(cleaned_demographic, file = "Output/cleaned_demographic.csv")

# 3. Recode Psychometric Scores ####

source("Functions/ultimate-scoring-function-en-fr.R")
scores_list <- scoring(scores_all_list)
rm(scoring)

## Reset row names for each score df
for(scores_df in names(scores_list)){
  row.names(scores_list[[scores_df]]) <- NULL
}
rm(scores_df)

## Count the number of NA in each score df
source("Functions/mnar_check.R")
missing_count <- mnar_check(scores_list)

## Mean imputation
source("Functions/mean_imputation.R")
imputed_scores_list <- mean_imputation(scores_list)
rm(scores_list)
rm(mean_imputation)
imputed_missing_count <- mnar_check(imputed_scores_list)

## Extract relevant scores from the list of dataframe
## The 'individual' variable creates a dataframe with scores from all questions individually
## The 'total' variable creates a dataframe with the total scores only

source("Functions/extract_scores.R")

individual_cleaned_scores <- extract_scores(imputed_scores_list,"individual")
total_cleaned_scores <- extract_scores(imputed_scores_list,"total")

rm(imputed_scores_list)
rm(extract_scores)

write.csv(individual_cleaned_scores, file = "Output/individual_cleaned_scores.csv")

## Remove rows with more missing data than the acceptable cutoff
source("Functions/rm_missing_rows.R")

total_cleaned_scores <- rm_missing_rows(total_cleaned_scores,20)
colSums(is.na(total_cleaned_scores))

cleaned_demographic <- rm_missing_rows(cleaned_demographic,6)
colSums(is.na(cleaned_demographic))

write.csv(total_cleaned_scores, file = "Output/total_cleaned_scores.csv")

## Merge cleaned psychometric scores and cleaned demographic dataframes
combined_scores_demographic <- merge(total_cleaned_scores,cleaned_demographic,by.x= c("ID","class","time"), 
                                     by.y = c("ID","class","time"),
                                     all = FALSE,
                                     no.dups = TRUE)
all_scores_demographic <- combined_scores_demographic[order(combined_scores_demographic$class, combined_scores_demographic$time), ]
colSums(is.na(all_scores_demographic))

write.csv(all_scores_demographic, file = "Output/all_scores_demographic.csv")
rm(split_cols)