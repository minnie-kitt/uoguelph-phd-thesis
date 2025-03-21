
data <- read.csv("~/Desktop/Quant/Analysis/Output/all_scores_demographic.csv", header = TRUE, row.names = "X")

### Recode 'Other'
data$gender <- ifelse(data$gender=="Prefer not to say","Other",data$gender)
data$gender <- ifelse(data$gender=="queer","Other",data$gender)
data$gender <- ifelse(data$gender=="trans_man","Other",data$gender)
data$gender <- ifelse(data$gender=="nb","Other",data$gender)
data$gender <- ifelse(data$gender=="trans_woman","Other",data$gender)



### Age Category
data$age_cat <- data$age
data$age_cat <- ifelse(data$age<25,"24 or less", data$age_cat)
data$age_cat <- ifelse(data$age>24 & data$age<30,"25 to 29", data$age_cat)
data$age_cat <- ifelse(data$age>29,"30 or more", data$age_cat)



### Ethnicity
data$ethnicity <- ifelse(data$ethnicity!="white" & data$ethnicity != "prefer_no","non_white",data$ethnicity)
data$ethnicity <- factor(data$ethnicity, levels = c("white","non_white","prefer_no"))


### Cohabitation
for (i in c("parent","partner","roommate")){
  data$cohabitation <- ifelse(data$cohabitation==i,"humans",data$cohabitation)
}
data$cohabitation <- ifelse(data$cohabitation=="multiple","pets_humans",data$cohabitation)

### Child
data$child <- ifelse(data$child>0,1,data$child)

### Career
data$career <- ifelse(data$career!="undecided" & data$career != "clinical", "non_clinical",data$career)

### Active
data$active <- ifelse(data$active>3,3,data$active)

write.csv(data, file = "Output/all_scores_demo-for-modelling.csv")