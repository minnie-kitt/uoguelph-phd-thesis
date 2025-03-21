recode_demo1 <- function(datalist){
  metadata <- datalist[["metadata"]]
  data <- datalist[["demo1"]]
  
  #### Prov
  
  previous_prov_txt <-  c(
    "United States- CT",
    "California",
    "USA",
    "Living in USA",
    "United States",
    "United States ",
    "Vermont",
    "Moved to California after vet school",
    "MO ",
    "Florida","United States (Illinois)",
    "California ","USA for residency",
    "USA for internship", "USA - NY/FL","USA - Vermont ","california"  
  )
  
  prov_txt_usa <-  c(
    "United States- CT",
    "California",
    "USA",
    "Living in USA",
    "United States",
    "United States ",
    "Vermont",
    "Moved to California after vet school",
    "MO ",    "USA for internship", "USA - NY/FL","USA - Vermont ","california",
    "Florida","United States (Illinois)", "California ","USA for residency" 
  )
  
  current_prov_txt <- na.omit(data[data$prov_txt!="",c("prov_txt")])
  diff_prov_text <- setdiff(current_prov_txt,previous_prov_txt)
  diff_prov_text_usa <- setdiff(current_prov_txt,prov_txt_usa)
  
  
  if(length(diff_prov_text)>0){
    print("There are 'prov_txt's that were not included in this code.")
  }
  
  if(length(diff_prov_text)==0){
    print("All 'Other' choices were USA.")
  }
  
  data$prov <- ifelse(data$prov=="Québec","Quebec",data$prov)
  data$prov <- ifelse(data$prov=="",NA,data$prov)
  
  data <- data[, -which(names(data) == c("prov_txt"))]
  
  #### School
  
  data$school <- ifelse(data$school=="",NA,data$school)
  
  #### Year of Birth and Age
  
  data$yob <- ifelse(data$yob=="",NA,data$yob)
  
  data$age <- data$yob
  data$age <- ifelse(data$age=="Before 1980",1979,data$age)
  data$age <- ifelse(data$age=="Prefer not to say",NA,data$age)
  data$age <- as.numeric(data$age)
  
  data$age <- as.numeric(datalist[["metadata"]][["class"]]) - data$age
  
  #### Gender
  
  previous_gender_txt <-  c(
    "Gender queer"
  )

  current_gender_txt <- na.omit(data[data$gender_txt!="",c("gender_txt")])
  diff_gender_text <- setdiff(current_gender_txt,previous_gender_txt)
  
  if(length(diff_gender_text)>0){
    print("There are 'gender_txt's that were not included in this code.")
  }
  
  data$gender <- ifelse(data$gender=="My gender is not listed above:",data$gender_txt,data$gender)
  
  data$gender <- ifelse(data$gender=="",NA,data$gender)
  
  data$gender <- ifelse(data$gender=="Cis-gendered man","cis_man",data$gender)
  
  data$gender <- ifelse(data$gender=="Cis-gendered woman","cis_woman",data$gender)
  data$gender <- ifelse(data$gender=="Femme cisgenre","cis_woman",data$gender)
  
  data$gender <- ifelse(data$gender=="Trans-gendered man","trans_man",data$gender)
  data$gender <- ifelse(data$gender=="Trans-gendered woman","trans_woman",data$gender)
  data$gender <- ifelse(data$gender=="Gender queer","nb",data$gender)
  data$gender <- ifelse(data$gender=="Non-binary","nb",data$gender)
  
  data <- data[, -which(names(data) == c("gender_txt"))]
  
  #### LGBTQIA+
  data$lgbt <- ifelse(data$lgbt=="",NA,data$lgbt)
  data$lgbt <- ifelse(data$lgbt=="No" | data$lgbt=="Non","not",data$lgbt)
  data$lgbt <- ifelse(data$lgbt=="Yes" | data$lgbt=="Oui","lgbt",data$lgbt)
  data$lgbt <- ifelse(data$lgbt=="Questioning","ques",data$lgbt)
  
  previous_relat_txt <-  c("Engaged & living together ")
  
  current_relat_txt <- na.omit(data[data$relat_txt!="",c("relat_txt")])
  diff_relat_txt <- setdiff(current_relat_txt,previous_relat_txt)
  
  if(length(diff_relat_txt)>0){
    print("There are 'relat_txt's that were not included in this code.")
  }
  #### Relationship status
  
  data <- data[, -which(names(data) == c("relat_txt"))]
  
  
  data$relat <- ifelse(data$relat=="Cohabitation / vie commune","cohab",data$relat)
  data$relat <- ifelse(data$relat=="Cohabitating / living together","cohab",data$relat)
  
  data$relat <- ifelse(data$relat=="Célibataire","single",data$relat)
  data$relat <- ifelse(data$relat=="Single","single",data$relat)
  
  data$relat <- ifelse(data$relat=="Committed relationship (not living together)","relationship",data$relat)
  data$relat <- ifelse(data$relat=="En relation (ne vivant pas ensemble)","relationship",data$relat)
  
  data$relat <- ifelse(data$relat=="Marié(e) / conjoint(e) de fait / union civile","married",data$relat)
  data$relat <- ifelse(data$relat=="Married / Common Law / Civil Union","married",data$relat)
  data$relat <- ifelse(data$relat=="Engaged & living together ","married",data$relat)
  
  data$relat <- ifelse(data$relat=="Separated / Divorced","separated",data$relat)
  
  data$relat <- ifelse(data$relat=="My relationship status is not listed here (please specify):",NA,data$relat)
  data$relat <- ifelse(data$relat=="",NA,data$relat)
  
  
  #### Number of children
  data$child <- ifelse(data$child=="",NA,data$child)
  
  data$child <- ifelse(data$child=="Aucun",0,data$child)
  data$child <- ifelse(data$child=="None",0,data$child)
  
  
  cleaned_data <- data[,c("prov","school","yob","age","gender","lgbt","relat","child")]
  
  metadata <- datalist[["metadata"]]
  new_data <- cbind(metadata,cleaned_data)
  
  datalist[["demo1"]] <- new_data
  new_datalist <- datalist
  
  return(new_datalist)
  
}




















