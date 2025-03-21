recode_career <- function(datalist){
  data <- datalist[["career"]]
  
  previous_other_txt <-  c(
    "Industry ",
    "Lab animal",
    "Cardiology",
    "Specialty",
    "Aquaculture ",
    "Aquatics",
    "pathology",
    "Emergency",
    "Aquaculture",
    "Anatomic Pathology",
    "Clinical teaching",
    "Aquacuture",
    "Wildlife",
    "Mixed Practice",
    "Currently working in small animal but main interest is bovine/small ruminant",
    "Specialization",
    "Small animal rotating intern",
    "Specialty intern/resident",
    "Intern ",
    "Specialization",
    "Speicalization",
    "Currently doing MBA degree in addition to practicing vet med",
    "Pathology",
    "Mixed",
    "Laboratory Animal Medicine",
    "Mixed animal rural",
    "Industry",
    "Pharmaceuticals",
    "Pharmaceuticals ",
    "intern (rotating)",
    "Lab animal Medicine",
    "Éthique",
    "Veterinary Resident",
    "Locum vet ",
    "Fish health/Aquaculture",
    "Imaging resident",
    "Emergency ",
    "Emergency Vet-small animal",
    "Part-time practicing vet and part-time Master's student",
    "Laboratory animal veterinarian","Chats"   
    
  )

  current_other_txt <- na.omit(data[data$other_txt!="",c("other_txt")])
  diff_other_text <- setdiff(current_other_txt,previous_other_txt)
  
  if(length(diff_other_text)>0){
    print("There are 'other_txt's that for `career` were not included in this code.")
  }
  
  recode_special <- c("Anatomic Pathology","Cardiology","Emergency","Pathology","pathology","Specialty",
                      "Specialization","Speicalization",    "Specialization",
                      "Specialty intern/resident", "Small animal rotating intern", "Intern ",
                      "intern (rotating)","Veterinary Resident","Locum vet ","Imaging resident",
                      "Emergency ","Emergency Vet-small animal", "Chats"
                      )
  data$special <- 0
  for (i in recode_special){
    data$special <- as.numeric(ifelse(data$other_txt==i,1,data$special))}
 
  recode_farm <- c("Aquaculture","Aquaculture ","Aquacuture","Aquatics","Fish health/Aquaculture")
  for (i in recode_farm){
    data$farm <- ifelse(data$other_txt==i,1,data$farm)}
  recode_academia <- c("Clinical teaching", "Éthique")
  for (i in recode_academia){
    data$academia <- ifelse(data$other_txt==i,1,data$academia)}

  recode_lab <- c("Lab animal","Laboratory Animal Medicine", "Lab animal Medicine","Laboratory animal veterinarian")
  data$lab <- 0
  for (i in recode_lab){
    data$lab <- ifelse(data$other_txt==i,1,data$lab)}

  recode_non_vet <- c("Industry ",
                      "Currently doing MBA degree in addition to practicing vet med",
                      "Industry",
                      "Pharmaceuticals",
                      "Pharmaceuticals ",
                      "Part-time practicing vet and part-time Master's student")
  for (i in recode_non_vet){
    data$non_vet <- ifelse(data$other_txt==i,1,data$non_vet)}
  
  
  recode_mixed <- c("Mixed","Mixed animal rural","Mixed Practice")
  data$mixed <- 0
  for (i in recode_mixed){
    data$mixed <- ifelse(data$other_txt==i,1,data$mixed)}
  
  
  recode_exotic <- c("Wildlife")
  for (i in recode_exotic){
    data$exotic <- ifelse(data$other_txt==i,1,data$exotic)}
  
  ### PRINT
  
  print("The following transformations were made from 'Other(Please Specify):'")
  print(c(paste(recode_special," ===> special"),    paste(recode_academia," ===> academia"),    paste(recode_lab," ===> lab"),
          paste(recode_non_vet," ===> non_vet"),    paste(recode_mixed," ===> mixed"),     paste(recode_exotic," ===> exotic"),
          paste(recode_farm," ===> farm")))
  
  for (i in c(    "Currently working in small animal but main interest is bovine/small ruminant")){
    data$small <- ifelse(data$other_txt==i,1,data$small)}
  
  for (i in c("small","equine","ruminants","exotic","farm","public_h","academia","non_vet","undecided","other","prefer_no")){
    data[[i]] <- as.numeric(ifelse(data[[i]]==0,0,
                                   ifelse(data[[i]]=="",NA,
                                          ifelse(is.na(data[[i]]),NA,
                                                 1))))}
  data1 <- data
  data1[is.na(data1)] <- 0
  data2 <- data1[,c("small","equine","ruminants","exotic","farm","public_h","academia","non_vet","lab","special","mixed")]
  data$sum <- apply(data2, 1, sum)
  
  data$career <- 0
  for (i in c("small","equine","ruminants","exotic","farm","public_h","academia","non_vet","undecided","lab","special","mixed")){
    data$career <- ifelse(data$sum==1 & data[[i]]==1,i,data$career)}
  
  data$career_type <- ""
  for (i in c("small","equine","ruminants","exotic","farm","public_h","academia","non_vet","undecided","lab","special","mixed")){
    data$career_type <- ifelse(data[[i]]==1,paste(data$career_type,i),data$career_type)
  }
  
  ## Check combinations
  data$career <- ifelse(data$sum==0,NA,data$career)
  combine_clinical <- c(" small special"," small exotic"," small ruminants"," equine ruminants"," small equine",
                        " small"," equine"," ruminants"," mixed"," exotic", " special",
                        " small equine ruminants"," small ruminants exotic"," small exotic special"," small equine exotic", " equine special")
  for (i in combine_clinical){
    data$career <- ifelse(data$career_type==i,"clinical",data$career)}  
  
  data$career <- ifelse(data$academia==1& data$sum<3, "academia",data$career)
  data$career <- ifelse(data$undecided==1, "undecided",data$career)
  
  data$career <- ifelse(data$sum>0 & data$career==0, "undecided",data$career)
  
  for (i in c("Rotating intern at academic institution ","ne s'applique pas","")){
    data$practice <- ifelse(data$practice==i,NA,data$practice)}
  
  for (i in c("No preference","Aucune préférence")){
    data$practice <- ifelse(data$practice==i,"none",data$practice)}
  
  for (i in c("Private practice","Cabinet privé")){
    data$practice <- ifelse(data$practice==i,"private",data$practice)}
  
  for (i in c("Corporate practice (i.e. VCA, Banfield)")){
    data$practice <- ifelse(data$practice==i,"corp",data$practice)}
  
  for (i in c("Je préfère ne pas répondre")){
    data$practice <- ifelse(data$practice==i,"prefer_no",data$practice)}
  
  
  previous_practice_txt <-  c("Private or corporate; prefer not VCA though","Private or corporate",
                               "Private and VCA partnered (50/50)","N/A",
                               " ","Government ",
                               "Referral hospital","Starting out corporate but long term would rather private ",
                               "Provincial Government ","Government ",
                              "Government (General Practice)",
                              "Private or corporate but entirely dependent on the team/ management ",
                              "Rotating intern at academic institution ",
                              "ne s'applique pas","Academia or specialty (either private or corporate)"
  )
  
  current_practice_txt <- na.omit(data[data$practice_txt!="",c("practice_txt")])
  diff_practice_text <- setdiff(current_practice_txt,previous_practice_txt)
  
  if(length(diff_practice_text)>0){
    print("There are 'other_txt's for `practice` that were not included in this code.")
  }
  
  recode_no_pref <- c("Private or corporate; prefer not VCA though","Private or corporate"," ",
                      "Private and VCA partnered (50/50)","N/A","Starting out corporate but long term would rather private",
                      "Academia or specialty (either private or corporate)")
  for(i in recode_no_pref){
    data$practice <- ifelse(data$practice_txt==i,"none",data$practice)}

  recode_private <- c("Referral hospital")
  for(i in recode_private){
    data$practice <- ifelse(data$practice_txt==i,"private",data$practice)}
  
  recode_govt <- c("Government ",
                   "Provincial Government ","Government ")
  for(i in recode_govt){
    data$practice <- ifelse(data$practice_txt==i,"govt",data$practice)}
  
  
  cleaned_data <- data[,c("career","career_type","practice")]
  
  metadata <- datalist[["metadata"]]
  new_data <- cbind(metadata,cleaned_data)
  datalist[["career"]] <- new_data
  new_datalist <- datalist
  
  return(new_datalist)
  

  }



