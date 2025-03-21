extract_scores <- function(datalist,choice){
  if(choice=="individual"){
    temp_df <- do.call(cbind, datalist)
    total_cols <- c("cdrisc.total","pwb.total","pwb.autonomy","pwb.envi_master","pwb.pers_growth",
                    "pwb.pos_relation","pwb.purpose","pwb.self_acc","ngse.total", "flo.total","swls.total",
                    "teique.total","teique.global_ei","teique.wellbeing","proqol.total", 
                    "teique.self_ctrl","teique.emotionality","teique.sociability", "teique.motivation", 
                    "teique.adaptability",
                    "proqol.cf","proqol.bo","proqol.cs","burnout.datalist...burnout...","sise.datalist...sise...")
    out <- subset(temp_df, select = -which(names(temp_df) %in% total_cols))
  } else if (choice=="total"){
    out <- data.frame(datalist[["metadata"]][["ID"]],
                      datalist[["metadata"]][["class"]],
                      datalist[["metadata"]][["count"]],
                      datalist[["cdrisc"]][["total"]], datalist[["ngse"]][["total"]],
                      datalist[["flo"]][["total"]],datalist[["swls"]][["total"]],
                      datalist[["pwb"]][["autonomy"]],datalist[["pwb"]][["envi_master"]],datalist[["pwb"]][["pers_growth"]],
                      datalist[["pwb"]][["pos_relation"]],datalist[["pwb"]][["purpose"]], datalist[["pwb"]][["self_acc"]],
                      datalist[["pwb"]][["total"]],
                      datalist[["teique"]][["wellbeing"]],datalist[["teique"]][["self_ctrl"]],
                      datalist[["teique"]][["emotionality"]],datalist[["teique"]][["sociability"]],
                      datalist[["teique"]][["motivation"]],
                      datalist[["teique"]][["adaptability"]],
                      datalist[["teique"]][["global_ei"]],
                      datalist[["proqol"]][["cf"]],datalist[["proqol"]][["bo"]],datalist[["proqol"]][["cs"]],
                      datalist[["proqol"]][["total"]],
                      datalist[["sise"]][["sise"]],datalist[["burnout"]][["burnout"]])
    
    colnames(out) <- c("ID","class","time",
                       "cdrisc", "ngse", 
                       "flo", "swls",
                       "p_autonomy",'p_envi_master',"p_pers_growth","p_pos_relation","p_purpose","p_self_acc","pwb",
                       "e_wellbeing","e_self_ctrl","e_emotion","e_social",
                       "e_motivation","e_adaptability","ei_global",
                       "pq_cf","pq_bo","pq_cs","proqol",
                       "sise", "burnout")
    rownames(out)<-NULL
  }
  return(out)
}