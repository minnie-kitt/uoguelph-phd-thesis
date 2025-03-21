
###################
# Functions
###################

summarize_Q<-function(data, output_csv_path, confidence_level=0.95)
{
  # Create a data frame to store results
  results_df <- data.frame(Column = character(), 
                           Mean = numeric(), 
                           ME = numeric(),
                           CI_Low = numeric(), 
                           CI_High = numeric())
  
  # Iterate over each column
  for (col_name in names(data)) 
  {
    # Ensure the column is numeric
    if (is.numeric(data[[col_name]])) 
    {
      # Calculate mean
      mean_val = mean(data[[col_name]], na.rm = TRUE)
      
      # Calculate standard error
      std_error = sd(data[[col_name]], na.rm = TRUE) / sqrt(length(data[[col_name]]))
      
      # Calculate confidence interval
      error_margin = qt(confidence_level / 2 + 0.5, df = length(data[[col_name]]) - 1) * std_error
      ci_lower = mean_val - error_margin
      ci_upper = mean_val + error_margin
      
      # Add the results to the data frame
      results_df <- rbind(results_df, data.frame(Column = col_name, 
                                                 Mean = mean_val, 
                                                 ME = error_margin,
                                                 CI_Low = ci_lower, 
                                                 CI_High = ci_upper))
    }
  }
  
  # Write results to CSV
  write.csv(results_df, output_csv_path, row.names = FALSE)
  
  # Plot the results
  temp.plot<-ggplot(data=results_df, aes(x=Column, y=Mean)) + 
    geom_bar(stat="identity") + 
    scale_y_continuous(limits=c(0, 7)) +
    geom_errorbar(aes(ymin=Mean-ME, 
                      ymax=Mean+ME, 
                      width=0.2)) +
    ggtitle(paste("Mean Question Responses (Unscaled)\nIncluding ", 
                  confidence_level*100,
                  "% Confidence Intervals",
                  sep="")) 
  
  temp.plot
}

summarize_D<-function(data, dimension, confidence_level=0.95)
{
  raw.data<-data.frame(Responses=rowSums(data))
  
  # Calculate mean
  mean_val = mean(raw.data$Responses, na.rm = TRUE)
  
  # Calculate standard error
  std_error = sd(raw.data$Responses, na.rm = TRUE) / sqrt(length(raw.data$Responses))
  
  # Calculate confidence interval
  error_margin = qt(confidence_level / 2 + 0.5, df = length(raw.data$Responses) - 1) * std_error
  ci_lower = mean_val - error_margin
  ci_upper = mean_val + error_margin
  
  # Plot the results
  temp.plot<-ggplot(data=raw.data, aes(x=Responses)) + 
    geom_histogram() +
    ggtitle(paste("Distribution of ",
                  dimension, 
                  " Score\nMean (", 
                  confidence_level*100,
                  "% CI) = ",
                  round(mean_val, digits=2), 
                  " (",
                  round(ci_lower, digits=2), 
                  ", ",
                  round(ci_upper, digits=2), 
                  ")",
                  sep="")) + 
    geom_vline(aes(xintercept=mean(Responses)),
               col="red",
               size=0.5)
  
  temp.plot
  
  return(temp.plot)
}

LPA<-function(scaled.data, modelType="VEI", clusters=2, method=BIC)
{
  # Run the model
  mod<-Mclust(scaled.data, modelNames=modelType, G=clusters, x=method)
  
  return(mod)
}

calculateQuestionMeansScaled<-function(mod, lowlim = -1, highlim = 1)
{
  # Extract the means for each question given the cluster
  means <- data.frame(mod$parameters$mean, stringsAsFactors = FALSE) %>%
    rownames_to_column() %>%
    rename(Question = rowname) %>%
    melt(id.vars = "Question", variable.name = "Group", value.name = "Mean") %>%
    mutate(Mean = round(Mean, 2))
  
  means$Group<-factor(means$Group, levels=paste("X", 1:mod$G, sep=""))
  levels(means$Group)<-paste("Group", 1:mod$G, sep=" ")
  
  qmeans<-ggplot(data=means, aes(x=Question, y=Mean, group=Group, color=Group)) +
    geom_line() +
    theme(legend.text=element_text(size=10),axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    ggtitle("Scaled Means per Question by Group Classification") +
    scale_y_continuous(limits=c(lowlim, highlim))
  
  qmeans
}

calculateQuestionMeansUnscaled<-function(data, classifications, confidence_level=0.95,plot_lim =c(0, 7))
{
  data<-data.frame(data, Class=classifications)
  # Extract the means for each question given the cluster
  results<-data.frame(Question=NULL,
                      Mean=NULL,
                      StdErr=NULL,
                      CILow=NULL,
                      CIHigh=NULL,
                      Group=NULL)
  
  # Iterate over each column
  for (col_name in names(data)[-length(names(data))]) 
  {
    # Ensure the column is numeric
    if (is.numeric(data[[col_name]])) 
    {
      # Calculate mean
      mean_val = aggregate(data[[col_name]], by=list(data$Class), mean)
      
      # Calculate standard deviation
      std_dev = aggregate(data[[col_name]], by=list(data$Class), sd)
      
      # Calculate the number of observations
      n = aggregate(data[[col_name]], by=list(data$Class), length)
      
      # Calculate the standard error
      std_error = std_dev[,2]/sqrt(n[, 2])
      
      # Calculate confidence interval
      error_margin = qt(confidence_level / 2 + 0.5, df = n[, 2] - 1) * std_error
      ci_lower = mean_val[, 2] - error_margin
      ci_upper = mean_val[, 2] + error_margin
      
      # Add the results to the data frame
      results <- rbind(results, 
                       data.frame(Question=col_name, 
                                  Mean=mean_val[, 2],
                                  StdErr=std_error,
                                  CILow=ci_lower,
                                  CIHigh=ci_upper,
                                  Group=as.factor(mean_val[, 1]))
      )
    }
  }
  
  temp.plot<-ggplot(data=results, aes(x=Question, y=Mean, group=Group, color=Group)) +
    geom_line() +
    theme(legend.text=element_text(size=10),axis.text.x = element_text(vjust = 0.5, hjust=1)) +
    ggtitle(paste("Means per Question by Group Classification\nIncluding ", 
                  confidence_level*100, 
                  "% Confidence Intervals", 
                  sep="")) +
    geom_line(data=results, 
              linetype="dashed",
              aes(x=Question, y=CILow, group=Group, color=Group), 
              inherit.aes=FALSE) +
    geom_line(data=results, 
              linetype="dashed",
              aes(x=Question, y=CIHigh, group=Group, color=Group), 
              inherit.aes=FALSE) +
    scale_y_continuous(limits=plot_lim)
  
  temp.plot
  
  print(temp.plot)
  return(results)
}

plotQuestionMeansUnscaled<-function(results, confidence_level=0.95)
{ 
  temp.plot<-ggplot(data=results, aes(x=Question, y=Mean, group=Group, color=Group)) +
    geom_line() +
    theme(legend.text=element_text(size=10),axis.text.x = element_text(vjust = 0.5, hjust=1)) +
    ggtitle(paste("Means per Question by Group Classification\nIncluding ", 
                  confidence_level*100, 
                  "% Confidence Intervals", 
                  sep="")) +
    geom_line(data=results, 
              linetype="dashed",
              aes(x=Question, y=CILow, group=Group, color=Group), 
              inherit.aes=FALSE) +
    geom_line(data=results, 
              linetype="dashed",
              aes(x=Question, y=CIHigh, group=Group, color=Group), 
              inherit.aes=FALSE) +
    scale_y_continuous(limits=c(0, 7))
  return(temp.plot)
}

summarizeDimensionMeansScaled<-function(scaled.data, classifications, dimension="all", calculate_ci = TRUE,
                                        confidence_level=0.95)
{
  
  classifications <- scaled.data[[classifications]]
  if(dimension=="all"){
    results<-expand.grid(Dimension=names(all_scales),
                        Group=min(classifications):max(classifications),
                        Mean=0,CILow=0,CIHigh=0)
    for (i in 1:length(all_scales)) {
      # get rowSums
      scores<-rowMeans(scaled.data[all_scales[[i]]], na.rm=TRUE)
      scores<-data.frame(scores=scores, class=classifications)
      
      # calculate classification/group statistics
      group.means<-aggregate(scores$scores, by=list(scores$class), mean)
      group.var<-aggregate(scores$scores, by=list(scores$class), var)
      group.n<-aggregate(scores$scores, by=list(scores$class), length)
      
      stderror<-sqrt(group.var[, 2]/group.n[, 2])
      
      if(calculate_ci==FALSE){
        results[results["Dimension"]==names(all_scales)[i],]<-data.frame(Dimension=names(all_scales)[i],
                                                                         Group=min(classifications):max(classifications),
                                                                         Mean=group.means[, 2],CILow=NA,CIHigh=NA)
      }
      
      if(calculate_ci==TRUE){
        # calculate confidence intervals
        marginError<-qt(confidence_level / 2 + 0.5, df = group.n[, 2] - 1) * stderror
        CILow<-group.means[, 2]-marginError
        CIHigh<-group.means[, 2]+marginError
        
        results[results["Dimension"]==names(all_scales)[i],]<-data.frame(Dimension=names(all_scales)[i],
                                                                         Group=min(classifications):max(classifications),
                                                                         Mean=group.means[, 2],
                                                                         CILow=CILow,
                                                                         CIHigh=CIHigh)
        
      }
      
    }
  } else {
    # get rowSums
    scores<-rowMeans(scaled.data[dimension], na.rm=TRUE)
    scores<-data.frame(scores=scores, class=classifications)
    
    # calculate classification/group statistics
    group.means<-aggregate(scores$scores, by=list(scores$class), mean)
    group.var<-aggregate(scores$scores, by=list(scores$class), var)
    group.n<-aggregate(scores$scores, by=list(scores$class), length)
    
    stderror<-sqrt(group.var[, 2]/group.n[, 2])
    
    if(calculate_ci==FALSE){
      results[results["Dimension"]==names(all_scales)[i],]<-data.frame(Dimension=names(all_scales)[i],
                                                                       Group=min(classifications):max(classifications),
                                                                       Mean=group.means[, 2])
    }
  }
  return(results)
}

plotDimensionSummaries<-function(plot.data, type="Scaled",dimension="Dimension", lowlim=-2.5, highlim=2.5)
{
  # plot the results
  temp.plot<-ggplot(data=plot.data, aes(x=Dimension, y=Mean, group=as.factor(Group), color=as.factor(Group))) +
    geom_line() +
    geom_line(data=plot.data, 
              linetype="dashed",
              aes(x=Dimension, y=CILow, group=as.factor(Group), color=as.factor(Group)), 
              inherit.aes=FALSE) +
    geom_line(data=plot.data, 
              linetype="dashed",
              aes(x=Dimension, y=CIHigh, group=as.factor(Group), color=as.factor(Group)), 
              inherit.aes=FALSE) +
    ggtitle(paste(type, " Means per ",dimension," by Group Classification", sep="")) +
    scale_y_continuous(limits=c(lowlim, highlim))
  
  temp.plot  
}

