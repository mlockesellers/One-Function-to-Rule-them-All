#Last updated by MLS 4.24.24

#Load in necessary libraries
library(car)
library(ggplot2)
library(plyr)
library(dplyr)
library(ggrepel)
library(ggpp)
library(patchwork)
library(stringr)


#Set Working Directory
#Choose the working directory that you need
#setwd("~/Documents/The One Ring Code")

#Set up the function
#Make sure the .csv has the same name as your document
#CpFT_ref input options are "Ohio_R","Ohio_M",Southern_Africa_R", or "Northern_Europe_R" written exactly like that

i=1
LEH.fun <- function(csv_file_name,CpFT_ref) {
  #Part 1: Data Setup and LOWESS Curve Production
  
  #Read the specific data into the function: Make sure you have the exact file
  #name to avoid errors
  data = read.csv(csv_file_name) 
  
  #Split the Data by Tooth
  dat_list <- split(data, data$Tooth)
  
  #Creating a date-specific pdf- you can edit the pdf name here
  pdf(file = paste(Sys.Date(),"LEH Data.pdf"))
  par(mfrow = c(1,1))
  
  #Identifying outlier values
  f <- function(x){
    r <- quantile(x, probs = c(0.1, 0.25, 0.5, 0.75, 0.9), na.rm = TRUE)
    names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
    r
  }
  o <- function(x){
    subset(x, x < quantile(x,0.1) | quantile(x,0.9) < x)
    outlier <- subset(x, x < quantile(x,0.1, na.rm = TRUE) | quantile(x,0.9, na.rm = TRUE) < x)
    print(outlier)
  }
  
  #Create a blank dataframe to save LEH outliers
  LEH_list <- data.frame()
  
  #For loop for finding outliers
  for (i in 1:length(dat_list)) {
    thing <- dat_list[[i]]
    name <- as.character(thing[1,"Tooth"])
    findoutlier <- function(x){
      return(x < quantile(x,0.1, na.rm = TRUE) | quantile(x,0.9, na.rm = TRUE) < x)
    }
   
   #Run the LOWESS, find outliers, and split it by individual and tooth
    lo <- loess(thing$Perikymata ~ thing$Length, span = 0.1)
    lo.res <- as.data.frame(lo$residuals)
    lo.res$Perikymata <- seq.int(nrow(lo.res))
    res.data <- merge(thing,lo.res,by = "Perikymata")
    names(res.data)[names(res.data) == "lo$residuals"] <- "Residuals"
    res.data <- res.data %>% 
      group_by(Tooth) %>% 
      mutate(outlier = ifelse(findoutlier(Residuals), Perikymata, NA))
    
    #Separates outliers and saves upper outliers
    out_list <- subset(res.data, !is.na(outlier))
    upper_out_list <- subset(out_list, out_list$Residuals > 0)
    
    #Saves the upper outliers from each iteration into one dataframe
    LEH_list = rbind(LEH_list, upper_out_list)
    
    #Make the boxplots
    g <- ggplot(res.data,aes(x=factor(0),y=Residuals)) + stat_summary(fun.data = f, geom = "boxplot") + stat_summary(fun = o, geom = "point")+ labs(x= name, y= "LOWESS Residuals") + ggtitle(name) + theme(plot.title = element_text(hjust = 0.5)) + geom_text_repel(max.overlaps = 25, label.padding = 0.4, point.padding = 0.5, aes(label=outlier), na.rm = TRUE, nudge_x = c(-1,1), direction = "y", hjust = -0.5) 
    print(g)
  }  
  dev.off()

  #Part 2: Calculating the age-at-defect values for each event

  #Defining tooth categories
  MaxI1 <- c("RMax1","LMax1")
  MaxI2 <- c("RMax2","LMax2")
  MaxC <- c("RCU","LCU")
  MandI1 <- c("RMand1","LMand1")
  MandI2 <- c("RMand2","LMand2")
  MandC <- c("RCL","LCL")
  
  #Reading in default values
  #Ohio values from Blatt 2013, Southern Africa and Northern Europe from Reid and Dean 2006
  tooth <- c("MaxI1","MaxI2","MaxC","MandI1","MandI2","MandC")
  
  #Ohio Risnes Correction
  time_OR <- c(99.44,162.91,251.32,129.85,250.69,165.89)
  correction_OR <- c(32.9,85.8,78.2,62.8,23.9,78.3)
 
   #Ohio Mahoney Correction
  time_OM <- c(90.79,148.74,229.47,118.56,219.85,151.47)
  correction_OM <- c(30.1,78.4,71.4,57.3,33.5,71.5)
  
  #Southern Africa Risnes Correction
  time_SNA <- c(284,283,339,214,223,327)
  correction_SNA <- c(18,19,26,23,20,21)
  
  #Northern Europe Risnes Correction
  time_NNE <- c(289,274,355,256,212,348)
  correction_NNE <- c(16,16,24,21,21,28)
  
  #Constructing the Reference Dataframes
  Ohio_R <- data.frame(tooth, time_OR, correction_OR)
  colnames(Ohio_R) <- c("Tooth","Time","Correction")
  Ohio_M <- data.frame(tooth, time_OM, correction_OM)
  colnames(Ohio_M) <- c("Tooth","Time","Correction")
  Southern_Africa_R <- data.frame(tooth, time_SNA, correction_SNA)
  colnames(Southern_Africa_R) <- c("Tooth","Time","Correction")
  Northern_Europe_R <- data.frame(tooth, time_NNE, correction_NNE)
  colnames(Northern_Europe_R) <- c("Tooth","Time","Correction")
  
  #Defining CpFT
  if(CpFT_ref=="Ohio_R") {
    CpFT <- Ohio_R
  }
  if(CpFT_ref=="Ohio_M") {
    CpFT <- Ohio_M
  }
  if(CpFT_ref=="Southern_Africa_R") {
    CpFT <- Southern_Africa_R
  }
  if(CpFT_ref=="Northern_Europe_R") {
    CpFT <- Northern_Europe_R
  }
  
  #Split LEH List Individual Name and Specific Tooth
  LEH_list[c('Site','Individual','Tooth')] <- str_split_fixed (LEH_list$Tooth," +",3)
  
  #Make an original tooth category
  LEH_list <- LEH_list %>% mutate(Tooth_Name = Tooth)
  
  #Transform Tooth to standard format
  LEH_list$Tooth <- revalue(LEH_list$Tooth, c("LCL"="MandC", "RCL"="MandC","LCU"="MaxC","RCU"="MaxC", "LMand1"="MandI1","RMand1"="MandI1","LMand2"="MandI2","RMand2"="MandI2","LMax1"="MaxI1","RMax1"="MaxI1","LMax2"="MaxI2","RMax2"="MaxI2"
  ))
  
  #Add CpFT to LEH List
LEH_list <- merge(LEH_list, CpFT, by="Tooth")
LEH_list <- arrange(LEH_list,Site,Individual,Tooth_Name)
  
  #Create blank dataframe
  Stress_Events <- data.frame()
  
  #For loop to calculate age at defect
  for (i in 1:nrow(LEH_list)) {
    Individual <- LEH_list[i,"Individual"]
    Tooth_Name <- LEH_list[i,"Tooth_Name"]
    LEH <- LEH_list[i,"Perikymata"]
    CpFT_tooth <- LEH_list[i,"Time"]
    Correction <- LEH_list[i,"Correction"]
    LEH_days <- (LEH*8)+CpFT_tooth
    LEH_years <- LEH_days/365
    LEH_upper_error_days <- LEH_days + Correction
    LEH_upper_error_years <- LEH_upper_error_days/365
    LEH_lower_error_days <- LEH_days - Correction
    LEH_lower_error_years <- LEH_lower_error_days/365
    output <- c(Individual,Tooth_Name, LEH_days,LEH_years,LEH_lower_error_days,LEH_upper_error_days,LEH_lower_error_years,LEH_upper_error_years)
    Stress_Events <- rbind(Stress_Events, output)
    colnames(Stress_Events) <- c("Individual","Tooth","Age in Days at Stress Event","Age in Years at Stress Event","Lower Range Age in Days at Stress Event","Upper Range Age in Days at Stress Event", "Lower Range Age in Years at Stress Event","Upper Range Age in Years at Stress Event")

    #Make Stress_Events columns numeric
    Stress_Events <- Stress_Events %>% 
      mutate_at(vars(`Age in Days at Stress Event`,`Age in Years at Stress Event`,`Lower Range Age in Days at Stress Event`,`Lower Range Age in Years at Stress Event`,`Upper Range Age in Days at Stress Event`,`Upper Range Age in Years at Stress Event`), as.numeric)
    #Round to three decimal places
    Stress_Events <- Stress_Events %>% mutate_if(is.numeric, round, digits = 3)
  }
  
  #Save the output as a csv
  write.csv(Stress_Events, paste(Sys.Date(),"LEH Event Age Data.csv"))
}
