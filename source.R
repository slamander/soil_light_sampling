sampling <- function(type = "soil"){
  
  library(tidyverse)
  
  sampling <- data.frame(
  plot = seq(1,120,by=1),
  block = factor(rep(1:10, each=12)),
  dist = NA
  )
  
  disturbed <- c(1,2,4,7,10,12,13,15,16,18,23,24,25,27,28,32,33,35,
               37,40,44,45,46,47,52,53,56,57,58,60,62,65,66,67,68,
               71,75,76,77,79,82,83,85,86,87,90,92,93,97,100,101,
               102,103,107,112,114,115,117,118,120)
  
  sampling$dist[sampling$plot %in% disturbed] <- 1
  sampling$dist[is.na(sampling$dist)] <- 0
  
  sampling_list <- split(sampling, sampling$block)
  
  #####################################################################
  ####################### sampling for light ##########################
  #####################################################################
  if(type == "light"){
    light_sampling <- list()
    
    for(i in 1:length(sampling_list)){
      block <- sample(sampling_list[[i]])
      light_sampling[[paste("Block: ", i)]] <- data.frame(
        dist_plot = sample(block$plot[block$dist ==1], size=1),
        undist_plot = sample(block$plot[block$dist ==0], size=1)
      )
    }
    return(light_sampling)
  }
  
  #####################################################################
  ####################### sampling for soil ###########################
  #####################################################################
  if(type == "soil"){
    soil_sampling <- list()
    
    for(i in 1:length(sampling_list)){
      block <- sample(sampling_list[[i]])
      soil_sampling[[paste("Block: ", i)]] <- data.frame(
        dist_plot = sample(block$plot[block$dist ==1], size=3, replace=F),
        undist_plot = sample(block$plot[block$dist ==0], size=3, replace=F)
        )
    }
    return(soil_sampling)
  }
}



