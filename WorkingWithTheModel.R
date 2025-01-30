library(ncdf4)
# Install and load required libraries
# install.packages("rasterVis")
# install.packages("rgl")  # For interactive 3D plots
library(rasterVis)
library(raster)
library(rgl)
#install.packages("reticulate")
library(reticulate)
#reticulate::install_miniconda()
#reticulate::use_miniconda("r-reticulate", required = TRUE)
#reticulate::py_install("tensorflow")
library(keras)
library(tensorflow)
library(caret)
library(lattice)

#dev.off()

Simtoplot <- 1
extended <- 50

min_val_scaling <- -10
max_val_scaling <- 1307

base_model_path <- "MODELPATH"
file_path <- "PLOTPATH"
setwd("PATH TO DATA")


model_name <- "5Conv0Dense5Features_MSE_LR00001_5EP_slope_windaspect_10Epochs_64Batch_50BUFF_RotaCorr_2"








  #### Path setting ####

  model_path <- file.path(base_model_path, model_name)
 
  model_name <- sub(".*/(.+)_5EP", "\\1", model_name)
  file_path <- file.path(file_path, model_name)
  #dir.create(file_path)
  
  
  
  #### Functions ####
  
  min_max_scaler_W <- function(x, min_val, max_val) {
    scaled <- (x - min_val) / (max_val - min_val)
    return(scaled)
  }
  
  PlotDSWEwithModel <- function(Simtoplot, file_path, data_list_test, normalized_vector, model, extended) {
    
    
    
    #### Split into features and Labels ####
    
    Features <- array(0, dim = c(1,dim(data_list_test[[1]]$terrain)[1],dim(data_list_test[[1]]$terrain)[1],5))
    
    Features[1,,,] <- normalized_vector[Simtoplot,,,1:5]
    Labels  <- Testing[,,,6] 
    Labels <- Labels[Simtoplot,,]
    
    
    #### Predict ####
    
    predictions <- model %>% predict(Features)
    predictions <- predictions[,,,]
    dswe <- predictions[extended:(dim(data_list_test[[1]]$terrain)[1]-extended-1),extended:(dim(data_list_test[[1]]$terrain)[1]-extended-1)]
    predictions <- predictions[extended:(dim(data_list_test[[1]]$terrain)[1]-extended-1),extended:(dim(data_list_test[[1]]$terrain)[1]-extended-1)]
    
    
    # Outlier Eliminiation
    
    
    # Define min and max values for your z-axis
    #min_value <- min(Testing[,,,6])
    #max_value <- max(Testing[,,,6])
    
    min_value <- min(dswe)
    max_value <- max(dswe)
    
    
    # Set values above max_value to max_value
    #predictions[predictions > max_value] <- max_value
    
    # Set values below min_value to min_value
    #predictions[predictions < min_value] <- min_value
    
    #dswe <- predictions
    
    #### Prepare the Plot ####
    
    terrain <- Testing[Simtoplot,extended:(dim(data_list_test[[1]]$terrain)[1]-extended-1),extended:(dim(data_list_test[[1]]$terrain)[1]-extended-1),1]
    
    
    dd = data_list_test[[Simtoplot]]$dd
    
    # Convert wind direction to radians
    wind_direction_rad <- (90 - dd) * (pi / 180)
    
    # Define arrow coordinates
    arrow_x <- 205  # X-coordinate for the arrow
    arrow_y <- 230  # Y-coordinate for the arrow
    arrow_length <- 20  # Length of the arrow
    
    # Calculate arrowhead coordinates
    arrowhead_x <- arrow_x + arrow_length * cos(wind_direction_rad)
    arrowhead_y <- arrow_y + arrow_length * sin(wind_direction_rad)
    
    # Plot Describtion Generation
    
    
    SimuUsed <- data_list_test[[i]]$SimuUsed
    
    angle <- data_list_test[[i]]$angle
    
    if (data_list_test[[i]]$Original == 1)
      
    {
      IsOriginal <- "Original"
      
    } else {
      
      IsOriginal <- "Rotated"
      
    }
    
    #### Create the actual Plot! ####
    
    # Create a color palette ranging from blue to white to red
    custom_palette <- colorRampPalette(c("blue", "lightblue", "white", "lightpink", "red"))
    
    
    # Center the z-axis on zero
    zlim = c(-max(abs(min_value), abs(max_value)), max(abs(min_value), abs(max_value)))
    
    
    
    
    file_path <- paste(file_path,"/Prediction",Simtoplot,".png")
    file_path <- gsub(" ", "", file_path)  # Remove spaces
    
    png(file_path, width = 1200, height = 800)
    
    

    
    
    filled.contour(
      x = 1:255,
      y = 1:255,
      z = dswe,
      color.palette = custom_palette,
      plot.title = {par(cex.main=1.4);title(main = paste("Simulation:",SimuUsed,", dswe Estimation by CNN, Winddirection:",dd,",",IsOriginal,"Angle:",angle),
                                          xlab = "Units West", ylab = "Units North")},
      key.title = {par(cex.main=0.6);title(main = "kg m-2 s-1")},
      plot.axes = {
        axis(1)
        axis(2)
        contour(x = 1:255, y = 1:255, z = terrain, add = TRUE, col = rgb(0, 0, 0, alpha = 0.5))
      },
      xlim = c(1, 255),
      ylim = c(1, 255),
      zlim = zlim,
      nlevels = 25
    )
    arrows(arrow_x, arrow_y, arrowhead_x, arrowhead_y, length = 0.1, angle = 30, col = "red", lwd = 2)
    
    
    dev.off()
    
    return(list(predictions = predictions, Lables = Labels))
  }
  
  #### Data Reading ####
  
  data_list_test <- data_list[otherIndices]
  
  Testing <- Test_Data
  
  normalized_vector <- min_max_scaler_W(Test_Data[,,,c(1:5)], min_val_scaling, max_val_scaling)
  
  #### Model Loading ####
  
  new_model <- load_model_tf(model_path)
  
  
  
  #Data <- PlotDSWEwithModel(Simtoplot, file_path, data_list_test, normalized_vector, new_model)
  
  
  evaluation <- new_model %>% evaluate(Test_Data_Features, Test_Data_Labels)
  
  
  
  for (i in seq(1, dim(Testing)[1], by = 1)) 
    
  {
    
    Data <- PlotDSWEwithModel(i, file_path, data_list_test, normalized_vector, new_model,extended)
    
  }
  
  
  
  ### Save Model Summary ###
  
  file_pathtxt <- paste(file_path, "/summary", Simtoplot, ".txt")
  file_pathtxt <- gsub(" ", "", file_pathtxt)  # Remove spaces
  
  model_summary <- capture.output(summary(new_model))
  evaluation_summary <- capture.output(evaluation)
  
  sink(file_pathtxt, append = FALSE)
  
  # Print the model summary to the file
  cat(model_summary, sep = "\n")
  
  # Print the evaluation summary to the file
  cat("\n\nEvaluation Summary:\n", evaluation_summary, sep = "\n")
  
  # Stop sinking the output to the file
  sink()
  
