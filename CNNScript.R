#install.packages("reticulate")
#reticulate::install_miniconda()
#reticulate::use_miniconda("r-reticulate", required = TRUE)
#reticulate::py_install("tensorflow")

#reticulate::py_module_available("tensorflow")
#reticulate::py_module_available("keras")s

##### INSTALL LIBRARIES #####

# Function to install and activate libraries
install_and_activate_libraries <- function(libraries) {
  
  is_library_installed <- function(lib) {
    return(lib %in% installed.packages())
  }
  
  
  # Loop through the libraries list and install any missing libraries
  for (lib in libraries) {
    if (!is_library_installed(lib)) {
      install.packages(lib)
    }
  }
  
  # Load the libraries
  for (lib in libraries) {
    if (is_library_installed(lib)) {
      library(lib, character.only = TRUE)
      cat(sprintf("%s is installed and activated.\n", lib))
    } else {
      cat(sprintf("%s could not be installed.\n", lib))
    }
  }
}

# Example usage:
libraries <- c(
  "ncdf4",
  "keras",
  "tensorflow",
  "caret",
  "progress"
)

install_and_activate_libraries(libraries)

#install_tensorflow()


#SETWD TO FOLDER WHERE TRAININGDATA IS LOCATED

setwd("C:/Users/lukas/OneDrive/Studies/Studium/Bachelorarbeit/Coding/Datensatz/Trainingsdaten")


set.seed(123)



##### Functions #####

nc_reading <- function(base_filename) {
  
  
  # Create an empty list to store the NetCDF file objects
  nc_list <- list()
  
  # Loop through the file numbers and read in the corresponding files
  for (i in 1:72) {
    # Generate the complete file name
    current_filename <- paste0(base_filename, sprintf("%02d", i), ".nc")
    
    # Use nc_open to open the NetCDF file and store the object in the list
    nc <- nc_open(current_filename)
    
    # Append the NetCDF file object to the list
    nc_list[[i]] <- nc
  }
  return(nc_list)
}

read_variables_from_nc_with_rotation <- function(nc_list) {
  # Create an empty list to store the data
  data_list <- list()
  angles <- c(0, 90, 180, 270)
  
  rotate_matrix <- function(original_matrix, degrees) {
    
    
    rotate <- function(x) t(apply(x, 2, rev))
    
    if (degrees == 0) {
      rotated_matrix <- original_matrix
    } else if (degrees == 90) {
      rotated_matrix <- rotate(original_matrix)
    } else if (degrees == 180) {
      rotated_matrix <- rotate(rotate(original_matrix))
    } else if (degrees == 270) {
      rotated_matrix <- rotate(rotate(rotate(original_matrix)))
    } else {
      stop("Invalid degrees. Supported values are 0, 90, 180, and 270.")
    }
    
    return(rotated_matrix)
  }
  calculate_wind_components <- function(ff, dd) {
    # Convert wind direction from degrees to radians
    dd_rad <- dd * (pi / 180)
    
    ZweihundertsiebziginRad <- 270 * (pi / 180) 
    
    # Calculate u and v components
    u <- ff * cos(ZweihundertsiebziginRad - dd_rad)
    v <- ff * sin(ZweihundertsiebziginRad - dd_rad)
    
    # Return the results
    return(list(u = u, v = v))
  }
  calculate_aspect_cosine <- function(heights_matrix, global_wind_direction) {
    # Calculate gradient in x direction
    dx <- heights_matrix[, c(2:ncol(heights_matrix), ncol(heights_matrix))] -
      heights_matrix[, c(1, 1:(ncol(heights_matrix) - 1))]
    
    # Calculate gradient in y direction
    dy <- heights_matrix[c(2:nrow(heights_matrix), nrow(heights_matrix)),] -
      heights_matrix[c(1, 1:(nrow(heights_matrix) - 1)),]
    
    # Calculate aspect in radians
    aspect_rad <- atan2(dy, dx)
    
    # Convert aspect to degrees
    aspect_deg <- (90 - (aspect_rad * (180 / pi))) %% 360
    
    # Calculate the cosine of the angle difference between slope aspect and global wind direction
    angle_difference <- (aspect_deg - global_wind_direction) %% 360
    aspect_cosine <- cos(angle_difference * pi / 180)
    
    return(aspect_cosine)
  }
  calculate_slope <- function(heights_matrix) {
    # Calculate gradient in x direction
    dx <- (heights_matrix[, c(2:ncol(heights_matrix), ncol(heights_matrix))] -
             heights_matrix[, c(1, 1:(ncol(heights_matrix) - 1))]) / 2
    
    # Calculate gradient in y direction
    dy <- (heights_matrix[c(2:nrow(heights_matrix), nrow(heights_matrix)),] -
             heights_matrix[c(1, 1:(nrow(heights_matrix) - 1)),]) / 2
    
    # Combine gradients to get slope
    slope <- sqrt(dx^2 + dy^2)
    
    return(slope)
  }
  extendMatrix <- function(matrix, extension_size) {
    
    # Add the left rows to the right
    extended_matrix <- rbind(matrix, matrix[1:extension_size, ])
    
    # Add the right rows to the left
    extended_matrix <- rbind(matrix[(nrow(matrix)-(extension_size-1)):(nrow(matrix)), ], extended_matrix)
    
    # Add the bottom columns to the top
    extended_matrix <- cbind(extended_matrix, extended_matrix[, 1:extension_size])
    
    # Add the right columns to the left side
    extended_matrix <- cbind(extended_matrix[, rev((ncol(extended_matrix)-(extension_size)):
                            ((ncol(extended_matrix)-(extension_size-1))-extension_size))], extended_matrix)
    # Print the extended matrix
    return(extended_matrix)
    
  }
  
  
  for (angle in angles) {
    for (i in seq_along(nc_list)) {
      
      
      # Read specific variables from the NetCDF file
      terrain <- rotate_matrix(ncvar_get(nc_list[[i]], varid = "ter"),angle)
      dswe <- rotate_matrix(ncvar_get(nc_list[[i]], varid = "dswe"),angle)
      dd_input <- ncvar_get(nc_list[[i]], varid = "dd_input")
      ff_input <- ncvar_get(nc_list[[i]], varid = "ff_input")
      
      dd_input <- (dd_input + angle) %% 360
      
      # Calculate Wind Components
      result <- calculate_wind_components(ff_input, dd_input)
      
      
      if(angle==0){
      
      # Create a data frame or list to store the variables
      file_data <- list(
        terrain = extendMatrix(terrain,50),
        u = extendMatrix(matrix(result$u, 255,255),50),
        v = extendMatrix(matrix(result$v, 255,255),50),
        slope = extendMatrix(calculate_slope(terrain),50),
        windaspect = extendMatrix(calculate_aspect_cosine(terrain, dd_input),50),
        dswe = extendMatrix(dswe,50),
        dd_input = dd_input,
        Original = 1,
        SimuUsed = i,
        angle = angle
      )
      } else {
        
        file_data <- list(
          terrain = extendMatrix(terrain,50),
          u = extendMatrix(matrix(result$u, 255,255),50),
          v = extendMatrix(matrix(result$v, 255,255),50),
          slope = extendMatrix(calculate_slope(terrain),50),
          windaspect = extendMatrix(calculate_aspect_cosine(terrain, dd_input),50),
          dswe = extendMatrix(dswe,50),
          dd_input = dd_input,
          Original = 0,
          SimuUsed = i,
          angle = angle
        )
        
        
      }
      
      # Append the data to the list
      data_list[[length(data_list) + 1]] <- file_data
      
      
      
      
      
    }
  }
  return(data_list)
}

##### Deprecated #####

read_variables_from_nc_with_rotation_POIZOOM <- function(nc_list) {
  # Create an empty list to store the data
  data_list <- list()
  angles <- c(0, 90, 180, 270)
  
  pb <- progress_bar$new(
    format = "[:bar] :percent Elapsed: :elapsed Time remaining: :eta",
    total = length(angles) * length(nc_list),
    clear = FALSE
  )
  
  
  zoomPOI <- function(matrix) {
    
    means <- numeric()  # Create an empty numeric array to store means
    submatrices <- list()  # Create an empty list to store submatrices
    start_rows <- numeric()  # Create an empty numeric array to store start rows
    start_cols <- numeric()  # Create an empty numeric array to store start columns
    
    step_size <- 10
    
    for (i in seq(1, nrow(matrix) - 50 + 1, by = step_size)) {
      for (j in seq(1, ncol(matrix) - 50 + 1, by = step_size)) {
        submatrix <- matrix[i:(i + 50), j:(j + 50)]
        average <- mean(submatrix)
        means <- c(means, average)  # Append the mean to the array
        submatrices[[length(means)]] <- submatrix  # Store the submatrix in the list
        start_rows <- c(start_rows, i)  # Store the start row
        start_cols <- c(start_cols, j)  # Store the start column
      }
    }
    
    max_mean_index <- which.max(means)  # Find the index of the maximum mean
    min_mean_index <- which.min(means)  # Find the index of the minimum mean
    
    result <- list(
      max_start_row = start_rows[max_mean_index],
      max_start_col = start_cols[max_mean_index],
      min_start_row = start_rows[min_mean_index],
      min_start_col = start_cols[min_mean_index]
    )
    
    return(result)
  }
  rotate_matrix <- function(original_matrix, degrees) {
    if (degrees == 0) {
      rotated_matrix <- original_matrix
    } else if (degrees == 90) {
      rotated_matrix <- t(original_matrix[, ncol(original_matrix):1])
    } else if (degrees == 180) {
      rotated_matrix <- t(original_matrix[nrow(original_matrix):1, ncol(original_matrix):1])
    } else if (degrees == 270) {
      rotated_matrix <- t(original_matrix[nrow(original_matrix):1, ])
    } else {
      stop("Invalid degrees. Supported values are 0, 90, 180, and 270.")
    }
    
    return(rotated_matrix)
  }
  zoom_matrix <- function(original_matrix, start_row, start_col) {
    
    upscale_factor <- 5
    
    selected_part <- original_matrix[start_row:(start_row + 50), start_col:(start_col + 50)]
    
    # Upscale the selected part to a 255x255 matrix using kronecker
    upscaled_part <- kronecker(selected_part, matrix(1, nrow = upscale_factor, ncol = upscale_factor))
    
    # Return the upscaled part
    return(upscaled_part)
  }
  calculate_wind_components <- function(ff, dd) {
    # Convert wind direction from degrees to radians
    dd_rad <- dd * (pi / 180)
    
    ZweihundertsiebziginRad <- 270 * (pi / 180) 
    
    # Calculate u and v components
    u <- ff * cos(ZweihundertsiebziginRad - dd_rad)
    v <- ff * sin(ZweihundertsiebziginRad - dd_rad)
    
    # Return the results
    return(list(u = u, v = v))
  }
  
  
  
  for (angle in angles) {
    for (i in seq_along(nc_list)) {
      
      #cat("Processing NC file:", i, "in ANgle", angle, "\n")
      pb$tick()
      
      # Read specific variables from the NetCDF file
      terrain <- rotate_matrix(ncvar_get(nc_list[[i]], varid = "ter"),angle)
      dswe <- rotate_matrix(ncvar_get(nc_list[[i]], varid = "dswe"),angle)
      phiflux_u_vertint <- rotate_matrix(ncvar_get(nc_list[[i]], varid = "phiflux_u_vertint"),angle)
      phiflux_v_vertint <- rotate_matrix(ncvar_get(nc_list[[i]], varid = "phiflux_v_vertint"),angle)
      um_10 <- rotate_matrix(ncvar_get(nc_list[[i]], varid = "um_10"),angle)
      vm_10 <- rotate_matrix(ncvar_get(nc_list[[i]], varid = "vm_10"),angle)
      subl_vertint <- rotate_matrix(ncvar_get(nc_list[[i]], varid = "subl_vertint"),angle)
      dd_input <- ncvar_get(nc_list[[i]], varid = "dd_input")
      ff_input <- ncvar_get(nc_list[[i]], varid = "ff_input")
      
      dd_input <- (dd_input + angle) %% 360
      
      # Calculate Wind Components
      result <- calculate_wind_components(ff_input, dd_input)
      
      # Create a data frame or list to store the variables
      file_data <- list(
        terrain = terrain,
        phiflux_u_vertint = phiflux_u_vertint,
        phiflux_v_vertint = phiflux_v_vertint,
        um_10 = um_10,
        vm_10 = vm_10,
        subl_vertint = subl_vertint,
        u = result$u,
        v = result$v,
        dswe = dswe
      )
      
      # Append the data to the list
      data_list[[length(data_list) + 1]] <- file_data
      
      
      POI <- zoomPOI(dswe)
      
      start_row <- POI$max_start_row
      start_col <- POI$max_start_col
      
      file_data_zoom_max <- list(
        terrain = zoom_matrix(terrain, start_row, start_col),
        phiflux_u_vertint = zoom_matrix(phiflux_u_vertint, start_row, start_col),
        phiflux_v_vertint = zoom_matrix(phiflux_v_vertint, start_row, start_col),
        um_10 = zoom_matrix(um_10, start_row, start_col),
        vm_10 = zoom_matrix(vm_10, start_row, start_col),
        subl_vertint = zoom_matrix(subl_vertint, start_row, start_col),
        u = result$u,
        v = result$v,
        dswe = zoom_matrix(dswe, start_row, start_col)
      )
      data_list[[length(data_list) + 1]] <- file_data_zoom_max
      
      
      
      start_row <- POI$min_start_row
      start_col <- POI$min_start_col
      
      file_data_zoom_min <- list(
        terrain = zoom_matrix(terrain, start_row, start_col),
        phiflux_u_vertint = zoom_matrix(phiflux_u_vertint, start_row, start_col),
        phiflux_v_vertint = zoom_matrix(phiflux_v_vertint, start_row, start_col),
        um_10 = zoom_matrix(um_10, start_row, start_col),
        vm_10 = zoom_matrix(vm_10, start_row, start_col),
        subl_vertint = zoom_matrix(subl_vertint, start_row, start_col),
        u = result$u,
        v = result$v,
        dswe = zoom_matrix(dswe, start_row, start_col)
      )
      data_list[[length(data_list) + 1]] <- file_data_zoom_min
      
      
      
      
    }
  }
  pb$terminate()
  
  return(data_list)
}

######################

Wait <- function(wait_time = 5) {
  cat("Waiting: [", sep = "", flush = TRUE)
  
  for (i in 1:wait_time) {
    Sys.sleep(1)
    
    # Update progress bar
    cat("#", sep = "", flush = TRUE)
  }
  
  cat("] Done!\n", sep = "")
}

unlistData <- function (data_list) {
  
  size_X_y <- dim(data_list[[1]]$terrain)[1]
  
  combined <- array(0, dim = c(length(data_list), size_X_y,size_X_y,6))
  
  for (i in seq_along(data_list)) 
    
  {
    ter <- data_list[[i]]$terrain
    u <- data_list[[i]]$u
    v <- data_list[[i]]$v
    slope <- data_list[[i]]$slope
    windaspect <- data_list[[i]]$windaspect
    dswe <- data_list[[i]]$dswe
    
    zsm <- array(c(ter,u,v,slope,windaspect, dswe), dim = c(size_X_y,size_X_y,6))
    
    combined[i,,,] <- zsm
    
    cat("\rUnlisting Progress: ", sprintf("%.2f%%", (i / length(data_list)) * 100))
    flush.console()
  }
  
  cat("\n")
  return(combined)
  
}

min_max_scaler <- function(x) {
  min_val <- min(x)
  max_val <- max(x)
  scaled <- (x - min_val) / (max_val - min_val)
  return(scaled)
}





#### Read in Data and prepare
nc_list <- nc_reading("wrfout_extract_attr_sim_")

data_list <- read_variables_from_nc_with_rotation(nc_list)

gc()

CombinedforTesting <- unlistData(data_list)

gc()

Wait(2)

##### Split to Training and Test Data ####

trainpercentage <- 0.9

n <- round(dim(CombinedforTesting)[1] * trainpercentage)

# Generate random indices
Indices <- sample(seq_len(dim(CombinedforTesting)[1]), size = n, replace = FALSE)

# Select all other indices
otherIndices <- setdiff(seq_len(dim(CombinedforTesting)[1]), Indices)


Train_Data <-CombinedforTesting[Indices, , ,] 
Test_Data <- CombinedforTesting[otherIndices, , ,]

gc()

Train_Data_Labels <- Train_Data[,,,6]
Train_Data_Features <- min_max_scaler(Train_Data[,,,c(1:5)])

gc()

Test_Data_Labels <- Test_Data[,,,6]
Test_Data_Features <- min_max_scaler(Test_Data[,,,c(1:5)])

rm (trainpercentage, n)

gc()

Wait(2)

##### Define and Train the Model #####


model <- keras_model_sequential() %>%
  layer_conv_2d(filters = 32, kernel_size = c(3, 3), activation = 'relu', input_shape = c(dim(Train_Data)[2],dim(Train_Data)[2],5), padding = 'same') %>%
  layer_conv_2d(filters = 64, kernel_size = c(3, 3), activation = 'relu', padding = 'same') %>%
  layer_conv_2d(filters = 128, kernel_size = c(3, 3), activation = 'relu', padding = 'same') %>%
  layer_conv_2d(filters = 256, kernel_size = c(3, 3), activation = 'relu', padding = 'same') %>%
  layer_conv_2d(filters = 1, kernel_size = c(3, 3), activation = 'linear', padding = 'same')
#layer_dense(units = 128, activation = 'relu') %>%
#layer_dense(units = 128, activation = 'relu') %>%
  #layer_dense(units = 64, activation = 'relu') %>%
  #layer_dense(units = 32, activation = 'relu') %>%
#layer_dense(units = 1 , activation = 'linear', kernel_initializer = 'glorot_uniform')
  #layer_dense(units = 1 , activation = 'linear')# Output layer for regression (1 unit for a single continuous value)


optimizer = optimizer_nadam()

#loss functions: https://neptune.ai/blog/keras-loss-functions

model %>% compile(
  loss = "mean_squared_error",  # Use mean squared error for regression
  optimizer = optimizer,
  metrics = c('mean_squared_logarithmic_error', 'mean_absolute_error', 'mean_squared_error')  # Use mean absolute error as a metric
)

gc()

### Optimzers: adam, nadam etc

# Batch Size: https://machinelearningmastery.com/use-different-batch-sizes-training-predicting-python-keras/

model %>% fit(
  Train_Data_Features, Train_Data_Labels,
  epochs = 10,
  batch_size = 64,
  validation_split = 0.2,
  # callbacks = list(tensorboard_callback)
)






 
gc()

evaluation <- model %>% evaluate(Test_Data_Features, Test_Data_Labels)

print(evaluation)


# SPECIFY WHERE MODEL SHOULD BE SAVED

save_model_tf(model,"PATH")

