


extendMatrix <- function(matrix, extension_size) {

  # Add the left rows to the right
  extended_matrix <- rbind(matrix, matrix[1:extension_size, ])
  
  # Add the right rows to the left
  extended_matrix <- rbind(matrix[(nrow(matrix)-(extension_size-1)):(nrow(matrix)), ], extended_matrix)
  
  # Add the bottom columns to the top
  extended_matrix <- cbind(extended_matrix, extended_matrix[, 1:extension_size])
  
  # Add the right columns to the left side
  extended_matrix <- cbind(extended_matrix[, rev((ncol(extended_matrix)-(extension_size)):((ncol(extended_matrix)-(extension_size-1))-extension_size))], extended_matrix)
  # Print the extended matrix
  return(extended_matrix)

}




matrix <- ncvar_get(nc_list[[1]],varid = "dswe")

extented <- extendMatrix(matrix,50)

filled.contour(extented)



rota <- ncvar_get(nc_list[[10]], varid = "dswe")

rotate <- function(x) t(apply(x, 2, rev))



filled.contour(rota)

filled.contour(rotate(rota))

filled.contour(rotate(rotate(rota)))

filled.contour(rotate(rotate(rotate(rota))))

