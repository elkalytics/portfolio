# Combine all lists into a single list

combine_lists <- function() {
  # Get all the objects in the environment that are lists
  lists <- Filter(is.list, mget(ls()))
  
  # Merge the lists into a single list of lists
  combined_list <- do.call(c, lists)
  
  return(combined_list)
}

