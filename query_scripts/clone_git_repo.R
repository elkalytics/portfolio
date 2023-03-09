#' Clone a Git repository
#'
#' This function clones a Git repository from a specified URL to a specified local path.
#'
#' @param repo_url The URL of the repository to clone.
#' @param local_path The local path where the repository will be cloned.
#'
#' @return A git2r repository object if successful, or an error message if not.
#'
#' @examples
#' # Clone a repository to a local path
#' repo_url <- "https://github.com/username/repo-name.git"
#' local_path <- "/path/to/local/folder"
#' clone_git_repo(repo_url, local_path)
#'
#' @import git2r
#' @export
clone_git_repo <- function(repo_url, local_path) {
  # Load the git2r package
  library(git2r)
  
  # Check if the local path already exists and is a directory
  if (file.exists(local_path) && !is.dir(local_path)) {
    cat("Error: The local path specified is a file and not a directory.\n")
  } else if (dir.exists(local_path)) {
    # Check if the local path is empty or not
    if (!is.empty(local_path)) {
      cat("Error: The local path specified already exists and is not empty.\n")
    } else {
      # Clone the repository
      repo <- clone(repo_url, local_path)
      
      # Check if the repository was cloned successfully
      if (exists("repo")) {
        cat("Repository cloned successfully!\n")
      } else {
        cat("Error: Repository cloning failed.\n")
      }
    }
  } else {
    # Create the local directory
    dir.create(local_path)
    
    # Clone the repository
    repo <- clone(repo_url, local_path)
    
    # Check if the repository was cloned successfully
    if (exists("repo")) {
      cat("Repository cloned successfully!\n")
    } else {
      cat("Error: Repository cloning failed.\n")
    }
  }
}