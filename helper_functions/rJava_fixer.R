# Check if rJava is already installed
if (!requireNamespace("rJava", quietly = TRUE)) {
  
  # Try to set the JAVA_HOME environment variable
  java_home <- Sys.getenv("JAVA_HOME")
  if (java_home == "" || !dir.exists(file.path(java_home, "jre", "lib", "amd64"))) {
    java_home <- "/usr/lib/jvm/java"
  }
  Sys.setenv(JAVA_HOME = java_home)
  
  # Try to load the rJava package again
  if (!requireNamespace("rJava", quietly = TRUE)) {
    
    # Try to add the location of libjvm.so to LD_LIBRARY_PATH
    java_path <- file.path(java_home, "jre", "lib", "amd64", "server")
    if (dir.exists(java_path)) {
      Sys.setenv(LD_LIBRARY_PATH = paste0(java_path,":", Sys.getenv("LD_LIBRARY_PATH")))
    } else {
      stop("Unable to locate libjvm.so.")
    }
    
    # Try to install rJava from CRAN or GitHub
    if (!requireNamespace("devtools", quietly = TRUE)) {
      install.packages("devtools")
    }
    success <- FALSE
    for (repo in c("rforge/rJava", "cran/rJava")) {
      tryCatch({
        if (devtools::install_github(repo, dependencies = FALSE)) {
          success <- TRUE
          break
        }
      }, error = function(e) {
        message(paste0("Failed to install rJava package from '", repo, "': ", e$message))
      })
      if (success) {
        break
      }
    }
    if (!success) {
      stop("Failed to install rJava package from any available repository.")
    }
  }
}
