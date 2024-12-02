library(here)
library(yaml)

error_code_path <- here("inst/config/error_codes.yaml")

#' @title Load Error Codes
#'
#' @description Loads the error codes from the YAML configuration file in the `inst/config/` directory.
#' 
#' @return A list of error codes loaded from the YAML file.
#' @examples
#' \dontrun{
#'   error_codes <- load_error_codes()
#'   print(error_codes)
#' }
#' @export
load_error_codes <- function() {
    # Locate the error_codes.yaml file
    yaml_path <- system.file(error_code_path, package = "DBSCAN615")
    if (yaml_path == "") {
        stop("Error codes configuration file not found.", call. = FALSE)
    }
    # Read and return the YAML content as a list
    yaml::read_yaml(yaml_path)
}


#' @title Get Error Message
#'
#' @description Retrieves the error message corresponding to a given error code.
#'
#' @param error_code An integer representing the error code.
#' 
#' @return A string containing the error message corresponding to the error code.
#' @examples
#' \dontrun{
#'   message <- get_error_message(1001)
#'   print(message)
#' }
#' @export
get_error_message <- function(error_code) {
    # Load error codes
    error_codes <- load_error_codes()
    # Retrieve the message for the specified error code
    message <- error_codes$RUNTIME_ERROR[[as.character(error_code)]]
    if (is.null(message)) {
        stop(paste("Error code", error_code, "not found in configuration."), call. = FALSE)
    }
    return(message)
}
