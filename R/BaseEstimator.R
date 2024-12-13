#' @importFrom R6 R6Class

#' @title BaseEstimator R6 Class
#' @description A base class for implementing estimators in R6.
#' @details This class provides utility methods for managing parameters (`get_params` and `set_params`).
#' It is intended to be inherited by other estimator classes to streamline parameter handling.
#' @section Methods:
#' \describe{
#'     \item{\code{get_params(deep = TRUE)}}{Retrieves the parameters of the estimator as a named list.}
#'     \item{\code{set_params(...)}}{Sets the parameters of the estimator, allowing nested parameters.}
#' }
#' @examples
#' \dontrun{
#' # Create an instance of BaseEstimator (or subclass)
#' base <- BaseEstimator$new()
#'
#' # Get parameters
#' params <- base$get_params()
#' print(params)
#'
#' # Set parameters
#' base$set_params(param1 = "value", param2 = 42)
#' }
#'
BaseEstimator <- R6::R6Class(
    "BaseEstimator",
    public = list(
        #' @description Retrieves the parameters of the estimator.
        #' @param deep Logical. If TRUE, include parameters of nested estimators.
        #' @return A named list of parameters.
        get_params = function(deep = TRUE) {
            params <- list()
            for (key in private$..get_param_names()) {
                value <- private[[key]]
                if (deep && is.environment(value) && is.function(value$get_params)) {
                    nested_params <- value$get_params()
                    nested_keys <- paste0(key, "__", names(nested_params))
                    params[nested_keys] <- nested_params
                } else {
                    params[[key]] <- value
                }
            }
            return(params)
        },

        #' @description Sets the parameters of the estimator.
        #' @param ... Named parameters to update.
        #' @return The estimator itself (for method chaining).
        set_params = function(...) {
            params <- list(...)
            if (length(params) == 0) {
                return(self)  # Simple optimization
            }
            valid_params <- private$..get_param_names()
            for (key in names(params)) {
                partition <- function(string, delim) {
                    pos <- regexpr(delim, string, fixed = TRUE)
                    if (pos[1] == -1) {
                        return(c(string, "", ""))
                    } else {
                        key <- substr(string, 1, pos[1] - 1)
                        sub_key <- substr(string, pos[1] + nchar(delim), nchar(string))
                        return(c(key, delim, sub_key))
                    }
                }
                parts <- partition(key, "__")
                key <- parts[1]
                delim <- parts[2]
                sub_key <- parts[3]
                value <- params[[key]]
                if (!(key %in% valid_params)) {
                    stop(paste0("Invalid parameter: ", key, " for estimator ", self$class$classname))
                }
                if (is.null(delim)) {
                    private[[key]] <- value
                } else {
                    nested <- private[[key]]
                    if (!is.null(nested) && is.environment(nested) && is.function(nested$set_params)) {
                        nested$set_params(... = list(sub_key = value))
                    } else {
                        stop(paste0("Nested parameter setting not implemented for key: ", key))
                    }
                }
            }
            return(self)
        }
    ),
    private = list(
        ..get_param_names = function() {
            parameters <- names(formals(self$initialize))
            return(sort(parameters[parameters != "..." & parameters != "self"]))
        }
    )
)
