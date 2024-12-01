#' @title Title of the R6 Class
#'
#' @description A brief overview of what this class does.
#'
#' @details Add more detailed information about the class.
#'
#' @section Methods:
#' \describe{
#'     \item{\code{initialize(arg1, arg2)}}{Initializes the class with arguments arg1 and arg2.}
#'     \item{\code{public_method()}}{Performs a public action or calculation.}
#' }
#'
#' @section Private Methods:
#' \describe{
#'     \item{\code{$..private_method()}}{Performs an internal, private action or calculation.}
#' }
#'
#' @examples
#' \dontrun{
#'     # Create an instance of the class
#'     obj <- MyClassnew(arg1 = "value", arg2 = 42)
#'
#'     # Call a public method
#'     objpublic_method()
#'
#'     # Access a public field
#'     print(objfield_name1)
#' }
#'
#' @export
BaseEstimator <- R6::R6Class(
    "BaseEstimator",
    public = list(
        #' Get parameters for this estimator
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

        #' Set parameters of the estimator
        #' @description Updates the parameters of the estimator.
        #' @param ... Named parameters to update.
        #' @return The estimator itself (for chaining).
        set_params = function(...) {
            params <- list(...)
            if (length(params) == 0) {
                return(self)  # Simple optimization to gain speed (inspect is slow)
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
                    stop(paste0("Invalid parameter: ", key, "for estimator", self$class$classname))
                    stop("Valid parameters are: ", paste(valid_params, collapse = ", "))
                }

                if (is.null(delim)) {
                    private[[key]] <- value
                } else {
                    #TODO: Implement and test nested parameter setting
                    nested <- private[[key]]
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
