#' Logging levels enumeration
#' @export
LOG_LEVELS <- list(
  DEBUG = 1,
  INFO = 2,
  WARN = 3,
  ERROR = 4,
  NONE = 5
)

#' Global logging configuration
#' @export
logging_config <- new.env()
logging_config$level <- LOG_LEVELS$INFO
logging_config$file <- NULL

#' Configure logging settings
#' 
#' @param level Logging level (one of LOG_LEVELS)
#' @param file Optional file path to write logs to
#' @export
configure_logging <- function(level = LOG_LEVELS$INFO, file = NULL) {
  logging_config$level <- level
  logging_config$file <- file
}

#' Internal logging function
#' 
#' @param level The log level for this message
#' @param msg The message to log
#' @param ... Additional objects to include in log message
log_message <- function(level, msg, ...) {
  if (level >= logging_config$level) {
    # Format timestamp
    timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    
    # Get level name
    level_name <- names(LOG_LEVELS)[sapply(LOG_LEVELS, function(x) x == level)]
    
    # Format message with any additional objects
    full_msg <- sprintf(msg, ...)
    
    # Create log entry
    log_entry <- sprintf("[%s] %s: %s", timestamp, level_name, full_msg)
    
    # Output to console
    cat(log_entry, "\n")
    
    # Write to file if configured
    if (!is.null(logging_config$file)) {
      write(log_entry, file = logging_config$file, append = TRUE)
    }
  }
}

#' Debug level logging
#' @export
log_debug <- function(msg, ...) log_message(LOG_LEVELS$DEBUG, msg, ...)

#' Info level logging
#' @export
log_info <- function(msg, ...) log_message(LOG_LEVELS$INFO, msg, ...)

#' Warning level logging
#' @export
log_warn <- function(msg, ...) log_message(LOG_LEVELS$WARN, msg, ...)

#' Error level logging
#' @export
log_error <- function(msg, ...) log_message(LOG_LEVELS$ERROR, msg, ...) 