# Note: all functions copied from the 'insight' package, Version: 0.18.3.1
# published under GPL-3 license
# authors: 


#' @title Format messages and warnings
#' @name format_message
#'
#' @description Inserts line breaks into a longer message or warning string.
#'   Line length is adjusted to maximum length of the console, if the width
#'   can be accessed. By default, new lines are indented by two spaces.
#'
#'   `format_alert()` is a wrapper that combines formatting a string with a
#'   call to `message()`, `warning()` or `stop()`. By default, `format_alert()`
#'   creates a `message()`. `format_warning()` and `format_error()` change the
#'   default type of exception to `warning()` and `stop()`, respectively.
#'
#' @param string A string.
#' @param ... Further strings that will be concatenated as indented new lines.
#' @param line_length Numeric, the maximum length of a line.
#'   The default is 90% of the width of the console window.
#' @param indent Character vector. If further lines are specified in `...`, a
#' user-defined string can be specified to indent subsequent lines. Defaults to
#' `"  "` (two white spaces), hence for each start of the line after the first
#' line, two white space characters are inserted.
#'
#' @details
#'
#' This features has some limitations: it's hard to detect the exact length for
#' each line when the string has multiple lines (after line breaks) and the
#' string contains formatting tags. Thus, it can happen that lines are
#' wrapped at an earlier length than expected. Furthermore, if 
#' you have multiple words in a format tag (`{.b one two three}`), a 
#' line break might occur inside this tag, and the formatting no longer
#'works (messing up the message-string).
#'
#' @return For `format_message()`, a formatted string.
#'   For `format_alert()` and related functions, the requested exception,
#'   with the exception formatted using `format_message()`.
#' @export
format_message <- function(string,
                           ...,
                           line_length = 0.9 * getOption("width", 80),
                           indent = "  ") {
  if (is.null(line_length) || is.infinite(line_length) || line_length < 1) {
    line_length <- 70
  }
  
  all_lines <- c(string, ...)
  
  string <- .wrap_message_line(all_lines[1], line_length = line_length)
  further_lines <- all_lines[-1]
  
  if (length(further_lines)) {
    further_lines <- lapply(further_lines, function(i) {
      .wrap_message_line(string = i, line_length = line_length, indent = indent)
    })
    string <- paste0(c(string, unlist(further_lines)), collapse = "\n")
  }
  
  string
}

#' @name format_alert
#' @rdname format_message
#'
#' @param type Type of exception alert to raise.
#'   Can be `"message"` for `message()`, `"warning"` for `warning()`,
#'   or `"error"` for `stop()`.
#' @param call. Logical. Indicating if the call should be included in the the
#'   error message. This is usually confusing for users when the function
#'   producing the warning or error is deep within another function, so the
#'   default is `FALSE`.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   format_alert("This is a message.")
#'   format_alert("This is a warning.", type = "warning")
#'   format_warning("This is a warning.")
#'   format_error("This is an error.")
#' }
format_alert <- function(string,
                         ...,
                         line_length = 0.9 * getOption("width", 80),
                         indent = "  ",
                         type = "message",
                         call. = FALSE) {
  type <- match.arg(type, choices = c("message", "warning", "error"))
  if (type == "message") {
    message(format_message(
      string = string, ...,
      line_length = line_length, indent = indent
    ))
  } else if (type == "warning") {
    warning(format_message(
      string = string, ...,
      line_length = line_length, indent = indent
    ), call. = call.)
  } else {
    stop(format_message(
      string = string, ...,
      line_length = line_length, indent = indent
    ), call. = call.)
  }
}

#' @name format_warning
#' @rdname format_message
#' @export
format_warning <- function(...) {
  format_alert(..., type = "warning")
}

#' @name format_error
#' @rdname format_message
#' @export
format_error <- function(...) {
  format_alert(..., type = "error")
}



# helper -----------------------

.wrap_message_line <- function(string, line_length, indent = NULL) {
  line_length <- round(line_length)
  line_separator <- "\\1\n  "
  lsub <- 0
  tmp_string <- string
  
    
  # check if line breaks are required
  if (line_length > 0 && nchar(tmp_string) > line_length) {
    # insert line breaks into string at specified length
    pattern <- paste("(.{1,", line_length, "})(\\s|$)", sep = "")
    string <- gsub(pattern, line_separator, string)
    
    # remove last line break
    l <- nchar(string)
    lc <- substr(string, l - lsub, l)
    if (lc == "\n") {
      string <- substr(string, 0, l - (lsub + 1))
    }
  }
  
  # remove trailing white space
  if (grepl("\\n  $", string)) {
    string <- gsub("\\n  $", "", string)
  }
  
  if (!is.null(indent)) {
    string <- paste0(indent, string)
  }
  
  string
}

