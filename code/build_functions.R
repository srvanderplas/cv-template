
# Stolen from https://stackoverflow.com/questions/36338629/escaping-special-latex-characters-in-r
escapeLatexSpecials <- function(x) {
  # x <- gsub("\\", "$\\backslash$", x, fixed = T)
  x[!is.na(x)] <- gsub("#", "\\\\#", x[!is.na(x)])
  x[!is.na(x)] <- gsub("$", "\\$", x[!is.na(x)], fixed = T)
  x[!is.na(x)] <- gsub("%", "\\\\%", x[!is.na(x)])
  x[!is.na(x)] <- gsub("&", "\\\\&", x[!is.na(x)])
  x[!is.na(x)] <- gsub("~", "\\\\~", x[!is.na(x)])
  x[!is.na(x)] <- gsub("_", "\\\\_", x[!is.na(x)])
  # x <- gsub("^", "\\\\^", x)
  # x <- gsub("\\{", "\\\\{", x)
  # x <- gsub("\\}", "\\\\}", x)
  # x <- gsub(">", "$>$", x)
  # x <- gsub("<", "$<$", x)
  return(x)
}

add_heading <- function(entries, heading) {
  c(heading, entries)
}

add_spacing <- function(x, spacing = "\\medskip") {
  c(x, spacing, "")
}

add_entry_top <- function(entries, new_entry) {
  c(new_entry, entries)
}

add_entry_bottom <- function(entries, new_entry) {
  c(entries, new_entry)
}


make_heading <- function(heading_name, heading_type = "subsection") {
  glue("\\{heading_type} {{{heading_name}}}")
}


#' Formats dates to fit with modern timeline or modern cv
#' 
#' Function that will take arguments span (whether 2 dates are used),
#' timeline (whether moderncvtimeline is used), and datenames and format a 
#' glue string to ingest data.
#' @param datenames vector of 1-2 date name fields
#' @param span logical variable to indicate whether two dates will be used. 
#'             If moderncvtimeline is also used, this will create a tlcventry.
#' @param timeline logical variable to indicate whether to format based on 
#'             moderncvtimeline (T) or just moderncv (F)
#' @param ... other arguments passed to make_XXentry functions
#' @return a vector of latex codes to handle dates within moderncv
#' @importFrom dplyr case_when
make_dates <- function(datenames, span = T, timeline = T, ...) {
  stopifnot(is.logical(span))
  stopifnot(is.logical(timeline))
  stopifnot(length(datenames) %in% c(1, 2))
  stopifnot(is.character(datenames))
  
  ifelse(timeline, {
    ifelse(span, make_tlcventry(datenames, ...), make_tldatecventry(datenames, ...))
  }, {
    ifelse(span, make_cventry(datenames, ...), make_cventry(datenames[1], ...))
  })
}
fix_year <- function(x, get_year = F) {
  if (get_year) {
    paste("{ year(", x, ") }")
  } else {
    paste("{ ", x, " }")
  }
}
collapse_dates <- function(datenames, collapse = "", ...) {
  datenames2 <- fix_year(datenames, ...)
  paste0("{{", paste(datenames2, collapse = collapse, sep = ""), "}}")
}
collapse_labels <- function(labelname, ...) {
  paste0("{{ {", labelname, "} }}")
}
make_tlcventry <- function(datenames, ...){
  stopifnot(length(datenames) == 2)
  paste0("\\tlcventry", 
         collapse_dates(datenames, collapse = "}}{{", ...)
  )
}
make_tldatelabelcventry <- function(datenames, labelname, verbose = F, ...){
  stopifnot(length(datenames) == 2)
  paste0("\\tldatelabelcventry", 
         collapse_labels(labelname, ...),
         collapse_dates(datenames, collapse = "}}{{", ...)
  )
}
make_tldatecventry <- function(datenames, verbose = F, ...){
  if(length(datenames) > 1) {
    if (verbose) warning("Using only first date name")
    datenames <- datenames[1]
  }
  paste0("\\tldatecventry", collapse_dates(datenames, ...))
}
make_cventry <- function(datenames, span, ...) {
  coll <- ifelse(length(datenames) == 2, "--", "")
  paste0("\\cventry", collapse_dates(datenames, collapse = coll, ...))
}

#' Clean NA fields from latex code
#' 
#' @param x string to clean
#' @return a vector the same length as x
#' @importFrom stringr str_replace_all
clean_na <- function(x) {
  str_replace_all(x, "\\{NA\\}", "{}")
}


#' Make a generic set of CV entries
#' 
#' @param data data frame of information
#' @param span either the column name used to indicate whether span 
#'             (start - end) should be shown, quoted, or a logical value 
#'             indicating whether the span should be shown for all items.
#' @param timeline whether `moderncvtimeline` is to be used or not
#' @param datenames vector of up to two date names
#' @param fieldnames vector of up to five field names, in order
#' @param ... arguments to be passed on to make_XXcventry
#' @return vector of latex commands, one for each row of data
#' @importFrom tidyr nest unnest
#' @importFrom dplyr mutate groupby `%>%`
#' @importFrom glue glue_collapse glue_data
make_generic <- function(data, 
                         span = "show_span", 
                         timeline = T, 
                         datenames = paste0("date", 1:2),
                         fieldnames = paste0("field", 1:5), ...){
  # data not actually substituted until later
  if(is.character(span)) {
    data$date_template <- make_dates(datenames, span = data[[span]], timeline = timeline, ...) 
  } else if (is.logical(span)) {
    data$date_template <- make_dates(datenames, span = span, timeline = timeline, ...) 
  } else {
    stop("span parameter must be a column name or T/F")
  }
  
  data <- tidyr::nest(data, data = - date_template) %>%
    group_by(date_template) %>%
    mutate(date_filled = purrr::map2(data, date_template, ~glue_data(.x = .x, .y))) %>%
    tidyr::unnest(c(date_filled, data))
  
  # Handle variable numbers of included fields
  extra <- ""
  if (length(fieldnames) < 5) {
    extra <- rep("{}", length = 5 - length(fieldnames)) %>% paste(collapse = "")
  } else if (length(fieldnames) > 5) {
    warning("Only first 5 fields will be used")
    fieldnames <- fieldnames[1:5]
  }
  
  # Escape latex characters in fields
  data <- data %>%
    mutate(across(all_of(fieldnames), escapeLatexSpecials))
  
  # Create fields from data + template
  data$fields <- paste0("{{{", fieldnames, "}}}") %>%
    glue_collapse() %>%
    glue_data(data, .) %>%
    paste0(., extra)
  
  # Paste date template and field template together, clean out NAs
  paste0(data$date_filled, data$fields) %>%
    clean_na()
}
