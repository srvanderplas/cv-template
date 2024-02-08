
# Stolen from https://stackoverflow.com/questions/36338629/escaping-special-latex-characters-in-r
escapeLatexSpecials <- function(x) {
  # x <- gsub("\\", "$\\backslash$", x, fixed = T)
  x <- gsub("#", "\\\\#", x)
  x <- gsub("$", "\\$", x, fixed = T)
  x <- gsub("%", "\\\\%", x)
  x <- gsub("&", "\\\\&", x)
  x <- gsub("~", "\\\\~", x)
  x <- gsub("_", "\\\\_", x)
  # x <- gsub("^", "\\\\^", x)
  # x <- gsub("\\{", "\\\\{", x)
  # x <- gsub("\\}", "\\\\}", x)
  # x <- gsub(">", "$>$", x)
  # x <- gsub("<", "$<$", x)
  return(x)
}


print_heading <- function(type, data,
                          heading_type = "\\subsection{%s}", spacing = "\\medskip",
                          fn = print_talks) {
  c(
    sprintf(heading_type, type),
    fn(data),
    spacing,
    ""
  )
}

print_generic <- function(type, date1, date2 = NA,
                          field1 = "", field2 = "", field3 = "",
                          field4 = "", field5 = "") {
  datefields <- ifelse(type == "tlcventry",
                       sprintf("{%d}{%d}", date1, date2),
                       sprintf("{%d}", date1))
  sprintf("\\%s%s{%s}{%s}{%s}{%s}{%s}", type, datefields,
          field1, field2, field3, field4, field5) %>%
    gsub("{NA}", "{}", ., fixed = T)
}

print_df_generic <- function(df) {
  stopifnot(all(c("start", "end") %in% names(df)))
  stopifnot("show_span" %in% names(df))
  df <- df %>%
    mutate(type = ifelse(show_span, "tlcventry", "tldatecventry")) %>%
    mutate(datefields = ifelse(type == "tlcventry",
                         sprintf("{%d}{%d}", start, end),
                         sprintf("{%d}", start)))
  # Figure out what fields are left in the data frame and rename them to field1... fieldn
  fields <- setdiff(names(df), c("show_span", "type", "start", "end", "datefields"))
  names(fields) <- paste0("field", 1:length(fields))
  df <- rename(df, fields)
  # We have 5 fields to fill.
  # Figure out which ones we need to create dummy cols for
  newfields <- setdiff(paste0("field", 1:5), fields)
  for(i in newfields) df[[i]] <- ""

  # Replace any NAs with ""
  df <- mutate(df, across(all_of(fields), ~str_replace_na(., ""))) %>%
    mutate(across(matches("field"), escapeLatexSpecials))

  with(df,
       sprintf("\\%s%s{%s}{%s}{%s}{%s}{%s}", type, datefields,
               field1, field2, field3, field4, field5))
}


print_grants <- function(df, include_links = F) {
  dates <- ifelse(df$show_span,
                  sprintf("\\tlcventry{%d}{%d}", df$start, df$end),
                  sprintf("\\tldatecventry{%d}", df$year_applied))
  grant_info <- sprintf("{%s}{%s}{%s}{%s}{}",
                        df$funding, df$grant_title, df$grant_role, df$amount)

  item_format <- paste0(dates, grant_info)
}

print_talks <- function(df, include_links = F) {
  sprintf("\\tldatecventry{%d}{%s}{%s}{%s}{%s}{%s} %% %s",
          df$year, df$Title,
          df$Event, df$Event2,
          df$Location, ifelse(include_links, df$slides, ""),
          as.character(df$Date))
}

print_software <- function(df) {
  dates <- ifelse(df$deprecated,
                  sprintf("\\tldatecventry{%d}", df$date_start),
                  sprintf("\\tlcventry{%d}{%d}", df$date_start, df$date_end))
  info <- sprintf("{\\texttt{%s}}{%s}{\\href{%s}{Repository}}{}{}",
                  df$package, df$description, df$link)

  item_format <- paste0(dates, info)
}


print_teaching <- function(df){
  datefields <- sprintf("{%d}", df$year)
  sprintf("\\%s%s{%s}{%s}{%s}{%s}{%s}", "tldatecventry", datefields,
          df$course, df$course_title, df$location, df$note, "") %>%
    gsub("{NA}", "{ }", ., fixed = T)
}


print_mentoring <- function(df){
  datefields <- ifelse(df$year_start != df$year_end,
                       sprintf("\\tlcventry{%d}{%d}", df$year_start, df$year_end),
                       sprintf("\\tldatecventry{%d}", df$year_start))
  sprintf("%s{%s}{%s}{%s}{%s}{%s}", datefields,
          df$name, df$school, df$description, df$note, "") %>%
    gsub("{NA}", "{ }", ., fixed = T)
}
