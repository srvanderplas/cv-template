#!/usr/bin/Rscript

# Script to convert spreadsheet into CV sections
library(dplyr)
library(lubridate)
library(googlesheets4)
library(scales)

# Update to match your info
sheet_link <- "https://docs.google.com/spreadsheets/d/11S8-U4vasXgPbW-IKQPCy1nO-1UsmvzNLz7GvdONN_w/edit#gid=389960622"
talk_order <- c("Invited", "Contributed", "Seminars")
grant_order <- c("Under Review", "Funded", "Not Funded")

edu_data <- read_sheet(sheet_link, sheet = "Education")
exp_data <- read_sheet(sheet_link, sheet = "Experience")
grant_data <- read_sheet(sheet_link, sheet = "Grants")
prs_data <- read_sheet(sheet_link, sheet = "PRS")
award_data <- read_sheet(sheet_link, sheet = "Awards")
sw_data <- read_sheet(sheet_link, sheet = "Software")
talk_data <- read_sheet(sheet_link, sheet = "Talks")
teach_data <- read_sheet(sheet_link, sheet = "Teaching")
coursedev_data <- read_sheet(sheet_link, sheet = "CourseDev")
mentor_data <- read_sheet(sheet_link, sheet = "Mentoring")
outreach_data <- read_sheet(sheet_link, sheet = "Outreach")


# Functions used to build CV files
source("code/build_functions.R")


## XXX TODO: Rewrite these so that the functions actually make sense.
## Maybe add generic preprocessing functions?

if (nrow(edu_data) > 0) {
  edu_data <- edu_data %>%
    mutate(across(c(start, end), ymd)) %>%
    mutate(across(c(start, end), year)) %>%
    arrange(desc(end), desc(start)) %>%
    rename(field1 = degree, field2 = major,
           field3 = school, field4 = other) %>%
    select(show_span, start, end, matches("field"))

  print_df_generic(edu_data) %>%
    writeLines(con = "_edu.tex")
}

if (nrow(exp_data) > 0) {
  exp_data <- exp_data %>%
    mutate(across(c(start, end), ymd)) %>%
    mutate(across(c(start, end), year)) %>%
    arrange(desc(end), desc(start)) %>%
    rename(field1 = position, field2 = department,
           field3 = location, field4 = other) %>%
    select(show_span, start, end, matches("field"))

  print_df_generic(exp_data) %>%
    writeLines(con = "_exp.tex")
}

# --- Process Grants -----------------------------------------------------------
if (nrow(grant_data) > 0) {
  grant_data <- grant_data %>%
    # Convert NAs to ''
    mutate(across(where(is.character), ~ifelse(is.na(.), "", .))) %>%
    mutate(status = factor(status, levels = grant_order, ordered = T)) %>%
    arrange(status, desc(year_applied)) %>%
    # Lots more info here, so have to do some extra formatting...
    mutate(funding = paste(funding_org, funding_title, sep = ": "),
           grant_total_str = dollar_format()(grant_total),
           sub_total_str = dollar_format()(sub_total),
           amount_sub = sprintf("Total: %s, Sub: %s (%s)", grant_total_str, sub_total_str, sub_note) %>%
             gsub(" \\(\\)$", "", .),
           amount_nosub = sprintf("Total: %s", grant_total_str),
           amount = ifelse(subaward, amount_sub, amount_nosub)) %>%
    mutate(across(c(funding, amount, grant_title), escapeLatexSpecials))

  tmp <- grant_data %>% nest_by(status)
  info <- purrr::map2(tmp$status, tmp$data, print_heading, fn = print_grants)

  writeLines(unlist(info), con = "_grants.tex")
}


# --- Process Talks ------------------------------------------------------------
# Create columns for year - CV doesn't need that much detail
talk_data <- talk_data %>%
  mutate(year = year(Date)) %>%
  # Convert NAs to ''
  mutate(across(where(is.character), ~ifelse(is.na(.), "", .))) %>%
  mutate(Type = factor(Type, levels = talk_order, ordered = T)) %>%
  mutate(slides = sprintf("\\href{%s}{Link}", escapeLatexSpecials(Link)) %>%
           gsub("^.href.{3}Link.$", "", .) %>%
           gsub("$(.){2}", "\1", .)) %>%
  arrange(desc(Date), Type)


tmp <- talk_data %>% nest_by(Type)
info <- purrr::map2(tmp$Type, tmp$data, print_heading, fn = print_talks)

writeLines(unlist(info), con = "_talks.tex")

# --- Process Software ---------------------------------------------------------
software_data <- software_data %>%
  # Convert NAs to ''
  mutate(across(where(is.character), ~ifelse(is.na(.), "", .))) %>%
  mutate(deprecated = is.na(date_end)) %>%
  arrange(desc(date_start)) %>%
  mutate(across(c(description, link), escapeLatexSpecials))

info <- print_software(software_data)

writeLines(unlist(info), con = "_software.tex")

# --- Process Teaching ---------------------------------------------------------
info <- teaching_data %>%
  mutate(semester = factor(semester, levels = c("Spring", "Fall"), ordered = T)) %>%
  mutate(course = paste(course_prefix, course_number)) %>%
  mutate(note = ifelse(!is.na(eval_mean),
                       paste(note, sprintf("Evals: %.2f (mean), %.0f (median)", eval_mean, eval_median), sep = ". "),
                       note)) %>%
  arrange(desc(year), semester, course_number)

writeLines(unlist(print_teaching(info)), con = "_teaching.tex")

# --- Process Mentoring --------------------------------------------------------

tmp <- mentoring_data %>%
  mutate(across(where(is.character), ~ifelse(is.na(.), "", .))) %>%
  mutate(degree = factor(degree, levels = c("Ph.D.", "MS", "Undergraduate", "Summer"))) %>%
  mutate(graduated = year_end > 0) %>%
  arrange(degree, graduated, desc(year_start), desc(year_end)) %>%
  nest_by(degree)

info <- purrr::map2(tmp$degree, tmp$data, print_heading, fn = print_mentoring)
writeLines(unlist(info), con = "_mentoring.tex")
