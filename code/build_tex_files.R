#!/usr/bin/Rscript

# Script to convert spreadsheet into CV sections
library(dplyr) # verbs
library(tidyr) # nest/unnest, mostly
library(lubridate) # date handling
library(glue) # pasting stuff together
library(stringr) # string manipulation
library(scales) # formatting numbers as e.g. dollars
library(purrr) # list/df manipulation
library(magrittr) # pipe and extract2()
library(googlesheets4) # API access

# Update to match your info
sheet_link <- "https://docs.google.com/spreadsheets/d/11S8-U4vasXgPbW-IKQPCy1nO-1UsmvzNLz7GvdONN_w/edit#gid=389960622"
talk_order <- c("Invited", "Contributed", "Seminars")
grant_order <- c("Under Review", "Funded", "Not Funded")
service_order <- c("discipline", "institution", "department")

edu_data <- read_sheet(sheet_link, sheet = "Education")
exp_data <- read_sheet(sheet_link, sheet = "Experience")
grant_data <- read_sheet(sheet_link, sheet = "Grants")
prs_data <- read_sheet(sheet_link, sheet = "PRS")
award_data <- read_sheet(sheet_link, sheet = "Awards")
sw_data <- read_sheet(sheet_link, sheet = "Software")
talk_data <- read_sheet(sheet_link, sheet = "Talks")
teach_data <- read_sheet(sheet_link, sheet = "Teaching")
outreach_data <- read_sheet(sheet_link, sheet = "Outreach")
service_data <- read_sheet(sheet_link, sheet = "Service")
practice_data <- read_sheet(sheet_link, sheet = "Practice")
coursedev_data <- read_sheet(sheet_link, sheet = "CourseDev")
mentor_data <- read_sheet(sheet_link, sheet = "Mentoring")


# Functions used to build CV files
source("code/build_functions.R")


## XXX TODO: Rewrite these so that the functions actually make sense.
## Maybe add generic preprocessing functions?

# --- Process Education --------------------------------------------------------
if (nrow(edu_data) > 0) {
  edu_data %>%
    mutate(across(c(start, end), ymd)) %>%
    arrange(desc(end), desc(start)) %>%
    make_generic(datenames = c("start", "end"), 
                 fieldnames = c("degree", "major", "school", "other"),
                 get_year = T) %>%
    add_heading(make_heading("Education", "section")) %>%
    add_spacing("\\medskip") %>%
    writeLines(con = "_edu.tex")
}

# --- Process Experience -------------------------------------------------------
if (nrow(exp_data) > 0) {
  exp_data <- exp_data %>%
    mutate(across(c(start, end), ymd)) %>%
    arrange(desc(end), desc(start)) %>%
    make_generic(datenames = c("start", "end"),
                 fieldnames = c("position", "department", "location", "other"), 
                 get_year = T) %>%
    add_heading(make_heading("Professional Experience", "section")) %>%
    add_spacing("\\medskip") %>%
    writeLines(con = "_exp.tex")
}

# --- Process Awards -----------------------------------------------------------
if (nrow(award_data) > 0) {
  award_data %>%
    arrange(desc(start), desc(end), type) %>%
    make_generic(datenames = c("start", "end"),
                 fieldnames = c("title", "organization", "other")) %>%
    add_heading(make_heading("Awards", "section")) %>%
    add_spacing("\\medskip") %>%
    writeLines(con = "_awards.tex")
}
# --- Process Grants -----------------------------------------------------------
if (nrow(grant_data) > 0) {
  
  # Format grant data better
  grant_data_fix <- grant_data %>%
    mutate(across(ends_with("_total"), as.numeric)) %>%
    mutate(status = factor(status, levels = grant_order, ordered = T)) %>%
    arrange(status, desc(year_applied)) %>%
    # Lots more info here, so have to do some extra formatting...
    mutate(funding = paste(funding_org, funding_title, sep = ": "),
           grant_total_str = dollar_format()(grant_total),
           sub_total_str = dollar_format()(sub_total),
           amount_sub = sprintf("Total: %s, Sub: %s (%s)", grant_total_str, sub_total_str, sub_note) %>%
             gsub(" \\(\\)$", "", .),
           amount_nosub = sprintf("Total: %s", grant_total_str),
           amount = ifelse(subaward, amount_sub, amount_nosub))

  grant_data_fix %>% 
    nest(data = -status) %>%
    mutate(
      header = make_heading(status),
      texlines = .$data %>% 
        map(~make_generic(., datenames = c("start", "end"),
                          fieldnames = c("funding", "grant_title", 
                                         "grant_role", "amount"), 
                          get_year = F)),
      texlines = map2(texlines, header, add_heading)
    ) %>%
    unnest(status) %>%
    extract2("texlines") %>%
    unlist() %>%
    add_heading(make_heading("Grants", "section")) %>%
    writeLines(con = "_grants.tex")
}


# --- Process Talks ------------------------------------------------------------
if(nrow(talk_data) > 0) {
  talk_data %>%
    mutate(Type = factor(Type, levels = talk_order, ordered = T)) %>%
    mutate(slides = ifelse(nchar(Link) > 0, 
                           sprintf("\\href{%s}{Link}", Link),
                           "")) %>%
    arrange(desc(Date), Type) %>%
    nest(data = -Type) %>%
    mutate(
      header = make_heading(Type),
      texlines = .$data %>% 
        map(
          ~make_generic(., span = F, datenames = c("Date"),
                        fieldnames = c("Title", "Event", "Event2", "Location", "slides"),
                        get_year = T)),
      texlines = map2(texlines, header, add_heading)
    ) %>%
    tidyr::unnest(Type) %>%
    magrittr::extract2("texlines") %>%
    unlist() %>%
    add_heading(make_heading("Talks", "section")) %>%
    writeLines(con = "_talks.tex")
}

# --- Process Software ---------------------------------------------------------
if (nrow(sw_data) > 0) {
  sw_data %>%
    mutate(deprecated = is.na(date_end)) %>%
    arrange(desc(date_start)) %>%
    make_generic(datenames = c("date_start", "date_end"),
                 fieldnames = c("package", "description", "repository", "note"),
                 span = T, get_year = T) %>%
    add_entry_top("\\cvitem{}{\\footnotesize Dates show initial involvement; only packages which are no longer maintained have end dates.}") %>%
    add_heading(make_heading("Software", "subsection")) %>%
    add_spacing("\\medskip") %>%
    writeLines(con = "_software.tex")
}
# --- Process Teaching ---------------------------------------------------------
teach_data %>%
  mutate(semester = factor(semester, levels = c("Spring", "Fall"), ordered = T)) %>%
  mutate(course = paste(course_prefix, course_number)) %>%
  mutate(note = ifelse(!is.na(eval_mean),
                       paste(note, sprintf("Evals: %.2f (mean), %.0f (median)", eval_mean, eval_median), sep = ". "),
                       note)) %>%
  arrange(desc(year), semester, course_number) %>%
  make_generic(span = F, timeline = T, datenames = "year", fieldnames = c("course", "course_title", "location", "note")) %>%
  add_heading(make_heading("Teaching", "section")) %>%
  add_spacing("\\medskip") %>%
  writeLines(con = "_teaching.tex")


# --- Process Mentoring --------------------------------------------------------
if(nrow(mentor_data) > 0) {
  mentoring_data %>%
    mutate(degree = factor(degree, levels = c("Ph.D.", "MS", "Undergraduate", "Summer"))) %>%
    mutate(graduated = year_end > 0) %>%
    arrange(degree, graduated, desc(year_start), desc(year_end)) %>%
    nest(data = -degree) %>%
    mutate(
      header = make_heading(degree),
      texlines = .$data %>% 
        map(
          ~make_generic(., datenames = c("year_start", "year_end"),
                        fieldnames = c("name", "school", "description", "note", ""))),
      texlines = map2(texlines, header, add_heading)
    ) %>%
    tidyr::unnest(degree) %>%
    magrittr::extract2("texlines") %>%
    unlist() %>%
    add_heading(make_heading("Mentoring", "section")) %>%
    writeLines(con = "_mentoring.tex")
}
# --- Process Service ----------------------------------------------------------
if(nrow(service_data) > 0) {
  service_data %>%
    mutate(type = factor(str_to_title(type), 
                         levels = unique(str_to_title(service_order), type))) %>%
    arrange(type, desc(start), desc(end)) %>%
    nest(data = -type) %>%
    mutate(
      header = make_heading(type),
      texlines = 
        map(data, 
          ~make_generic(., datenames = c("start", "end"),
                        fieldnames = c("description", "description2", "org", "note"))),
      texlines = map2(texlines, header, add_heading)
    ) %>%
    tidyr::unnest(type) %>%
    magrittr::extract2("texlines") %>%
    unlist() %>%
    add_heading(make_heading("Service", "section")) %>%
    writeLines(con = "_service.tex")
}

# --- Process Practice ---------------------------------------------------------
if(nrow(practice_data) > 0) {
  practice_data %>%
    mutate(Date = ymd(Date), year = year(Date)) %>%
    arrange(Type, desc(Date)) %>%
    nest(data = -Type) %>%
    mutate(
      header = make_heading(Type),
      texlines = 
        map(data, 
            ~make_generic(., datenames = c("year"), span = F,
                          fieldnames = c("Title", "Location1", "Location2", "Length", "Notes"))) %>%
        map2(., header, add_heading)
    ) %>%
    tidyr::unnest(Type) %>%
    magrittr::extract2("texlines") %>%
    unlist() %>%
    add_heading(make_heading("Professional Practice", "section")) %>%
    writeLines(con = "_practice.tex")
}



