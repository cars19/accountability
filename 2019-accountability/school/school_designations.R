# 2019 School Designations (Focus, Reward, Priority Exit)
# Josh Carson
# Last Updated: 2019-07-17

library(magrittr)
library(openxlsx)
library(tidyverse)

# Resume on line ___
# Search for "Q:" to find questions.

# Data ------------------------------------------------------------------------

# 2016 and 2017 Success Rates
accountability_2017 <-
  read_csv("N:/ORP_accountability/data/2017_final_accountability_files/school_numeric_2017_JW_10242017.csv")

accountability_2018 <-
  read_csv("N:/ORP_accountability/data/2018_final_accountability_files/2018_school_accountability_file.csv")

grades_2018 <-
  read_csv("N:/ORP_accountability/projects/2018_school_accountability/school_grading_grades.csv")

metrics_2018 <-
  read_csv("N:/ORP_accountability/projects/2018_school_accountability/school_grading_metrics.csv")

school_names_2018 <-
  read_csv("N:/ORP_accountability/data/2018_final_accountability_files/school_names.csv")

schools_csi_2018 <-
  read.xlsx("N:/ORP_accountability/data/2018_final_accountability_files/school_designations_file.xlsx",
            sheet = "Comprehensive Support")

schools_priority_2018 <-
  read.xlsx("N:/ORP_accountability/data/2018_final_accountability_files/school_designations_file.xlsx",
            sheet = "Priority")

tvaas_level_2017 <-
  read.xlsx("N:/ORP_accountability/data/2017_tvaas/2017 School Composites.xlsx") %>%
  janitor::clean_names()

tvaas_level_2018 <-
  read.xlsx("N:/ORP_accountability/data/2018_tvaas/School Composite Level.xlsx") %>%
  janitor::clean_names()

student_groups_tsi <-
  accountability_2018 %>%
  distinct(subgroup) %>%
  filter(!subgroup %in% c("All Students", "English Learners",
                          "Super Subgroup")) %>%
  arrange(subgroup) %>%
  extract2("subgroup")

# TSI (Focus) -----------------------------------------------------------------

# Focus Rule 1, Line 61: 10 Student Groups

# Q: Why do the file layouts on TNShare list 11 groups?

s <-
  metrics_2018 %>%
  filter(subgroup %in% student_groups_tsi) %>%
  distinct(subgroup)

stopifnot(nrow(s) == 10)
rm(s)

metrics_group <-
  metrics_2018 %>%
  filter(
    designation_ineligible == 0,
    # Q: Should subgroup_score:focus be null where subgroup == "All Students"?
    subgroup != "Subgroups",
    # Rule 1, Line 67: Eligibility for All Indicators
    total_weight == 1
  ) %>%
  # Rule 2a, Line 68: Calculating Percent Ranks
  group_by(pool, subgroup) %>%
  mutate(
    # Rule 3, Line 71: Exclusion of Schools > 1.0
    elig_tsi =
      as.numeric(
        # Removed per guidance from USED:
        # subgroup_average <= 1
        !subgroup %in% c("All Students", "Super Subgroup")
      ),
    # ATSI Rule 3, Line 79: Safe Harbor
    safe_atsi = 
      as.numeric(
        pmin(score_achievement, score_growth, score_grad,
             score_ready_grad, score_absenteeism, score_elpa,
             na.rm = T) >= 3
      ),
    rank = rank(subgroup_average, na.last = "keep", ties.method = "min"),
    n_schools = sum(!is.na(subgroup_average)),
    subgroup_score_percentile = 100 * rank / n_schools,
    year = 2018
  ) %>%
  ungroup() %>%
  select(
    year, system, school, subgroup, pool, designation_ineligible,
    subgroup_average, elig_tsi, safe_atsi,
    rank:subgroup_score_percentile
  ) %>%
  rename(subgroup_score = subgroup_average) %>%
  arrange(system, school, subgroup)

t <- metrics_group
stopifnot(n_distinct(t$system, t$school, t$pool, t$subgroup) == nrow(t))
rm(t)

focus_group <-
  metrics_group %>%
  # Q: Where do we document that a CSI school cannot be TSI?
  anti_join(schools_csi_2018 %>% select(system, school)) %>%
  # Rule 2, Line 68: Student Groups in the Bottom 5%
  mutate(focus = as.numeric(elig_tsi == 1 & subgroup_score_percentile <= 5)) %>%
  group_by(year, system, school) %>%
  filter(max(focus) == 1) %>%
  ungroup() %>%
  # ATSI Rule 1, Line 77: ATSI from TSI
  # ATSI Rule 3, Line 79: Safe Harbor
  mutate(elig_atsi = as.numeric(focus == 1 & safe_atsi == 0)) %>%
  left_join(school_names_2018, by = c("system", "school")) %>%
  select(
    year, system, system_name, school, school_name, subgroup:subgroup_score,
    rank:subgroup_score_percentile, focus, elig_atsi
  ) %>%
  arrange(system_name, school_name)

# 159 TSI schools
n_distinct(focus_group$system, focus_group$school)
summary(focus_group$subgroup_score_percentile)
mean(focus_group$elig_atsi[focus_group$focus == 1])

# Compare the share of TN schools that is high schools to the share of TSI
# schools that is high schools.

list(metrics_group, focus_group) %>%
  set_names(nm = c("All", "TSI")) %>%
  map(
    ~ .x %>%
      distinct(system, school, pool) %>%
      summarize(
        n_high_schools = sum(pool == "HS"),
        n_schools = n(),
        percent_high_school = mean(pool == "HS")
      )
  )

# ATSI ------------------------------------------------------------------------

# In the 2017 numeric file, there are multiple grade levels within some
# school-subject-subgroup combinations. Grade levels are mutually exclusive,
# though, so ostensibly students will not be double-counted below.

accountability_2017 %>%
  group_by(year) %>%
  summarize(
    n_school = n_distinct(system, school),
    n_subject = n_distinct(system, school, subject),
    n_subgroup = n_distinct(system, school, subject, subgroup),
    n_grade = n_distinct(system, school, subject, subgroup, grade),
    n_row = n()
  )

list(accountability_2017, mutate(accountability_2018, year = 2018)) %>%
  map(~ .x %>% distinct(year, subgroup) %>% mutate(present = 1)) %>%
  reduce(bind_rows) %>%
  spread(year, present) %>%
  arrange(subgroup) %>%
  View()

# black_2018 <- "Black or African American"
el_2018 <- "English Learners with Transitional 1-4"
# hpi_2018 <- "Native Hawaiian or Other Pacific Islander"

accountability_2017 %>% distinct(year, grade) %>% arrange(year, grade)

# Rule 2, Footnote 16: Three Years of Data
# 2019 ATSI designations will rely upon data from 2016-17, 2017-18, and 2018-19.

# Q: How do we handle a school without three years of achievement data?

# Q: How do we handle a school that is designation-eligible in 2019 but
# ineligible in 2017 and/or 2018? (Is this even possible?)

success_rate_group <-
  accountability_2018 %>%
  # Accountability content areas pre-2019: ELA, Math, and Science
  filter(
    designation_ineligible == 0,
    indicator == "Achievement"
    # n_count >= 30 (This filter happens below, after pooling across years.)
  ) %>%
  transmute(
    year = 2018,
    system, school, subgroup,
    n_otm = round(n_count * metric / 100),
    n_total = n_count,
    success_rate = metric
  ) %>%
  bind_rows(
    accountability_2017 %>%
      filter(!subject %in% c("ACT", "Graduation Rate", "Social Studies")) %>%
      mutate(
        # Q: How should we compare ELs across years?
        subgroup =
          if_else(subgroup == "English Language Learners with T1/T2",
                  "English Learners with Transitional 1-4",
                  subgroup)
        # subgroup =
        #   case_when(subgroup == "Black" ~ black_2018,
        #             str_detect(subgroup, "^English Language") ~ el_2018,
        #             str_detect(subgroup, "^Hawaii") ~ hpi_2018)
      ) %>%
      inner_join(
        accountability_2018 %>%
          filter(
            designation_ineligible == 0,
            indicator == "Achievement"
            # n_count >= 30 (This filter happens below.)
          ) %>%
          distinct(system, school, subgroup),
        by = c("system", "school", "subgroup")
      ) %>%
      # Sum student counts across subjects and grade levels.
      group_by(system, school, subgroup, year) %>%
      summarize(
        n_otm = sum(n_ontrack_prof) + sum(n_mastered_adv),
        n_total = sum(valid_tests)
      ) %>%
      ungroup() %>%
      mutate(success_rate = 100 * n_otm / n_total)
  ) %>%
  filter(subgroup %in% student_groups_tsi) %>%
  # Q: Is it okay to assign each school its 2018 pool?
  left_join(
    accountability_2018 %>% select(system, school, pool) %>% distinct(),
    by = c("system", "school")
  ) %>%
  mutate(
    n_otm = if_else(n_total == 0, 0, n_otm),
    success_rate = if_else(n_total == 0, NA_real_, success_rate)
  ) %>%
  select(system, school, pool, subgroup, year, everything()) %>%
  arrange(system, school, subgroup, year)

t <- success_rate_group
stopifnot(n_distinct(t$system, t$school, t$pool, t$subgroup, t$year) == nrow(t))
rm(t)

success_rate_group <-
  success_rate_group %>%
  # Q: Here should I filter out schools without three years of data?
  # mutate(year_count = 1) %>%
  group_by(system, school, pool, subgroup) %>%
  summarize_at(vars(n_otm, n_total), sum) %>% # Potentially add year_count.
  # Q: Is it correct to filter out small groups AFTER pooling across years?
  filter(n_total >= 30) %>%
  mutate(
    success_rate =
      if_else(n_total == 0, NA_real_, 100 * n_otm / n_total + 1e-9)
  ) %>%
  group_by(pool, subgroup) %>%
  mutate(
    rank = rank(success_rate, na.last = "keep", ties.method = "min"),
    # Q: Which schools should we include in this denominator?
    n_schools = n(),
    subgroup_score_percentile = 100 * rank / n_schools
  ) %>%
  ungroup() %>%
  arrange(system, school, subgroup)

# Check for consistent student groups.
s <-
  success_rate_group %>%
  distinct(subgroup) %>%
  arrange(subgroup) %>%
  extract2("subgroup")

stopifnot(s == student_groups_tsi)
rm(s)

# Q: To remove ASD schools not in the bottom 5%, I removed non-Priority ASD
# schools. Was this correct? (I was a bit confused by the percent ranks greater
# than 5.)

csi_success_rate_cutoffs <-
  schools_csi_2018 %>%
  # Q: Is this filter correct?
  # This filter is a conservative step to make sure we don't base the HS
  # cut-off on a school not identified for success rate. Adding this filter
  # reduced the number of ATSI schools from 59 to 55.
  filter(is.na(grad_less_than_67)) %>%
  left_join(
    schools_priority_2018 %>% select(system, school, priority),
    by = c("system", "school")
  ) %>%
  filter(system != 985 | priority == 1) %>%
  group_by(pool) %>% 
  summarize(cutoff = max(pct_on_mastered, na.rm = T)) %>% 
  ungroup()

atsi_group <-
  # Rule 1, Line 77: ATSI from TSI
  focus_group %>%
  # filter(elig_atsi == 1) %>%
  left_join(
    success_rate_group %>%
      select(system, school, pool, subgroup, success_rate),
    by = c("system", "school", "pool", "subgroup")
  ) %>%
  left_join(csi_success_rate_cutoffs, by = "pool") %>%
  # Rule 2, Line 78: Student Group Would Be in Bottom 5% of Pool
  mutate(atsi = as.numeric(elig_atsi == 1 & success_rate <= cutoff)) %>%
  group_by(year, system, school) %>%
  filter(max(atsi) == 1) %>%
  ungroup() %>%
  # select(year:subgroup_score, subgroup_score_percentile, atsi) %>%
  arrange(system_name, school_name, subgroup)

# 57 ATSI schools across 28 districts
n_distinct(atsi_group$system)
n_distinct(atsi_group$system, atsi_group$school)
mean(atsi_group$atsi)

atsi_group %>%
  group_by(subgroup) %>%
  summarize(n_atsi = sum(atsi)) %>%
  ungroup() %>%
  ggplot(aes(x = subgroup, y = n_atsi)) +
  geom_col() +
  geom_text(aes(label = n_atsi), hjust = -0.25) +
  coord_flip()

# Priority Exit ---------------------------------------------------------------

priority_success_rate_cutoff_hs <-
  schools_priority_2018 %>%
  filter(pool == "HS", is.na(grad_less_than_67)) %>%
  summarize(max = max(percentile)) %>%
  extract2("max")

priority_exit_success_grad <-
  accountability_2018 %>%
  # All Students for Success Rates and Graduation Rate
  # Rule 1, Line 37
  # Rule 2, Line 39
  filter(
    indicator %in% c("Achievement", "Graduation Rate"),
    subgroup == "All Students"
    # Q: Should I filter out designation-ineligible schools here?
  ) %>%
  group_by(indicator) %>%
  nest() %>%
  mutate(
    data = set_names(data, nm = c("achievement", "grad_rate")),
    data =
      map_at(
        data, "achievement",
        ~ .x %>%
          group_by(pool) %>%
          mutate(
            rank = rank(metric, na.last = "keep", ties.method = "min"),
            n_schools = sum(!is.na(metric)),
            percent_rank = 100 * rank / n_schools,
            rank_prior = rank(metric_prior, na.last = "keep", ties.method = "min"),
            n_schools_prior = sum(!is.na(metric_prior)),
            percent_rank_prior = 100 * rank_prior / n_schools_prior
          ) %>%
          ungroup() %>%
          select(system, school, metric, metric_prior, rank:percent_rank_prior) %>%
          rename(success_rate = metric, success_rate_prior = metric_prior)
      ),
    data =
      map_at(
        data, "grad_rate",
        ~ .x %>%
          transmute(
            system,
            school,
            grad_rate = metric,
            grad_rate_prior = metric_prior,
            exit_grad = as.numeric(pmin(metric, metric_prior) >= 67)
          )
      )
  ) %>%
  extract2("data") %>%
  reduce(full_join) %>%
  inner_join(
    schools_priority_2018 %>%
      select(system, school, pool, percentile, grad_less_than_67)
  ) %>%
  mutate(
    # Q: Is this line necessary (for consistency with the designation file)?
    # Change to percent_rank_prior = percentile for actual coding.
    percent_rank = percentile,
    # Q: Which high schools were identified only for grad rate?
    # Here the current approach is to use the maximum SR percentile of high
    # schools not identified for GR. This approach will fail if the
    # maximum SR percentile actually belongs to a high school that WAS
    # identified for GR, too.
    hs_priority_pathway =
      case_when(pool == "K8" ~ NA_character_,
                is.na(grad_less_than_67) ~ "Success Rate Only",
                percentile > priority_success_rate_cutoff_hs ~ "Graduation Rate Only",
                T ~ "Both"),
    # Rule 1, Line 37: Two Years above 10th Percentile
    exit_10 =
      if_else(is.na(hs_priority_pathway) | hs_priority_pathway != "Graduation Rate Only",
              as.numeric(percent_rank > 10 & percent_rank_prior > 10),
              NA_real_),
    # Rule 2, Line 39: One Year above 15th Percentile
    exit_15 =
      if_else(is.na(hs_priority_pathway) | hs_priority_pathway != "Graduation Rate Only",
              as.numeric(percent_rank > 15),
              NA_real_),
    # Rule 4, Line 43: Graduation Rate at Least 67 for Two Years
    exit_grad =
      if_else(is.na(grad_less_than_67), NA_real_, exit_grad)
  ) %>%
  select(
    system, school, pool, percentile, success_rate, success_rate_prior,
    percent_rank, percent_rank_prior, grad_rate, grad_rate_prior,
    hs_priority_pathway, exit_10, exit_15, exit_grad
  ) %>%
  arrange(system, school)

priority_exit_tvaas <-
  list(tvaas_level_2018, tvaas_level_2017) %>%
  map(
    ~ .x %>%
      rename_all(funs(gsub("school_wide_", "", .))) %>%
      transmute(
        system = as.numeric(district_number),
        school = as.numeric(school_number),
        # Rule 3, Line 41: TVAAS 4 or 5 in All Content Areas for Two Years
        # Q: Which content areas count for our purposes here?
        exit = as.numeric(pmin(literacy, numeracy) > 3)
      ) %>%
      inner_join(schools_priority_2018 %>% select(system, school))
  ) %>%
  reduce(left_join, by = c("system", "school"), suffix = c("_18", "_17")) %>%
  left_join(
    transmute(priority_exit_success_grad,
              system, school, hs_priority_pathway)
  ) %>%
  transmute(
    system,
    school,
    # Rule 3, Line 41: TVAAS 4 or 5 in All Content Areas for Two Years
    exit_tvaas =
      if_else(is.na(hs_priority_pathway) | hs_priority_pathway != "Graduation Rate Only",
              pmin(exit_18, exit_17),
              NA_real_)
  ) %>%
  arrange(system, school)

# Add a "closed" indicator to the output file. (List designated schools that
# are closed.)
# Identify pathway in output files (SSR, grad rate, or both).
priority_exit <-
  list(
    schools_priority_2018 %>% select(system:designation_ineligible),
    priority_exit_success_grad %>% select(-pool),
    priority_exit_tvaas
  ) %>%
  reduce(left_join, by = c("system", "school")) %>%
  mutate(
    exit_success_tvaas = pmax(exit_10, exit_15, exit_tvaas, na.rm = T),
    exit =
      case_when(hs_priority_pathway == "Both" ~ pmin(exit_success_tvaas, exit_grad),
                hs_priority_pathway == "Graduation Rate Only" ~ exit_grad,
                T ~ exit_success_tvaas)
  )

# 0 priority exits
summary(priority_exit$exit)

# produce a "CSI/Priority schools that did not exit" table

# Reward ----

# see accountability protocol (7/12) to answer questions
# based on most recent year of data
# overall weighted average 3.1 or higher (overall grade A)
# and not identified as Priority or Focus schools

# same methodology

# School Grades ---------------------------------------------------------------

# 1) Open school_grading_grades.csv
# 2) Assign an F to each Priority school not exited
# 3) Add a minus sign to each Focus school's grade
# 4) Cap Focus grades at B-

# Q: Should we assign an F to each CSI school or only each Priority school?
# Probably only Priority schools
grades_new <-
  grades_2018

# comms list ----

# district, school, designation (user-friendly explanations for internal users like OSI?)

# Write user-friendly versions to OSI SharePoint.
