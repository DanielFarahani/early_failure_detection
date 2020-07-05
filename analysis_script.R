
# Setup -------------------------------------------------------------------
library(janitor)
library(cluster)
library(tidyverse)
library(readxl)
library(summarytools)
library(Rtsne)
library(skimr)
library(forcats)

# Read in data ---------------------------------------------------------------
# reading in raw data 
cohort_res_raw <- read_csv("data/cohort_res_raw.csv") %>%
  clean_names()

# Take and clean features of interest
cohort_cleaned <- cohort_res_raw %>%
  select(
    uid,
    gpa,
    gpa_course_count,
    age_group,
    gender,
    is_indigenous_flag,
    is_disabled_flag,
    disability_advice,
    first_in_family,
    catering_type,
    owner,
    religious_affiliation,
    weekly_tariff_grouping,
    program_career,
    field_of_education_broad_description,
    program_type,
    program_plan_group,
    appointment_flag,
    home_state,
    home_australia_ses,
    home_low_ses,
    home_metropolitan,
    home_regional,
    home_remote,
    basis_of_admission,
    attendance_type,
    citizenship,
    entrance_score_group_5,
    drive_time_band2,
    v_permit_no_days,
    cello_parked_flag,
    cello_parking_days,
    academic_skills_booked,
    academic_skills_dropin
  ) %>%
  replace_na(
    list(
      home_metropolitan = 0,
      home_regional = 0,
      home_remote = 0,
      cello_parking_days = 0,
      academic_skills_booked = 0,
      academic_skills_dropin = 0,
      v_permit_no_days = 0,
      hall_residence = "Off campus",
      catering_type = "off campus",
      owner = "off campus",
      religious_affiliation = "unknown",
      weekly_tariff_grouping = "off campus",
      field_of_education_broad_description = "09 - Society and Culture"
    )
  ) %>%
  mutate(
    home_overseas = ifelse(home_state == "Overseas", 1, 0),
    is_local = ifelse(home_state == "ACT", 1, 0),
    is_minority = ifelse(
      home_australia_ses == "Low" |
        basis_of_admission == "Mature age special entry" |
        is_indigenous_flag == 1 |
        is_disabled_flag == 1 |
        first_in_family == 1,
      yes = 1,
      no = 0
    ),
    field_of_education_broad_description = ifelse(
      field_of_education_broad_description == "00 - Not Allocated",
      yes = "03 - Engineering and Related Technologies",
      no = field_of_education_broad_description
    ),
    program_plan_group = fct_recode(
      program_plan_group, 
      double = "Flexible Doubles", 
      double = "Vertical Double"),
    religious_affiliation = fct_recode(
      religious_affiliation,
      religious = "Anglican",
      religious = "Christian"
    ),
    age_group = ifelse(age_group == "< 17 Years", "17 below Years", age_group),
    age_group = ifelse(age_group == "> 59 Years", "59 above Years", age_group)
  ) %>%
  mutate_at(
    c(
      "cello_parked_flag",
      "disability_advice",
      "first_in_family",
      "home_low_ses",
      "is_disabled_flag",
      "is_indigenous_flag",
      "appointment_flag",
      "is_local",
      "is_minority"
    ),
    as.factor
  ) %>%
  mutate_if(is.character, as.factor) %>%
  mutate(age_group = factor(age_group, ordered = TRUE),
         drive_time_band2 = factor(drive_time_band2, ordered = TRUE),
         entrance_score_group_5 = factor(entrance_score_group_5, ordered = TRUE),
         home_australia_ses = factor(home_australia_ses, ordered = FALSE),
         uid = as.character(uid),
         entrance_score_group_5 = fct_collapse(
           entrance_score_group_5,
           "64 below" = c("30 - 34", "35 - 39", "40 - 44", "45 - 49", "50 - 54", "55 - 59", "60 - 64"))
  ) %>%
  mutate(
    age_group2 = fct_collapse(
      age_group,
      "24 below Years" = c(
        "17 below Years",
        "17 Years",
        "18 Years",
        "19 Years",
        "20 Years",
        "21 Years",
        "22 Years",
        "23 Years",
        "24 Years"
      ),
      "25 - 29 Years" = c("25 Years", "26 Years", "27 Years", "28 Years", "29 Years"),
      "50 above Years" = c("50 - 59 Years", "59 above Years")
    )
  ) %>% 
  mutate(age_group2 = factor(
    age_group2,
    ordered = TRUE,
    levels = c(
      "24 below Years",
      "25 - 29 Years",
      "30 - 39 Years",
      "40 - 49 Years",
      "50 above Years"
    )
  )) %>%
  filter(!is.na(home_state))


# summayr peek ------------------------------------------------------------

skim(cohort_cleaned)
cohort_cleaned %>% freq(appointment_flag, order="levels")
ordered(cohort_cleaned$age_group2) 


# Clustering preparation -------------------------------------------------------

# data subset-ing
cohort_subset <- cohort_cleaned %>% 
  select(program_career, is_minority, appointment_flag, is_local)  # remove appointment_flag

# gender, first_in_family, home_australia_ses, academic_skills_booked, academic_skills_dropin, is_indigenous_flag, is_disabled_flag, is_local #all minorities manual
# academic_skills_booked, academic_skills_dropin, cello_parked_flag, gpa_course_count, home_metropolitan, home_overseas, home_regional, home_remote, v_permit_no_days # numeric
# program_career, field_of_education_broad_description, gender, first_in_family, drive_time_band2, home_overseas # interesting ones
# attendance_type, gender, citizenship, program_career, first_in_family, owner # generic 


# distance matrixs
diss_matrix <- daisy(cohort_subset, metric = "gower")

# Cluster performance -----------------------------------------------------

### PAM
search_gird <- 2:4

search <- tibble(cluster = search_gird) %>% 
  mutate(ave_sw = map_dbl(.x = cluster, 
                          .f = ~pam(x = diss_matrix, 
                                    k = .x, 
                                    diss = TRUE)$silinfo$avg.width))

# graph output of silhouette coefficient
ggplot(search, aes(cluster, ave_sw)) +
  geom_line() +
  labs(title = "Which value of k returns the best silhouette coefficient",
       subtitle = "Using a heuristic we can pick the Min value of k that gives a reasonable result",
       y = "Average Silhouette Coefficient",
       x = "Number of clusters (k)")

# Clustering --------------------------------------------------------------

pam_fit <- pam(diss_matrix, k = 13, diss = TRUE)
summary(pam_fit)


# Visualisation -------------------------------------------------------------------


# attaching cluster labels
cohort_clustered <- cohort_cleaned %>% 
  mutate(cluster = pam_fit$clustering)


# Embed clusters in 2D for visualisation
tsne_obj <- Rtsne(X = diss_matrix, is_distance = TRUE)

# extract projected data points, add cluster assingment and make data frame
tsne_data <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(cohort_clustered$cluster),
         label = cohort_clustered$program_career) 

# create mean of each cluster to add as centroid
tsne_summ <- tsne_data %>% 
  group_by(cluster) %>% 
  summarise(X = mean(X), 
            Y = mean(Y))

# create convex hulls 
hulls <- tsne_data %>% 
  group_by(cluster) %>% 
  slice(chull(X, Y))

# plot the clusters and nets
tsne_data %>% 
  ggplot(aes(x = X, y = Y)) +
  geom_point(aes(color = cluster), show.legend = FALSE) +
  geom_text(data = tsne_summ, aes(X, Y, label = cluster), size = 7) +
  #  geom_text(aes(X, Y, label = label), check_overlap = TRUE) +
  geom_polygon(data = hulls, aes(X, Y, fill = cluster), alpha = 0.7, show.legend = FALSE) +
  theme_minimal() +
  labs(title = "Cluster results of cohor data",
       subtitle = "picking n clusters") +
  theme(panel.grid = element_blank(), 
        axis.text = element_blank(), 
        axis.title = element_blank())


cohort_clustered %>%  
  group_by(cluster) %>% 
  dfSummary(plain.ascii = FALSE, style = "grid", 
            graph.magnif = 0.75, valid.col = FALSE)

# prototypes
prototypes <- cohort_clustered[pam_fit$medoids,]

# save
write_csv(cohort_clustered, 'data/cohort_clustered.csv')
write_csv(prototypes, 'data/proto.csv')


# cluster comparisons -----------------------------------------------------

tapply(cohort_clustered$gpa, cohort_clustered$cluster, summary)
  cohort_clustered %>%
  split(.$cluster) %>% 
  map(summary)

# archive -----------------------------------------------------------------

# clustered summary
print(by(data = cohort_clustered, INDICES = cohort_clustered$cluster, FUN = dfSummary), method = 'browser')

