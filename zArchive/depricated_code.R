
# reading in files  -------------------------------------------------------


# reading hall res info
res_info_raw <- read_csv("data/hall_res_info.csv") %>%
  clean_names()
# joining raw data to res info
cohort_joined <- cohort_raw %>% 
  left_join(res_info_raw, by = "hall_residence") %>% 
  clean_names()


# summary info ------------------------------------------------------------


cohort_cleaned %>%
  select(uid,
         home_metropolitan,
         home_regional,
         home_remote,
         home_overseas) %>%
  gather(var, metric,-uid) %>%
  ggplot(aes(metric)) +
  geom_histogram(binwidth = 0.2) +
  facet_wrap( ~ var, scales = "free")


cohort_cleaned %>%
  select(uid, hall_residence, in_room_flag) %>%
  count(hall_residence, sort = TRUE) %>%
  ggplot(aes(hall_residence, n)) +
  geom_col() +
  coord_flip()


# parallel attempt
install.packages("doSMP") 
rmSessions(all.names=TRUE)
workers = startedWorkers(4, registeredDoSMP(workers))
# TODO parallel processing
fill <- tibble(cluster = 2:20)
search = foreach(i=2:20) %dopar% {
  fill <- mutate(fill, ave_sw = map_dbl(.x = cluster, 
                                        .f = ~pam(x = diss_matrix, 
                                         k = .x, 
                                         diss = TRUE)$silinfo$avg.width))
}
fill = unlist(fill)


# parallel testing

library(parallel)
library(MASS)

x <- iris[which(iris[,5] != "setosa"), c(1,5)]
trials <- seq(1, 10000)
boot_fx <- function(trial) {
  ind <- sample(100, 100, replace=TRUE)
  result1 <- glm(x[ind,2]~x[ind,1], family=binomial(logit))
  r <- coefficients(result1)
  res <- rbind(data.frame(), r)
}
system.time({
  results <- mclapply(trials, boot_fx, mc.cores = numCores)
})