# test new against old pre processing method 

data(voters)
lm_fit <- lm(proposition_vote ~ treatment, data = voters)

object <- lm_fit
clustid <- reformulate(c("group_id1", "group_id2"))
clustid_char <- c("group_id1", "group_id2")
#bootcluster <- c("group_id1", "state")
bootcluster <- "min"
N <- nobs(object)

old <- fwildclusterboot:::preprocess(
  object = object,
  cluster = clustid_char,
  fe = NULL, 
  param = "treatment",
  bootcluster = bootcluster,
  na_omit = FALSE,
  R = 1,
  boot_algo = "R"
)

new <- fwildclusterboot:::preprocess2.lm(
  object = object, 
  clustid = clustid_char, 
  R = 1, 
  param = "treatment",
  bootcluster = bootcluster
)




check_names <- names(new)[names(new) %in% names(old)]

names(new)[!(names(new) %in% names(old))]

for(x in check_names){
  cat(x, "\n")
  print(all.equal(new[[x]], old[[x]]))
}

#new$clustid_dims
#old$clustid_dims

head(new[["bootcluster"]])
head(old[["bootcluster"]])

new[["clustid_dims"]]
