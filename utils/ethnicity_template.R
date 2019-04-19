write.csv(
  x = data.frame(
    SUBJID = as.character(round(runif(n = 100, min = 0, max = 10^6))),
    PC01 = rnorm(n = 100),
    PC02 = rnorm(n = 100)
  ),
  file = "ethnicity_template.csv", 
  quote = FALSE, 
  row.names = FALSE
)
