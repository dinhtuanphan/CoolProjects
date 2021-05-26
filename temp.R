arr <- c(1, 2, 4, NA, NA, 11, NA, 10)
# function to impute the values
missingValue <- function(arr) {
  ifelse(is.na(arr), mean(arr, na.rm = TRUE), arr)
}
missingValue(arr)
ifelse()
merge(employeee_salary, employee_experience, by = NULL)

fac <- factor(c(2.3, 1.5, 3, 4.9))
as.numeric(fac)
levels(fac)[fac]
fac
levels(fac)
