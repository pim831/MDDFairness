##
# File containing helper functions.
##

##
# Statistical Parity Difference (SPD) [-1,1]:
# (TPP+FPP)/(NP)-(TPU+FPU)/NU
##
compute_spd <- function(cmp, cmnop) {
  return (((cmp$TP+cmp$FP)/cmp$N) - ((cmnop$TP+cmnop$FP)/cmnop$N))
}

# mode function
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
