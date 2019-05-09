library(data.table)
library(Matrix)

to_sparse <- function(d_table){
  
  i_list <- lapply(d_table, function(x) which(x != 0))
  counts <- unlist(lapply(i_list, length), use.names = F)
  
  sparseMatrix(
    i = unlist(i_list, use.names = F),
    j = rep(1:ncol(d_table), counts),
    x = unlist(lapply(d_table, function(x) x[x != 0]), use.names = F),
    dims = dim(d_table),
    dimnames = list(NULL, names(d_table)))
}

# simple, small-scale test

dt <- data.table(a = c(0, 0, 0, 1, 22, 0, 0, 1),
                 b = c(1, 0, 0, 0, -2, 1, 0, 0))

str(to_sparse(dt))

to_sparse(dt)
