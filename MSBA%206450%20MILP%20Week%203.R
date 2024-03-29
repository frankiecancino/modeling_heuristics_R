##https://www.rdocumentation.org/packages/Rglpk/versions/0.6-4/topics/Rglpk_solve_LP
##https://cran.r-project.org/web/packages/Rglpk/Rglpk.pdf
#install.packages("Rglpk")
library(Rglpk)
## Simple linear program.
## maximize: 2 x_1 + 4 x_2 + 3 x_3
## subject to: 
## 3 x_1 + 4 x_2 + 2 x_3 <= 60
## 2 x_1 + x_2 + 2 x_3 <= 40
## x_1 + 3 x_2 + 2 x_3 <= 80
## x_1, x_2, x_3 are non-negative real numbers
obj <- c(2, 4, 3)
mat <- matrix(c(3, 2, 1, 4, 1, 3, 2, 2, 2), nrow = 3)
dir <- c("<=", "<=", "<=")
rhs <- c(60, 40, 80)
max <- TRUE
Rglpk_solve_LP(obj, mat, dir, rhs, max = max)
## Simple mixed integer linear program.
## maximize: 3 x_1 + 1 x_2 + 3 x_3
## subject to: 
## -1 x_1 + 2 x_2 + x_3 <= 4
## 4 x_2 - 3 x_3 <= 2
## x_1 - 3 x_2 + 2 x_3 <= 3
## x_1, x_3 are non-negative integers
## x_2 is a non-negative real number
obj <- c(3, 1, 3)
mat <- matrix(c(-1, 0, 1, 2, 4, -3, 1, -3, 2), nrow = 3)
dir <- c("<=", "<=", "<=")
rhs <- c(4, 2, 3)
types <- c("I", "C", "I")
max <- TRUE
Rglpk_solve_LP(obj, mat, dir, rhs, types = types, max = max)
## Same as before but with bounds replaced by
## -Inf < x_1 <= 4
## 0 <= x_2 <= 100
## 2 <= x_3 < Inf
bounds <- list(lower = list(ind = c(1L, 3L), val = c(-Inf, 2)),
               upper = list(ind = c(1L, 2L), val = c(4, 100)))
Rglpk_solve_LP(obj, mat, dir, rhs, bounds, types, max)
## Examples from the GLPK manual
## Solver output enabled
## 1.3.1
## maximize: 10 x_1 + 6 x_2 + 4 x_3
## subject to: x_1 + x_2 + x_3 <= 100
## 10 x_1 + 4 x_2 + 5 x_3 <= 600
## 2 x_1 + 2 x_2 + 6 x_3 <= 300
## x_1, x_2, x_3 are non-negative real numbers
obj <- c(10, 6, 4)
mat <- matrix(c(1, 10, 2, 1, 4, 2, 1, 5, 6), nrow = 3)
mat
dir <- c("<=", "<=", "<=")
dir
rhs <- c(100, 600, 300)
rhs
max <- TRUE
Rglpk_solve_LP(obj, mat, dir, rhs, max = max, control = list("verbose" =
                                                               TRUE, "canonicalize_status" = FALSE))
## Simple (Dumb actually *LOL*) binary program.
## maximize: 3x + 4y.
## subject to: x >= 10 b1, y >= 10b2, b1 + b2 = 1 (only use one).
## b1 and b2 are binary.


obj <- c(3, 4, 0, 0)

mat<- matrix(c(1,0,0,0,1,0,-10,0,1,0,-10, 1), ncol = 4)
mat
dir <- c(">=", ">=", "==")
rhs <- c(0, 0, 1)
types <- c("C","C", "B", "B")
max <- FALSE
Rglpk_solve_LP(obj, mat, dir, rhs, types = types, max = max)

