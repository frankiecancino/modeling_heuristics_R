#minimize 8y1 + 8y2
# S.T. 
# 2y1 + y2 >= 300
# y1 + 2y2 >= 200
# y1, y2 >= 0
library(lpSolve)
f.obj <- c(8, 8)
f.obj
f.con <- matrix(c(2, 1, 1, 2), ncol=2, byrow=T)
f.con
f.dir <- c(">=", ">=")
f.dir
f.rhs <- c(300, 200)
f.rhs
lp("min", f.obj, f.con, f.dir, f.rhs)
lp("min", f.obj, f.con, f.dir, f.rhs)$solution

r1 = c(1, 2, 3)
r2 = c(3, 2, 2)
f.obj = c(1, 9 ,1)
f.con = rbind(r1, r2)
f.con
f.dir = c("<=", "<=")
f.rhs = c(9, 15)
lp("max", f.obj, f.con, f.dir, f.rhs)
lp("max", f.obj, f.con, f.dir, f.rhs)$solution

lp("max", f.obj, f.con, f.dir, f.rhs, int.vec = 1:3)
lp("max", f.obj, f.con, f.dir, f.rhs, int.vec = 1:3)$solution


