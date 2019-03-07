
file_loc = "/Users/behnam/Documents/kaggle/insurance/data/"
file_name = "opt-2017-10-26.rds"
df = readRDS(paste0(file_loc, file_name))
str(df)
pc = df$params
M = df$eval_met
head(M)
M = M[1:62,]
inx = which(M[, 4] == min(M[, 4]))
print(inx)
M[inx,]
pc[inx,]

plot(M[, 4], type='l', ylim = c(.14,.16))
lines(M[,4], col = 'red')
