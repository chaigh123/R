# 创建数据
x1 = matrix(rep(c(1,0,0,0),40),nrow = 4)
x2 = matrix(rep(c(1,0,0,1),58),nrow = 4)
x3 = matrix(rep(c(1,0,1,0),2),nrow = 4)
x4 = matrix(rep(c(1,0,1,1),18),nrow = 4)
x5 = matrix(rep(c(1,1,0,0),9),nrow = 4)
x6 = matrix(rep(c(1,1,0,1),26),nrow = 4)
x7 = matrix(rep(c(1,1,1,1),98),nrow = 4)
x = cbind(x1,x2,x3,x4,x5,x6,x7)
x = t(x)
y1 = rep(0,32)
y2 = rep(1,8)
y3 = rep(0,30)
y4 = rep(1,28)
y5 = rep(0,2)
y6 = rep(0,17)
y7 = rep(1,1)
y8 = rep(0,9)
y9 = rep(0,3)
y10 = rep(1,23)
y11 = rep(0,87)
y12 = rep(1,11)
y = c(y1,y2,y3,y4,y5,y6,y7,y8,y9,y10,y11,y12)

h <- function(x){
  exp(x)/(1+exp(x))
}

h1 <- function(x){
  exp(x)/(1+exp(x))^2
}

# x,y为样本数据，beta为迭代初值，e为精度，k为最大迭代次数。
f1 <- function(x,y,beta,e,k_m) {
  a = 1
  k = 0
  beta = t(t(beta)) # 将输入的beta转换成列向量,没办法，要转两次
  while (a > e & k < k_m) {
    z_beta = x %*% beta
    h_z_beta = h(z_beta)
    h1_z_beta = h1(z_beta)
    F_beta = 0
    for (i in 1:length(y)) {
      F_beta = F_beta + x[i,] %*% t(x[i,]) *(h1_z_beta[i])^2/(h_z_beta[i]*(1-h_z_beta[i]))
    }
    s_beta = 0
    for (i in 1:length(y)) {
      s_beta = s_beta + x[i,]*h1_z_beta[i]*(y[i]-h_z_beta[i])/(h_z_beta[i]*(1-h_z_beta[i]))
    }
    s_beta = t(t(s_beta))# 这里也是，要转两次
    a = sum((solve(F_beta)%*%s_beta)^2)/sum(beta^2)#这里要先计算a，因为下一步beta就变啦
    beta = beta + solve(F_beta)%*%s_beta
    k = k+1
  }
  if (k == k_m) {
    print('迭代失败')
  }
  return(c('beta_hat:',list(beta),'迭代次数:',k,'迭代精度:',a))
}

m = 10000

f1(c(-1,-1,-1,1),1/m,1000000)# 选择合适的beta0很重要，报错就是因为选的不对。


