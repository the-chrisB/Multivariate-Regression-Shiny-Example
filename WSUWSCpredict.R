
pred <- function(x)
{
  master <- read.csv('data/WSUWSCOptimize.csv', header = T, check.name = F)
  master.spec <- master[,-1:-3]
  m3g.standard <- read.csv("data/m3g standard.csv", header = T, check.name = F)
  m3g.standard <- m3g.standard[4,]

  m3g <- data.frame(master$M3Gs, master.spec[,201:471])
  colnames(m3g) <- c('M3Gs', c(430:700))
  
  m3g.prime <- master.spec[,291]/m3g.standard[,291]
  m3g.standard <- as.matrix(m3g.standard)
  m3g.prime <- as.matrix(m3g.prime)
  m3g.spec <- data.frame(m3g.prime%*%m3g.standard[,1:290], master.spec[,291:471])
  colnames(m3g.spec) <- c(c(230:700))
  
  tip.spec <- master.spec[,1:200] - m3g.spec[,1:200]
  tip.final <- data.frame(master$TIPs, tip.spec)
  colnames(tip.final) <- c("TIPs", c(230:429))

  pc.final <- data.frame(master$PCs, tip.spec)
  colnames(pc.final) <- c("PCs", c(230:429))
  
  x.m3g <- data.frame(x[,201:471])
  colnames(x.m3g) <- c(430:700)
  
  x.m3g.prime <- x[,292]/m3g.standard[,291]
  m3g.standard <- as.matrix(m3g.standard)
  x.m3g.prime <- as.matrix(x.m3g.prime)
  x.m3g.spec <- data.frame(x.m3g.prime%*%m3g.standard[,1:290], x[,291:471])
  colnames(x.m3g.spec) <- c(230:700)
  
  x.tip.spec <- x[,2:201] - x.m3g.spec[,1:200]
  x.tip <- data.frame(x.tip.spec)
  colnames(x.tip) <- c(230:429)
  
  x.pc <- data.frame(x.tip.spec)
  colnames(x.pc) <- c(230:429)

  pc.svm <- svm(PCs~., data = pc.final, cost = 0.3, kernel = "linear", type = "nu-regression")
  tip.svm <- svm(TIPs~., data = tip.final, cost = 1.0, kernel = "linear", type = "nu-regression")
  m3g.svm <- svm(M3Gs~., data = m3g, cost = 0.1, kernel = "linear", type = "nu-regression")
  
  predict.pc <- predict(pc.svm, x.pc)
  predict.tip <- predict(tip.svm, x.tip)
  predict.m3g <- predict(m3g.svm, x.m3g)
  
  sum.test <- data.frame(format(round(predict.m3g, 1), nsmall = 1), format(round(predict.pc, 1), nsmall = 1), format(round(predict.tip, 1), nsmall = 1)) 
                         
  colnames(sum.test) <- c('Anthocyanins (mg/L Malv EQ)', 'Tannins (mg/L CE)', 'Total Iron Reactive phenolics (mg/L CE)')
  
  sum.test
}

