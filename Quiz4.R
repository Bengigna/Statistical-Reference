##Question 1
(Q1 <- data.frame(base=c(140,138,150,148,135),later=c(132,135,151,146,130)))
Q1
t.test(Q1$base,Q1$later,paired=T, var.equal=T, alt="two.sided")$p.value

#Q2
1100+c(-1,1)*qt(0.975,8)*30/sqrt(9)

#Q3
pbinom(2, size=4, prob=0.5, lower.tail=F)

##or Pvalue=P(X???3):choose(4,3)*0.5^3*0.5^1+choose(4,4)*0.5^4*0.5^0

#Q4
lamda<-1/100 *1787
ppois(10, lamda)

#Q5
SP<-(8*(1.5)^2+8*(1.8)^2)/(9+9-2)

ts <- (x1 - x2)/(SP * sqrt(1/n1 + 1/n2))

2 * pt(ts, n1 + n2 - 2)

#Q7
power.t.test(n=100,delta=0.01,sd=0.04,alt="one.sided", type = "one.sample",sig.level = 0.05)$power

#Q8
power.t.test(power=0.90,delta=0.01,sd=0.04,alt="one.sided", type = "one.sample",sig.level = 0.05)$n

#9
s=seq(.01, .1, by=.01)

A<-cbind(s,sapply(s, function (a) {
        power.t.test(n=100, delta=.01, sd=.04, alt="one.sided", type="one.sample", sig.level=a)$power
      })
      )


