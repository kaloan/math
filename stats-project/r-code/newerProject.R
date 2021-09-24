library("xtable")

as_matrix <- function(x){
  if(!tibble::is_tibble(x) ) stop("x must be a tibble")
  y <- as.matrix.data.frame(x[,-1])
  rownames(y) <- x[[1]]
  y
}

modeFunction <- function(x) {
  tt <- table(x)
  return(as.double(names(tt)[tt == max(tt)]))    
}

csex <- function(x) {
  temp<-0;
  for(it in x){
    if(it=='f') temp=temp+1;
  }
  return(temp)
}

temp=0;
for(k in ais[3])
{
  if(k=='f') temp=temp+1;
}

ais <- read.csv("C:/Users/kaloa/Downloads/ais.csv")

printInfo<-function(x){
  print(mean(x))
  print(median(x))
  #print(modeFunction(x))
  print(var(x))
  print(sd(x))
}

getInfo<-function(x){
  c(mean(x), median(x), modeFunction(x), var(x), sd(x))
  #c(mean(x), median(x), var(x), sd(x))
}


csex2 <- function(x,y) {
  temp<-0;
  for(it in x){
    if(it==y) temp=temp+1;
  }
  return(temp)
}


aisf=ais[ais$sex=='f',]
aism=ais[ais$sex=='m',]


fullInfo<-function()
{
  lbmAInfo <- getInfo(ais$lbm)
  wccAInfo <- getInfo(ais$wcc)
  lbmMInfo <- getInfo(aism$lbm)
  wccMInfo <- getInfo(aism$wcc)
  lbmFInfo <- getInfo(aisf$lbm)
  wccFInfo <- getInfo(aisf$wcc)
  info <- matrix(nrow=6, ncol=4)
  info[1,] <- lbmAInfo
  info[2,] <- wccAInfo
  info[3,] <- lbmMInfo
  info[4,] <- wccMInfo
  info[5,] <- lbmFInfo
  info[6,] <- wccFInfo
  print(xtable(info),file = "introInfo.tex")
}


seq(min(values), max(values))


hist(ais$lbm)
hist(ais$lbm,main="Histogram of all lbm",xlab="lbm")



hist(ais$lbm,main="lbm-всички",xlab="lbm",ylab="Честота",col="green")

hist(aisf$lbm,main="lbm-жени",xlab="lbm",ylab="Честота",col="pink")
hist(aism$lbm,main="lbm-мъже",xlab="lbm",ylab="Честота",col="blue")


hist(ais$lbm,main="lbm-всички",xlab="lbm",ylab="Честота",col="green")
hist(aisf$lbm,main="lbm-жени",xlab="lbm",ylab="Честота",col="pink")
hist(aism$lbm,main="lbm-мъже",xlab="lbm",ylab="Честота",col="blue")

hist(ais$lbm,main="lbm-всички",xlab="lbm",ylab="Честота",col="green",breaks = 16,xlim=c(30,110),
     ylim=c(0,35))
axis(side=1, at=seq(30,110,5))
axis(side=2,at=seq(0,35,5))

hister<-function(info,mname,xname,yname,hcol,xl,yl,xm,ym,xint,yint,brnum)
{
  hist(info,main=mname,xlab=xname,ylab=yname,col=hcol,breaks = brnum,xlim=c(xl,xm),
       ylim=c(yl,ym))
  axis(side=1,at=seq(xl,xm,xint))
  axis(side=2,at=seq(yl,ym,yint))
}

hister<-function(info,mname,xname,yname,hcol,xl,yl,xm,ym,xint,yint)
{
  hist(info,main=mname,xlab=xname,ylab=yname,col=hcol,
       breaks=seq(xl,xm,xint),xlim=c(xl,xm),ylim=c(yl,ym))
  
  axis(side=1,at=seq(xl,xm,xint))
  axis(side=2,at=seq(yl,ym,yint))
}

histerw<-function(info,mname,xname,yname,hcol,xl,yl,xm,ym,xint,yint)
{
  hist(info,main=mname,xlab=xname,ylab=yname,col=hcol,
       breaks = seq(xl,xm,xint),xlim=c(xl,xm),ylim=c(yl,ym))
  
  axis(side=1, at=seq(xl,xm,2*xint))
  axis(side=2,at=seq(yl,ym,yint))
}

hister(ais$lbm,"lbm-всички","lbm","Честота","green",30,0,110,35,5,5)
hister(aisf$lbm,"lbm-жени","lbm","Честота","pink",30,0,110,35,5,5)
hister(aism$lbm,"lbm-мъже","lbm","Честота","blue",30,0,110,35,5,5)


histerw(ais$wcc,"Koнцентрация бели кръвни телца-всички","wcc","Честота","green",3.0,0,15,35,0.5,5)
histerw(aisf$wcc,"Koнцентрация бели кръвни телца-жени","wcc","Честота","pink",3.0,0,15,35,0.5,5)
histerw(aism$wcc,"Koнцентрация бели кръвни телца-мъже","wcc","Честота","blue",3.0,0,15,35,0.5,5)

hister(ais$wcc,"Koнцентрация бели кръвни телца-всички","wcc","Честота","green",3.0,0,14.5,35,0.5,5)
hister(aisf$wcc,"Koнцентрация бели кръвни телца-жени","wcc","Честота","pink",3.0,0,14.5,35,0.5,5)
hister(aism$wcc,"Koнцентрация бели кръвни телца-мъже","wcc","Честота","blue",3.0,0,14.5,35,0.5,5)

hister(ais$wcc,"Koнцентрация бели кръвни телца-всички","wcc","Честота","green",3.0,0,14.5,35,0.75,5)
hister(aisf$wcc,"Koнцентрация бели кръвни телца-жени","wcc","Честота","pink",3.0,0,14.5,35,0.75,5)
hister(aism$wcc,"Koнцентрация бели кръвни телца-мъже","wcc","Честота","blue",3.0,0,14.5,35,0.75,5)


hist(ais$wcc)


hist(aisf$lbm,main="lbm-жени",xlab="lbm",ylab="Честота",col="pink",breaks = 20)
hist(aism$lbm,main="lbm-мъже",xlab="lbm",ylab="Честота",col="blue",breaks = 20)



shapiro.test(ais$lbm)
shapiro.test(ais$wcc)

shapiro.test(aisf$lbm)
shapiro.test(aisf$wcc)

shapiro.test(aism$lbm)
shapiro.test(aism$wcc)



t.test(aism$lbm,aisf$lbm,alternative = "greater")


len=(max(ais$wcc)-min(ais$wcc))/0.1+1;

mn=vector(mode = "integer", length = len)
fn=vector(mode = "integer", length = len)

for(k in 1:length(aisf$wcc))
{
  place=((aisf$wcc[k]-3.3)/0.1)+1;
  fn[place]=fn[place]+1;
}

for(k in 1:length(aism$wcc))
{
  place=((aism$wcc[k]-3.3)/0.1)+1;
  mn[place]=mn[place]+1;
}

fnprime=fn/length(aisf$wcc);
mnprime=mn/length(aism$wcc);

chisq.test(fn,mn)
chisq.test(mn,p=fnprime)

wilcox.test(aisf$wcc,aism$wcc)


carr2=array(dim=c(2,111));
carr2[1,]=fn;
carr2[2,]=mn;

carr=array(dim=c(2,111));
carr[,1]=fn;
carr[,2]=mn;




cor(ais$lbm,ais$wcc)
cor(aisf$lbm,aisf$wcc)
cor(aism$lbm,aism$wcc)

cor(ais$lbm,ais$wcc,method="spearman")
cor(aisf$lbm,aisf$wcc,method="spearman")
cor(aism$lbm,aism$wcc,method="spearman")


qqplot(ais$lbm,ais$wcc)
qqplot(aisf$lbm,aisf$wcc)
qqplot(aism$lbm,aism$wcc)


qqer<-function(info1,info2,mname,xname,yname,xl,yl,xm,ym,xint,yint)
{
  qqplot(info1,info2,main=mname,xlab=xname,ylab=yname)
  axis(side=1,at=seq(xl,xm,2*xint))
  axis(side=2,at=seq(yl,ym,2*yint))
}

qqer(ais$lbm,ais$wcc,"QQPLOT-всички","lbm","wcc",30,3.0,110,14.5,5,0.5)
qqer(aisf$lbm,aisf$wcc,"QQPLOT-жени","lbm","wcc",30,3.0,110,14.5,5,0.5)
qqer(aism$lbm,aism$wcc,"QQPLOT-мъже","lbm","wcc",30,3.0,110,14.5,5,0.5)









qall=array(dim=c(2,202));
qall[1,]=ais$lbm;
qall[2,]=ais$wcc;


gger<-function(info,mname,xname,yname,xl,yl,xm,ym,xint,yint)
{
  gp=ggplot(info,aes(x=yname,y=xname))+geom_point()+coord_flip()
  gp=gp+ggtitle(mname)
  gp=gp+scale_x_continuous(limits=c(xl, xm)) +
    scale_y_continuous(limits=c(yl, ym))
  gp
}

gger<-function(info,mname,xname,yname,xl,yl,xm,ym,xint,yint)
{
  gp=ggplot(info,aes(x=yname,y=xname))+geom_point()+coord_flip()
  gp=gp+ggtitle(mname)
  gp=gp+scale_x_discrete(limits=c(xl, xm)) +
    scale_y_discrete(limits=c(yl, ym))
  gp
}



gger<-function(info,mname,xname,yname,xl,yl,xm,ym,xint,yint)
{
  gp=ggplot(info,aes(x=xname,y=yname))+geom_point()
  gp=gp+ggtitle(mname)
  gp=gp+scale_x_discrete(limits=c(xl, xm)) +
    scale_y_discrete(limits=c(yl, ym))
  gp
}


ggplot(ais, aes(x = wcc, y = lbm))+geom_point()+ggtitle("Всички")
ggplot(aisf, aes(x = wcc, y = lbm)) + geom_point()+
ggtitle("Жени")
ggplot(aism, aes(x = wcc, y = lbm)) + geom_point()+
ggtitle("Мъже")




gger(ais,"Scatter-всички","lbm","wcc",30,3.0,110,14.5,5,0.5)
qqer(aisf,"Scatter-жени","lbm","wcc",30,3.0,110,14.5,5,0.5)
qqer(aism,"Scatter-мъже","lbm","wcc",30,3.0,110,14.5,5,0.5)

plot(ais$lbm,ais$wcc)

scatterer<-function(info1,info2,mname,xname,yname,scol,
                    xl,yl,xm,ym,xint,yint,alpha)
{
  plot(info1,info2,main=mname,xlab=xname,ylab=yname, col=scol,
       xlim=c(xl,xm),ylim=c(yl,ym))
  axis(side=1,at=seq(xl,xm,2*xint))
  axis(side=2,at=seq(yl,ym,2*yint))
  
  regression=lm(info2~info1)
  regsum=summary.lm(regression)
  pvalue=regsum$coefficients[2,4]
  
  if(pvalue<=alpha) abline(regression,col=scol)
  else print("Linear regression is not significant")
}

regralpha=0.05
scatterer(ais$lbm,ais$wcc,"Всички","lbm","wcc","green",30,3.0,110,14.5,5,0.5,regralpha)
scatterer(aisf$lbm,aisf$wcc,"Жени","lbm","wcc","pink2",30,3.0,110,14.5,5,0.5,regralpha)
scatterer(aism$lbm,aism$wcc,"Мъже","lbm","wcc","blue",30,3.0,110,14.5,5,0.5,regralpha)
