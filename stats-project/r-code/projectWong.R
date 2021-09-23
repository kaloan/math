Wong <- read_csv("C:/Users/kaloa/Downloads/Wong.csv", col_types = cols(X1 = col_skip()))
View(Wong)
Wong
for (pp in Wong) {
  pp
}

as_matrix <- function(x){
  if(!tibble::is_tibble(x) ) stop("x must be a tibble")
  y <- as.matrix.data.frame(x[,-1])
  rownames(y) <- x[[1]]
  y
}

z=as_matrix(Wong)
z

Wong[Wong$id %in% Wong$id[duplicated(Wong$id)],]

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

elements <- read.csv("C:/Users/kaloa/Downloads/ais.csv")

getInfo<-function(x){
  print(mean(x))
  print(median(x))
  print(modeFunction(x))
  print(var(x))
  print(sd(x))
}

mean(ais$wcc)
modeFunction(ais$wcc)
median(ais$wcc)
sd(ais$wcc)
var(ais$wcc)


csex2 <- function(x,y) {
  temp<-0;
  for(it in x){
    if(it==y) temp=temp+1;
  }
  return(temp)
}


aisf=ais[ais$sex=='f',]
aism=ais[ais$sex=='m',]

hist(ais$lbm)
hist(ais$lbm,main="Histogram of all lbm",xlab="lbm")
hist(ais$lbm,main="Histogram of all lbm",xlab="lbm",ylab="Freq")

shapiro.test(ais$wcc)
shapiro.test(aisf$wcc)
shapiro.test(aism$wcc)

shapiro.test(ais$lbm)
shapiro.test(aisf$lbm)
shapiro.test(aism$lbm)

hist(aism$wcc)



mapply(function(x) if(ceiling(x)==floor(x)) as.integer(x) else x,
  c(1,1.5,3.5,5,12.3))