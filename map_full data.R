#worldmap (total data)

a <- read.table("metadata-2.tsv", header = T, sep = "\t", quote = "\"", na.strings = c("", "?"), comment.char = "")
names(a)


getCases <- function(cont){
  f <- sum(a$region == cont & a$sex == "Female", na.rm=TRUE)
  m <- sum(a$region == cont & a$sex == "Male", na.rm=TRUE)
  return(list(males=m, females=f))
}

unique(a$region)

as<-getCases("Asia")
af<-getCases("Africa")
eu <- getCases("Europe")


getPvalue <- function(cont){
  pbinom(q=cont$males-1, size=cont$males+cont$females, prob=0.5, lower.tail=F)
}

getPvalue(as)


total.females <- as$females + af$females + eu$females 
total.males <- as$males + af$males + eu$males

getPvalueHyper <- function(cont){
  phyper(q=cont$males-1, m=total.males, n=total.females, k = cont$males+cont$females, lower.tail = FALSE)
}

getPvalueHyper(as)
