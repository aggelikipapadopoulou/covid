install.packages("ggplot2")
library(ggplot2)
install.packages("rworldmap")
#-------------------open new data----------------
covid19data <- read.table("gisaid_hcov-19_2020_07_20_09.tsv", skip = 0, header = T, sep = "\t", quote = "\"")
dim(covid19data)
#------------------Location/Gender distribution-----------
#Location-continents 
location <- covid19data$Location
trim_n_split_continent <- function(x) {
  y <- strsplit(x, "/")[[1]][1]
  y <- trimws(y)
  return(y)
}
continent <- c() 
for (i in location){
  curr_continent <- trim_n_split_continent(i)
  continent <- c(continent, curr_continent)
}
#extra- apomonwsi xwras (gia xarti argotera)
trim_n_split_country <- function(x){
  y <- strsplit (x, "/")[[1]][2]
  y <- trimws(y)
  return(y)
}
country <- c()
for (i in location){
  curr_country <- trim_n_split_country(i)
  country <- c(country, curr_country)
}

#Gender 
gender <- covid19data$Gender
unique(gender)
f <- c("Female", "female", "Female ", "Woman")
m <- c("Male", "male", "Male ", "M")

gender_function <- function(x){
  if (is.na (x)){
    return("UNKNOWN")
  }
  else if (x %in% f){
    return("FEMALE")
  }
  else if (x %in% m){
    return("MALE")
  }
  else {
    return("UNKNOWN")
  }
}

gender_vec <- c()
for (i in gender){
 curr_gender  <- gender_function(i)
 gender_vec <- c(gender_vec, curr_gender)
}
table(gender_vec)
#ekana drop levels gia na min exw ta unknown, ALLA TA EVALE STO MALE!!
#gender_vec <- as.factor(gender_vec)
#levels(gender_vec)
#levels(gender_vec) <- droplevels(gender_vec, exclude = "UNKNOWN")

date <- covid19data$Collection.date
date <- as.Date(date)
#dataframe gia na douleuoume panw s auto. gia arxi me 4 processed columns + status general + age(rows 4412)
data_sum <- data.frame(date, continent, country, gender_vec, covid19data$Patient.status, covid19data$Patient.age)
dim(data_sum)

#plot continent-gender
continent_gender <- data.frame(continent, gender_vec)
continent_gender_freq <- as.data.frame(table(continent_gender))
continent_gender_freq
library(ggplot2)
g1 <- ggplot(continent_gender_freq[1:12,], aes(fill=gender_vec, x=continent, y=Freq)) + geom_bar(position = "dodge", stat = "identity") +theme_classic()
g2 <- g1 + ggtitle("Covid-19 patients") + xlab("Location") + ylab("Frequency") + labs(fill= "Gender")
g2
ggsave("Location_Gender_distribution.pdf", g2)

#x2 location-gender
table(continent_gender)
head(continent_gender)
chisq.test(continent_gender$continent, continent_gender$gender_vec)


#----STATUS----DECEASED-----
status <- covid19data$Patient.status
age_status_continent <- data.frame(covid19data$Patient.age, covid19data$Patient.status, continent)
head(age_status_continent)
table(status)

deceased_status_names <- c("Deceased", "Hospitalized/Deceased", " Deceased", "Death", "Hospitalized, deceased", "deceased")
deceased_status <- c()
ageofdeceased <- c()
continentofdeceased <- c()
for (i in 1:nrow(age_status_continent)){
  curr_row <- age_status_continent[i,]
  if (curr_row[2] %in% deceased_status_names){
    deceased_status <- c(deceased_status, curr_row[2])
    ageofdeceased <- c(ageofdeceased, curr_row[1])
    continentofdeceased <- c(continentofdeceased, curr_row[3])
  }
}
deceased_status <- unlist(deceased_status, use.names = F)
ageofdeceased <- unlist(ageofdeceased, use.names = F)
continentofdeceased <- unlist(continentofdeceased, use.names = F)
deceased_df <- data.frame(ageofdeceased, deceased_status, continentofdeceased)
deceased_df

#function gia na onomastoun DECEASED oles oi ypokatigories twn deceased 
count_deceased <- function(x){
  if (is.na(x)){
    return("OTHER")
  }
  else if (x %in% deceased_status){
    return("DECEASED")
  }
} #tha ekana apply sto column twn deceased to function x

#telika ftiaxnw vector me 160 deceased k ton prosthetw sto deceased_df gia to scatter
deceased <- c(rep("DECEASED", 160))
deceased_df <- data.frame(deceased_df, deceased)
deceased_df

#scatterplot for deceased 
ageofdeceased <- as.numeric(ageofdeceased)
deceased_scatter_df <- data.frame(ageofdeceased, continentofdeceased, deceased)
deceased_scatter_df
g1 <- ggplot(deceased_scatter_df, aes(x= continentofdeceased, y= ageofdeceased, colour = "red")) +geom_point(show.legend = F)
g2 <- g1 + scale_y_continuous(breaks = seq(0,100,10))
g3 <- g2 + theme_classic() + ylab("Age") + xlab("Location") + ggtitle("Covid-19 deceased")
g3
ggsave("deceased_scatter.pdf", g3)

#--------
age_status <- data.frame(data_sum$covid19data.Patient.age, data_sum$covid19data.Patient.status)
head(age_status)
unique(age)
grep("[^a-zA-Z]", age_status$data_sum.covid19data.Patient.age)

#ftiaxnw ta names sta columns tou data_sum
names(data_sum) <- c("Date", "Continent", "Country", "Gender", "Status", "Age")
head(data_sum)

clean.age <- function(x){
  if (is.na(x)){
    return(-1)
  }
  else if (grepl("[a-zA-Z]", x)){
    return(-1)
  }
  else {
    return(as.numeric(x))
  }
}

clean_age_vec = c()
for (i in data_sum$Age){
  res <- clean.age(i)
  clean_age_vec <- c(clean_age_vec, res)
}

data_sum <- data.frame(data_sum, clean_age_vec)
names(data_sum)[7] <- c("Clean_age")
head(data_sum)

#gia na parw mono ta rows pou exoun ok age
data_sum[data_sum$Age>0, ]

#----alternatively, apply the function clean.age(!!faster!!) -------
#me apply 
clean_age_apply <- apply(data_sum[6], MARGIN = 1, FUN = clean.age)
#me lapply gia list, meta xreiazetai unlist
clean_age_lapply <- lapply(data_sum$Age, clean.age )
clean_age_lapply <- unlist(clean_age_lapply, use.names = F)

#sub dataframe, sorted by age (mikrotero df, me col opws to data_sum alla me ola ta rows na exoun ok ilikia)
data_sum_agesorted <- data_sum[data_sum$`Clean_age`>0,]
head(data_sum_agesorted)
#sub df, to idi sorted by age, na exei kai sto gender mono male/female oxi unknown!
mf_vec <- c("MALE", "FEMALE")
data_sum_agesorted_gender <- data_sum_agesorted[data_sum_agesorted$Gender %in% mf_vec,]
head(data_sum_agesorted_gender)

#---scatterplot age-continent 

p1 <- ggplot(data_sum_agesorted_gender, aes(x=continent, y=c))

g4 <- ggplot(data_sum_agesorted_gender, aes(x=Continent, y=Clean_age, color= Gender)) +geom_point()
g4
g5 <- g4 + scale_y_continuous(breaks = seq(0,100,10)) + scale_color_manual(values= c("pink","blue"))
g5
g6 <- g5 +  xlab("Location") + ylab("Age") + ggtitle("Covid-19 patients") + labs(fill="Gender") + theme_classic()
g6
ggsave("scatter mf_loc.pdf", g6)

#-----age-distribution(age groups)----
age_clean_vec <- data_sum_agesorted$Clean_age
age_div <- age_clean_vec%/%10
age_dec <- age_div *10
table(age_dec)
age_distr_df<- as.data.frame(table(age_dec))
age_distr_df
age_help <- c(seq(9,109,10))

age_distr_df$age_dec<- paste(age_distr_df$age_dec,age_help, sep = "-")
age_distr_df

d1<- ggplot(age_distr_df[1:10,], aes(x=age_dec, y=Freq)) + geom_bar(stat = "identity", color="blue", fill= "grey", width = 0.5)
d2 <- d1 + xlab("Age group") + ylab("Frequency") + ggtitle("Covid-19 Age distribution") + theme_classic()
d2
ggsave("Age_distribution.pdf", d2)

#-------date------
#to data_sum_dag exei date se auksousa seira, clean gender, clean age 
data_sum_dag <- data_sum_agesorted_gender[order(data_sum_agesorted_gender$Date),]
nrow(data_sum_dag)
#petaw eksw ta nas apo to date(oloklires kataxwriseis)
data_sum_dag <- data_sum_dag[1:3408,]
dim(data_sum_dag)

#split ta dates, gia na xw mono month 
function(x){
  
}




