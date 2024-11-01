wq <- read.csv("Data/wq.csv", sep =";")


hwg <- read.csv("Data/hwg.csv")

# Raw
charToRaw("ø")


# Hvilke typer variabler man har fået til rådighed
str(hwg)
#Statistikken på det
summary(hwg)
#Lav en ny kolonne på dataframen. Her regner vi BMI'en ud ved at bruge en formel
head(hwg)
hwg$BMI <- hwg$Weight/(hwg$Height/100)^2

# hwg subset
hwgSub <- hwg[1:10, ]

baseURL <- "https://www.boligsiden.dk/tilsalg?page="


# for Loops.
for(i in 1:nrow(hwgSub)){
  print(hwg[i,1])
}

for(i in 1:nrow(hwgSub)){
  #print(c("element nr.",i,"", hwg[i,3]))
  outputURL <- paste0(baseURL,i)
  print(outputURL)
}

#https://www.boligsiden.dk/tilsalg?page=2

for(i in 1:nrow(hwgSub)){
  #print(c("element nr.",i,"", hwg[i,3]))
  output <- paste("element nr.",i,"", hwg[i,3])
  print(output)
}

hwgSub$BMI=NA

for(i in 1:nrow(hwgSub)){
  #print(c("element nr.",i,"", hwg[i,3]))
  #compute the BMI for each row manually
  # TIP: Debug: i = 1
  hwgSub[i,6] <- hwgSub[i,4]/(hwgSub[i,3]/100)^2
}


# Ny vektor på hwgSub på 10 elementer
kbh <- 1:10

NewhwgSub <- cbind(hwgSub, kbh)
# Rename kbh kolonne. Lav en normalfordelt vektor med 10 datapunkter
# Renaming af enkelt kolonne
names1 <- c("X", "Køn", "Højde", "Vægt", "Indeks", "BMI")
colnames(NewhwgSub)[1] <- "Kurt"

# Ny øvelse
library(ISLR2)
df <- Boston
#Histogram på H. Fundet ved at skrive Summary(df) i konsolen.
hist(df$age)

###### FORBEREDER DINE DATA TIL LOGISTISK REGRESSION ifelse siger hvis der er sandt, gør du det her. Hvis det ikke er sandt, gør du det her
df$agebind <- ifelse(df$age>60,1,0)
# Function skal bruges til at der kommer tal ind, og spytte en kategori ud
df$Age_cat <- sapply (df$age, FUN = give_age_cat)


is.na(hwg$Gender)
sum(is.na(hwg$Gender))
# Måde at ændre en variable på. Eller et navn
hwg$Gender <- sapply (hwg$Gender, function(x) gsub("l", "x", x))


give_age_cat <- function(house_age){
  ret_val <- ""
  if(house_age<2) {
    ret_val <- "Brand new"
  }else if (house_age<5){
    ret_val <- "New"
  }else if(house_age<10){
    ret_val <- "Normal"
  }else {
      ret_val <- "Old"
  }
  return(ret_val)
  }
}



