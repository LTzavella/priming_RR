#SCRIPT TO BE RUN ON RAW DATA FILES THAT HAVE NOT YET BEEN DE-IDENTIFIED - NOT FOR USE OUTSIDE THE LAB#

#VARIABLES WE HAVE CONSIDERED THAT THEIR COMBINATION CAN BE USED TO POTENTIALLY IDENTIFY INDIVIDUALS:

# Height and weight- specific self-reported measurements (not BMI)
# Gender- we had several options for gender and open-ended responses especially could be used to identify participants
# Ethnicity - ethnicity was only recorded to present the demographics of the samples and is not used for further analyses
# for the laboratory sample from Cardiff School of Psychology this can be identifying information and will therefore be deleted
# from openly shared data

# Read csv files from the specified directory and create data lists

require(here)
require(data.table)

#Data collected in the laboratory 
lab<- list.files(here("Lab_data"), all.files = TRUE, full.names = FALSE, no.. = TRUE, pattern = "\\.iqdat$")

# Data collected online 
online <- list.files(here("Online_data"), all.files = TRUE, full.names = FALSE, no.. = TRUE, pattern = "\\.iqdat$")

#Save data files
data1<- lapply(here("Lab_data", lab), function(x) {fread(x)})
data1 <- lapply(data1, function(x) as.data.frame(x))

data2 <- lapply(here("Online_data", online), function(x) {fread(x)})
data2 <- lapply(data2, function(x) as.data.frame(x))

#Assign names to the list elements (subject IDs).   

subs1 <- lapply(data1, function(x) x[1,1])
names(data1) <- subs1

subs2 <- lapply(data2, function(x) x[1,1])
names(data2) <- subs2

#Subset participants who had errors in the APP or missed trials in the FCT- manually for now.

data1 <- data1[sapply(data1, function(x) any(!x$subject %in% ER1_excl))]
data2 <- data2[sapply(data2, function(x) any(!x$subject %in% ER2_excl))]


data1 <- data1[sapply(data1, function(x) any(x$subject %in% FCT1_incl))]
data2 <- data2[sapply(data2, function(x) any(x$subject %in% FCT2_incl))]

data2 <- data2[sapply(data2, function(x) any(!x$subject %in% c("121", "287")))]



# Correct invalid BMI with imperial/metric re-calculation

data1 <- lapply(data1, transform, height_1 = as.numeric(response[trialcode=="height_1"]))
data1 <- lapply(data1, transform, weight_1 = as.numeric(response[trialcode=="weight_1"]))
data1 <- lapply(data1, transform, weight_2 = ifelse(response[trialcode=="weight_2"]=="",0, as.numeric(response[trialcode=="weight_2"])))



data1 <- lapply(data1, transform,
  height_system = ifelse(height_1 > 2,
    "imperial",
    "metric"
  )
)

data1 <- lapply(data1, transform,
  weight_system = ifelse(weight_1 < 38,
    "imperial",
    "metric"
  )
)

data1 <- lapply(data1, transform,
  height = ifelse(height_system == "imperial",
    abs(as.numeric(response[trialcode == "height_1"]) * 30.48) + abs(as.numeric(response[trialcode == "height_2"]) * 2.54),
    abs(as.numeric(response[trialcode == "height_1"]) * 100) + abs(as.numeric(response[trialcode == "height_2"]))
  )
)

data1 <- lapply(data1, transform,
  weight = ifelse(weight_system == "imperial",
    abs(as.numeric(response[trialcode == "weight_1"]) * 6.35029) + abs(weight_2 * 0.45352),
    abs(as.numeric(response[trialcode == "weight_1"])) + abs(weight_2 / 1000)
  )
)

data1 <- lapply(data1, transform,
                BMI = weight / ((height / 100) * height / 100)
)

###

data2 <- lapply(data2, transform, height_1 = ifelse("height_1" %in% trialcode, as.numeric(response[trialcode=="height_1"]), -99))
data2 <- lapply(data2, transform, weight_1 = ifelse("weight_1" %in% trialcode, as.numeric(response[trialcode=="weight_1"]), -99))
data2 <- lapply(data2, transform, weight_2 = ifelse("weight_2" %in% trialcode, as.numeric(response[trialcode=="weight_2"]), -99))
data2 <- lapply(data2, transform, weight_2 = ifelse(is.na(weight_2), 0, weight_2))



data2 <- lapply(data2, transform,
  height_system = ifelse(height_1 > 2,
    "imperial",
    "metric"
  )
)

data2 <- lapply(data2, transform,
  weight_system = ifelse(weight_1 < 38,
    "imperial",
    "metric"
  )
)

data2 <- lapply(data2, transform,
  height = ifelse(height_system == "imperial",
    abs(as.numeric(response[trialcode == "height_1"]) * 30.48) + abs(as.numeric(response[trialcode == "height_2"]) * 2.54),
    abs(as.numeric(response[trialcode == "height_1"]) * 100) + abs(as.numeric(response[trialcode == "height_2"]))
  )
)

data2 <- lapply(data2, transform,
  weight = ifelse(weight_system == "imperial",
    abs(as.numeric(response[trialcode == "weight_1"]) * 6.35029) + abs(weight_2 * 0.45352),
    abs(as.numeric(response[trialcode == "weight_1"])) + abs(weight_2/ 1000)
  )
)


data2 <- lapply(data2, transform,
                BMI = weight / ((height / 100) * height / 100)
)

# Now we can safely remove height/weight measurements and only keep BMI)

data1 <- lapply(data1, subset, trialcode!="height_1" & trialcode!="height_2" & trialcode!="weight_1" & trialcode!="weight_2",
                select = -c(height, weight))

data2 <- lapply(data2, subset, trialcode!="height_1" & trialcode!="height_2" & trialcode!="weight_1" & trialcode!="weight_2",
                select = -c(height, weight))

# Remove ID trialcodes - noticed that some participants instead of their Prolific IDs wrote their e-mail addresses/names

data1 <- lapply(data1, subset, trialcode!="ID")
data2 <- lapply(data2, subset, trialcode!="ID")

# Descriptives such as age, gender and ethnicity cannot be shared, but we can extract all variables 
# and add to a stand-alone cvs without subject IDs.

identifiers1 <- data.frame(matrix(nrow=length(data1), ncol=5))
identifiers2 <- data.frame(matrix(nrow=length(data2), ncol=5))

names(identifiers1) <- c("age", "gender", "gender_other", "ethnicity", "ethnicity_other")
names(identifiers2) <- c("age", "gender", "gender_other", "ethnicity", "ethnicity_other")

identifiers1[1] <- ldply(data1, function(x) paste(x$response[x$trialcode=="age"]), .id=NULL)
identifiers1[2] <- ldply(data1, function(x) paste(x$response[x$trialcode=="gender"]), .id=NULL)
identifiers1[3] <- ldply(data1, function(x) paste(x$response[x$trialcode=="genderother"]), .id=NULL)
identifiers1[4] <- ldply(data1, function(x) paste(x$response[x$trialcode=="ethnicity"]), .id=NULL)
identifiers1[5] <- ldply(data1, function(x) paste(x$response[x$trialcode=="ethnicityother"]), .id=NULL)

# A few online participants did not complete the last part of the study (survey) and variables are missing
# Conditional statements are therefore included in the code below.

identifiers2[1] <- ldply(data2, function(x) paste(x$response[x$trialcode=="age"]), .id=NULL)
identifiers2[2] <- ldply(data2, function(x) if("gender" %in% x$trialcode) {paste(x$response[x$trialcode=="gender"])} 
                         else {paste("0")}, .id=NULL)
identifiers2[3] <- ldply(data2, function(x) if("genderother" %in% x$trialcode) {paste(x$response[x$trialcode=="genderother"])} 
                         else {paste("0")}, .id=NULL)
identifiers2[4] <- ldply(data2, function(x) if("ethnicity" %in% x$trialcode) {paste(x$response[x$trialcode=="ethnicity"])} 
                         else {paste("0")}, .id=NULL)
identifiers2[5] <- ldply(data2, function(x) if("ethnicityother" %in% x$trialcode) {paste(x$response[x$trialcode=="ethnicityother"])} 
                         else {paste("0")}, .id=NULL)

identifiers2[identifiers2 == 0] <- NA


# Shuffle rows so that the order is not the same with data1/data2 lists of dataframes

identifiers1 <- identifiers1[sample(1:nrow(identifiers1)), ]
identifiers2 <- identifiers2[sample(1:nrow(identifiers2)), ]

# Remove these identifying variables from the data now

data1 <- lapply(data1, subset, trialcode!="gender" & trialcode!="genderother" & trialcode!="ethnicity" & trialcode!="ethnicityother"
                & trialcode!="age")
data2 <- lapply(data2, subset, trialcode!="gender" & trialcode!="genderother" & trialcode!="ethnicity" & trialcode!="ethnicityother"
                & trialcode!="age")

identifiers1$gender <- as.numeric(ifelse(identifiers1$gender=="1", 1, 0))
identifiers2$gender <- as.numeric(ifelse(identifiers2$gender=="1", 1, 0))



# Export csvs

write.csv(identifiers1, "identifiers1.csv")
write.csv(identifiers2, "identifiers2.csv")

# Export de-identified raw data files 

lapply(1:length(data1), function(i) 
  write.csv(data1[[i]], file=here("deidentified_lab", paste0(names(data1[i]), ".csv")), row.names=FALSE))

lapply(1:length(data2), function(i) 
  write.csv(data2[[i]], file=here("deidentified_online", paste0(names(data2[i]), ".csv")), row.names=FALSE))
































