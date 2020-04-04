##### 1. Package Upkeep  #####

##  Install Packages
# install.packages("lubridate")
# install.packages("zoo")
# install.packages("reshape2")
# install.packages("plyr")

##  Load Packages
library(lubridate)
library(zoo)
library(reshape2)
library(plyr)

##### 2. Working Directory  #####

##  Set working directory path
setwd("~/Cari Bogulski Files to Transfer/Misc/R Ladies")

##### 3. File Names  #####

##  Load File Name
##  NOTE: Use tab complete to populate this name after setting your
##  working directory
load.file.name = "2020-03-16 fake mch data.csv"

##  Save File Names
##  Summary Data File Name
##
##  NOTE: This file name is created dynamically based on the date
##  and time you ran this line.
##
##  CAUTION: This method will prevent you from overwriting data, but
##  may make a lot of files you don't necessarily want. You can always
##  hard code your save file names if you prefer.
save.file.name.monthly.data = paste0(format(now(), "%Y-%m-%d_%H-%M-%S_"),"fake.mch.summary.csv")

##### 4. Data Import  #####
mch.data = read.csv("2020-03-16 fake mch data.csv")

##### 5. Data Cleaning  #####

##  1/8/2020:
##  The social worker comes to your office to tell you that
##  a client was accidentally enrolled twice: once as 
##  client ID# 104 and again as client ID# 107.
##  
##  1/9/2020:
##  The social worker tells you that client ID# 109
##  had to leave in the middle of her enrollment, and the
##  care coordinator has not been able to get in touch with
##  her to complete the enrollment process. Remove client ID# 109
##  until she can complete the enrollment process.
##
##  NOTE: These can be separated by | in a c() call to have
##  multiple IDs in one line.
client.ids.to.drop = c("107|109")

##  Data without client ID# 107, who had already been enrolled
##  as client ID# 104.
mch.data.unique.clients = mch.data[!grepl(client.ids.to.drop, mch.data$ID),]

##  For some reason, Baby.Birthday is not importing with NAs as blanks.
##  Fix this.
mch.data.unique.clients$Baby.Birthday[mch.data.unique.clients$Baby.Birthday==""] = NA

##### 6. Baby-Level Data  #####

##  Extract just baby variables
mch.data.just.babies = mch.data.unique.clients[!is.na(mch.data.unique.clients$Baby.Birthday),]

##  Give the babies unique IDs based on their mothers' IDs
mch.data.just.babies$baby.id = paste0(mch.data.just.babies$ID,"-baby")

##### 7. Adding Variables #####

##  Identify month/year of each enrollment
##  Women: Use Date.Enrolled
mch.data.unique.clients$enrolled.month = as.yearmon(mdy(mch.data.unique.clients$Date.Enrolled))

##  Babies: Use Date.of.Data.Collection (because babies are only enrolled
##  after they are born, not when the mother was enrolled)
mch.data.just.babies$enrolled.month = as.yearmon(mdy(mch.data.just.babies$Date.of.Data.Collection))

##  Identify month/year of each data collection
##  Women
mch.data.unique.clients$data.collection.month = as.yearmon(mdy(mch.data.unique.clients$Date.of.Data.Collection))

##  Identify only the data for an enrollment.
mch.data.enrollments.women = mch.data.unique.clients[mdy(mch.data.unique.clients$Date.Enrolled)==mdy(mch.data.unique.clients$Date.of.Data.Collection),]

##### 8. Enrollments by Month #####

##  Women Enrolled by Month
women.enrollments = ddply(mch.data.enrollments.women, .(Conception.Phase,enrolled.month), 
                       summarise,
                       num.clients = length(Conception.Phase),
                       .drop = FALSE
)

##  Babies Enrolled by Month
baby.enrollments = ddply(mch.data.just.babies, .(enrolled.month), 
                          summarise,
                          Babies = length(baby.id),
                          .drop = FALSE
)

##  Reshaping women client enrollment summary
##  Melting
women.enrollments.m = melt(women.enrollments, id.vars = c("enrolled.month", "Conception.Phase"))

##  Casting
women.enrollments.c = dcast(women.enrollments.m, enrolled.month ~ Conception.Phase)

##  Merge women and babies
all.client.enrollments = merge(women.enrollments.c, baby.enrollments, by = c("enrolled.month"), all = T)

##  Set NAs equal to 0, since NA means 0 enrollments
all.client.enrollments[is.na(all.client.enrollments)] = 0

##### 9. Export Summary #####
##
##  NOTE: Using na = "" in case any blanks persist.
##  Also deleting row names.
write.csv(all.client.enrollments, save.file.name.monthly.data, na = "", row.names = FALSE)
