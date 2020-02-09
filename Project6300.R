install.packages("ggpubr")
install.packages("arules", dependencies = T)
install.packages("rJava", depencies = T)
library(ggplot2)
library(ggpubr)
library(grid)
library(arules)
library(arulesViz)
library(rJava)
library(rCBA)
theme_set(theme_pubr())

getwd()
setwd("C:\\Users\\abdul\\Desktop\\8650 Project files")
#--------Reading in the csv file and converting it into a dataframe
d <- read.csv("COBRA-2009-2018.csv", na.strings=c("",".","NA"))
View(d)
View(d[,c("Report.Date","UCR.Literal", "UCR.Literal")])
levels(d$UCR.Literal)
length(levels(d$Beat))

str(d)

#---------Changing the names of the column in the data frame to desired attributes. 
colnames(d) <- c("Report.Number", "Report.Date", "Occur.Date", "Occur.Time", "Possible.Date", "Possible.Time", "Beat", "Apartment.Office.Prefix", "Apartment.Number", "Location", "Shift.Occurrence", "Location.Type", "UCR.Literal", "UCR.No", "IBR.Code", "Neighborhood", "NPU", "Latitude", "Longitude")


#-----------Checking for total number of missing neighborhood values and approximating them using the NPU value
table(is.na(d$Neighborhood))

#----------percentage of missing values in the neighborhood column
k <- table(is.na(d$Neighborhood))
cat((as.vector(k[2])/nrow(d))*100, "%")

#---------Checking for most common neighborhood values with NPU value as C
#---------filtering out values with NA present in them so as to see which neighborhood value is the most recurring with NPU value of C

j <- d[d$NPU == "C" & !is.na(d$NPU) & !is.na(d$Neighborhood),c("Neighborhood","NPU")]
c <- tail(names(sort(table(j$Neighborhood))), 1)

#---------Filling in the most common value of neighborhood for C in the missing places in the neighborhood column.
k <- d
k <- k[!is.na(k$NPU),]
k[k$NPU == "C", c("Neighborhood", "NPU")]

#--------Total number of missing value rows before preprocessing
table(is.na(k$Neighborhood))

for (i in 1:nrow(k)){
  if (is.na(k$Neighborhood[i])){
    if(k$NPU[i] == "C"){
      k$Neighborhood[i] <- c
    }
  }
}

#---------Total number of missing value rows after preprocessing
table(is.na(k$Neighborhood))

#---------C column and adjacent neighborhood values with no missing values
k[k$NPU == "C",c("Neighborhood", "NPU")]


#---------CREATING VARIOUS TYPES OF PLOTS TO GAIN INSIGHTS INTO THE DATASET
#---------subsetting the data set to create different sections such as morning, afternoon, evening and night
k <- d

k$OccurTime[k$Occur.Time  > 459 & k$Occur.Time <=1200] <- 1
k$OccurTime[k$Occur.Time  > 1200 & k$Occur.Time <=1800] <- 2
k$OccurTime[k$Occur.Time  > 1800 & k$Occur.Time <=2100] <- 3
k$OccurTime[(k$Occur.Time  > 2100 & k$Occur.Time <=2400) | (k$Occur.Time  > 0 & k$Occur.Time <=459)] <- 4
k <- k[!is.na(k$OccurTime),]
barplot(table(k$OccurTime),
        names.arg = c("Morning\n(05:00am-12:00pm)",
                      "Afternoon\n(12:00pm-6:00pm)",
                      "Evening\n(06:00pm-09:00pm)",
                      "Night\n(09:00pm-05:00am)"),
        main = "Bar graph of crime frequency",
        xlab = "Time of day",
        ylab = "Frequency",
        border = F,
        col = "skyblue",
        ylim = c(0,90000))

#---------Creating crime distribution plot
k <- d
k <- k[!is.na(k$UCR.Literal),]
k <- k[!k$UCR.Literal == 26,]
k$UCR.Literal <- droplevels(k$UCR.Literal)

ob <- par(mar = c(4,12,4,7))
xx <- barplot(table(k$UCR.Literal),
              horiz = T,
              border = F,
              col = "skyblue",
              las = 1,
              xlim = c(0,120000),
              main = "Crime Distribution wrt Type of crime",
              xlab = "Frequency",
              font = 2,
              font.lab = 2
)
tx2 <- table(k$UCR.Literal) 
text(x = tx2+10000,
     y = xx,  
     labels = as.character(tx2),
     col = "red"
     ) 
rm(ob)

#---------Creating crime distribution wrt NPU
k <- d
k <- k[!is.na(k$NPU),]
k <- k[!k$NPU == "Campbellton Road",]
k$NPU <- droplevels(k$NPU)

ob <- par(mar = c(4,4,4,4))
xx <- barplot(table(k$NPU),
              horiz = T,
              border = F,
              col = "skyblue",
              las = 1,
              xlim = c(0,40000),
              main = "Crime Distribution wrt NPU",
              xlab = "Frequency",
              font = 2,
              font.lab = 2
              
)
tx2 <- table(k$NPU) 
text(x = tx2+2000,
     y = xx,  
     labels = as.character(tx2),
     col = "red") 
rm(ob)


#---------Creating crime distribution wrt year
k <- d
k <- k[!is.na(k$Report.Date),]
k$ReportDate <- gsub("-", "", k$Report.Date)
k$ReportDate <- strtrim(k$ReportDate,4)

ob <- par(mar = c(4,4,4,5))
xx <- barplot(table(k$ReportDate),
              horiz = T,
              border = F,
              col = "skyblue",
              las = 1,
              xlim = c(0,45000),
              main = "Crime Distribution wrt year",
              xlab = "Frequency",
              ylab = "Year",
              font = 2,
              font.lab = 2
)
tx2 <- table(k$ReportDate) 
text(x = tx2+2000,
     y = xx,  
     labels = as.character(tx2),
     col = "red") 
rm(ob)

#---------Crime distribution with respect to day of the week. 

k <- d
k <- k[!is.na(k$Report.Date),]
k$Weekday <- weekdays(as.Date(k$Report.Date))

table <- table(k$Weekday)
table_levels <- names(table)

ggplot(k, aes(Weekday)) +
  geom_bar(fill = "skyblue") +
  scale_x_discrete(limits = rev(c ("Monday",
                                   "Tuesday",
                                   "Wednesday",
                                   "Thursday",
                                   "Friday",
                                   "Saturday",
                                   "Sunday"))) +
  coord_flip()+
  ggtitle("Crime distribution wrt Weekday") +
  labs(x = "Weekday",
       y = "Frequency") +
  geom_text(stat = "count",
            position = "identity",
            hjust = -0.1,
            aes(label = ..count..,
                color = "red"),
            show.legend = FALSE) +
  ylim(0,55000) +
  theme_classic() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"),
        axis.text.y =  element_text(face = "bold"))

#---------Crime distribution wrt month
k <- d

k <- k[!is.na(k$Report.Date),]
k$Month <- months(as.Date(k$Report.Date))

k$UCRLiteral[k$UCR.Literal == "ROBBERY-COMMERCIAL" |
               k$UCR.Literal == "ROBBERY-PEDESTRIAN" |
               k$UCR.Literal == "ROBBERY-RESIDENCE"] <- "ROBBERY"
k$UCRLiteral[k$UCR.Literal == "MANSLAUGHTER" |
               k$UCR.Literal == "HOMICIDE"] <- "MURDER"
k$UCRLiteral[k$UCR.Literal == "BURGLARY-NONRES" |
               k$UCR.Literal == "BURGLARY-RESIDENCE"] <- "BURGLARY"
k$UCRLiteral[k$UCR.Literal == "LARCENY-FROM VEHICLE" | 
               k$UCR.Literal == "LARCENY-NON VEHICLE"] <- "LARCENY"
k$UCRLiteral[k$UCR.Literal == "AGG ASSAULT"] <- "AGG ASSAULT"
k$UCRLiteral[k$UCR.Literal == "AUTO THEFT"] <- "AUTO THEFT"
k$UCRLiteral[k$UCR.Literal == "RAPE"] <- "RAPE"
k <- k[!is.na(k$UCRLiteral),]


table <- table(k$Month)
table_levels <- names(table)

ggplot(k, aes(Month)) +
  geom_bar(fill = "skyblue") +
  # facet_grid(UCRLiteral~.
  #            ,scales = "free") +
  scale_x_discrete(limits = rev(c ( "January",
                                "February",
                                "March",
                                "April",
                                "May",
                                "June",
                                "July",
                                "August",
                                "September",
                                "October",
                                "November",
                                "December"
                                ))) +
  coord_flip()+
  ggtitle("Crime distribution wrt Month") +
  labs(x = "Month",
       y = "Frequency") +
  geom_text(stat = "count",
            position = "identity",
            hjust = -0.1,
            vjust = 0.5,
            aes(label = ..count..,
                color = "red"),
            show.legend = FALSE) +
  ylim(0,33000) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"),
        axis.text.y = element_text(face = "bold")) 


#---------Variation in different crimes wrt time of the day
k <- d
k$OccurTime[k$Occur.Time  > 459 & k$Occur.Time <=1200] <- "Morning"
k$OccurTime[k$Occur.Time  > 1200 & k$Occur.Time <=1800] <- "Afternoon"
k$OccurTime[k$Occur.Time  > 1800 & k$Occur.Time <=2100] <- "Evening"
k$OccurTime[(k$Occur.Time  > 2100 & k$Occur.Time <=2400) | (k$Occur.Time  > 0 & k$Occur.Time <=459)] <- "Night"

k <- k[!is.na(k$UCR.Literal),]
k <- k[!k$UCR.Literal == 26,]
k$UCR.Literal <- droplevels(k$UCR.Literal)


temp <- aggregate(k$UCR.Literal,
                  by = list(k$UCR.Literal,
                            k$OccurTime),
                  FUN = length)
names(temp) <- c("crime", "time.tag", "count")
temp$time.tag <- factor(temp$time.tag,
                        levels = c ("Morning","Afternoon","Evening","Night"))


ggplot(temp,
       aes(x = crime,
           y = factor(time.tag))) +
  geom_tile(aes(fill = count)) +
  coord_equal() +
  scale_x_discrete("Crime",
                   expand = c(0,0)) +
  scale_y_discrete("Time of day",
                   expand = c(0,-2)) +
  scale_fill_gradient("Number of crimes",
                      low = "white",
                      high = "darkblue") +
  theme_bw() +
  ggtitle("Crime distribution by time of day") +
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_line(colour = NA),
        panel.grid.minor = element_line(colour = NA),
        axis.text.x = element_text(angle = 45,
                                   hjust = 0.8,
                                   vjust = 0.9,
                                   size  = 7),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"),
        axis.ticks = element_blank(),
        axis.text = element_text(face = "bold"))


#---------Variation in different crimes wrt day of the week
k <- d

k <- k[!is.na(k$UCR.Literal),]
k <- k[!k$UCR.Literal == 26,]
k$UCR.Literal <- droplevels(k$UCR.Literal)

k <- k[!is.na(k$Report.Date),]
k$Weekday <- weekdays(as.Date(k$Report.Date))


temp <- aggregate(k$UCR.Literal,
                  by = list(k$UCR.Literal,
                            k$Weekday),
                  FUN = length)

names(temp) <- c("crime", "time.tag", "count")
temp$time.tag <- factor(temp$time.tag,
                        levels = c ("Monday",
                                    "Tuesday",
                                    "Wednesday",
                                    "Thursday",
                                    "Friday",
                                    "Saturday",
                                    "Sunday"))


ggplot(temp,
       aes(x = crime,
           y = factor(time.tag))) +
  geom_tile(aes(fill = count)) +
  coord_equal() +
  scale_x_discrete("Crime",
                   expand = c(0,0)) +
  scale_y_discrete("Day of the week",
                   expand = c(0,-2)) +
  scale_fill_gradient("Number of crimes",
                      low = "white",
                      high = "darkblue") +
  theme_bw() +
  ggtitle("Crime distribution by day of the week") +
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_line(colour = NA),
        panel.grid.minor = element_line(colour = NA),
        axis.text.x = element_text(angle = 45,
                                   hjust = 0.8,
                                   vjust = 0.9,
                                   size  = 7),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"),
        axis.ticks = element_blank(),
        axis.text = element_text(face = "bold"))


#---------Variation in different crimes wrt month
k <- d

k <- k[!is.na(k$UCR.Literal),]
k <- k[!k$UCR.Literal == 26,]
k$UCR.Literal <- droplevels(k$UCR.Literal)

k <- k[!is.na(k$Report.Date),]
k$Month <- months(as.Date(k$Report.Date))


temp <- aggregate(k$UCR.Literal,
                  by = list(k$UCR.Literal,
                            k$Month),
                  FUN = length)

names(temp) <- c("crime", "time.tag", "count")
temp$time.tag <- factor(temp$time.tag,
                        levels = c ("January",
                                    "February",
                                    "March",
                                    "April",
                                    "May",
                                    "June",
                                    "July",
                                    "August",
                                    "September",
                                    "October",
                                    "November",
                                    "December"
                        ))


ggplot(temp,
       aes(x = crime,
           y = factor(time.tag))) +
  geom_tile(aes(fill = count)) +
  coord_equal() +
  scale_x_discrete("Crime",
                   expand = c(0,0)) +
  scale_y_discrete("Month",
                   expand = c(0,-2)) +
  scale_fill_gradient("Number of crimes",
                      low = "white",
                      high = "darkblue") +
  theme_bw() +
  ggtitle("Crime distribution by month") +
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_line(colour = NA),
        panel.grid.minor = element_line(colour = NA),
        axis.text.x = element_text(angle = 45,
                                   hjust = 0.8,
                                   vjust = 0.9,
                                   size  = 7),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"),
        axis.ticks = element_blank(),
        axis.text = element_text(face = "bold"))


#---------Type of crime distribution wrt Month
k <- d

k <- k[!is.na(k$Report.Date),]
k$Month <- months(as.Date(k$Report.Date))

k$UCRLiteral[k$UCR.Literal == "ROBBERY-COMMERCIAL" |
               k$UCR.Literal == "ROBBERY-PEDESTRIAN" |
               k$UCR.Literal == "ROBBERY-RESIDENCE"] <- "ROBBERY"
k$UCRLiteral[k$UCR.Literal == "MANSLAUGHTER" |
               k$UCR.Literal == "HOMICIDE"] <- "MURDER"
k$UCRLiteral[k$UCR.Literal == "BURGLARY-NONRES" |
               k$UCR.Literal == "BURGLARY-RESIDENCE"] <- "BURGLARY"
k$UCRLiteral[k$UCR.Literal == "LARCENY-FROM VEHICLE" | 
               k$UCR.Literal == "LARCENY-NON VEHICLE"] <- "LARCENY"
k$UCRLiteral[k$UCR.Literal == "AGG ASSAULT"] <- "AGG ASSAULT"
k$UCRLiteral[k$UCR.Literal == "AUTO THEFT"] <- "AUTO THEFT"
k$UCRLiteral[k$UCR.Literal == "RAPE"] <- "RAPE"
k <- k[!is.na(k$UCRLiteral),]


table <- table(k$Month)
table_levels <- names(table)

ggplot(k, aes(Month)) +
  geom_bar(fill = "skyblue") +
  facet_grid(UCRLiteral~.
             ,scales = "free") +
  scale_x_discrete(limits = rev(c ( "January",
                                    "February",
                                    "March",
                                    "April",
                                    "May",
                                    "June",
                                    "July",
                                    "August",
                                    "September",
                                    "October",
                                    "November",
                                    "December"
  ))) +
  # coord_flip()+
  ggtitle("Crime distribution wrt Month") +
  labs(x = "Month",
       y = "Frequency") +
  geom_text(stat = "count",
            position = "identity",
            # hjust = 0.1,
            vjust = 1.3,
            aes(label = ..count..,
                color = "red"),
            show.legend = FALSE) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"),
        axis.text = element_text(face = "bold")) 


#---------Type of crime distribution wrt NPU
k <- d
k <- k[!is.na(k$Report.Date),]
k$Month <- months(as.Date(k$Report.Date))

k$UCRLiteral[k$UCR.Literal == "ROBBERY-COMMERCIAL" |
               k$UCR.Literal == "ROBBERY-PEDESTRIAN" |
               k$UCR.Literal == "ROBBERY-RESIDENCE"] <- "ROBBERY"
k$UCRLiteral[k$UCR.Literal == "MANSLAUGHTER" |
               k$UCR.Literal == "HOMICIDE"] <- "MURDER"
k$UCRLiteral[k$UCR.Literal == "BURGLARY-NONRES" |
               k$UCR.Literal == "BURGLARY-RESIDENCE"] <- "BURGLARY"
k$UCRLiteral[k$UCR.Literal == "LARCENY-FROM VEHICLE" | 
               k$UCR.Literal == "LARCENY-NON VEHICLE"] <- "LARCENY"
k$UCRLiteral[k$UCR.Literal == "AGG ASSAULT"] <- "AGG ASSAULT"
k$UCRLiteral[k$UCR.Literal == "AUTO THEFT"] <- "AUTO THEFT"
k$UCRLiteral[k$UCR.Literal == "RAPE"] <- "RAPE"
k <- k[!is.na(k$UCRLiteral),]
k <- k[!is.na(k$NPU),]


table <- table(k$NPU)
table_levels <- names(table)

ggplot(k, aes(NPU)) +
  geom_bar(fill = "skyblue") +
  facet_grid(UCRLiteral~.
             ,scales = "free") +
  scale_x_discrete(limits = c ( "A",
                                "B",
                                "C",
                                "D",
                                "E",
                                "F",
                                "G",
                                "H",
                                "I",
                                "J",
                                "K",
                                "L",
                                "M",
                                "N",
                                "O",
                                "P",
                                "Q",
                                "R",
                                "S",
                                "T",
                                "U",
                                "V",
                                "W",
                                "X",
                                "Y",
                                "Z")) +
  ggtitle("Crime distribution wrt NPU") +
  labs(x = "NPU",
       y = "Frequency") +
  geom_text(stat = "count",
            position = "identity",
            size = 3,
            # hjust = 0.1,
            vjust = 0,
            aes(label = ..count..,
                color = "red"),
            show.legend = FALSE) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"),
        axis.text = element_text(face = "bold")) 



#---------Treating the data set to apply apriori algortihm 


# VARIOUS TRIALS AND ERRORS BEFORE CORRECT RULES COULD BE OBTAINED
# k <- d
# k$OccurTime[k$Occur.Time  > 459 & k$Occur.Time <=1200] <- "Morning"
# k$OccurTime[k$Occur.Time  > 1200 & k$Occur.Time <=1800] <- "Afternoon"
# k$OccurTime[k$Occur.Time  > 1800 & k$Occur.Time <=2100] <- "Evening"
# k$OccurTime[(k$Occur.Time  > 2100 & k$Occur.Time <=2400) | (k$Occur.Time  > 0 & k$Occur.Time <=459)] <- "Night"
# k <- k[!is.na(k$Report.Date),]
# k$Weekday <- weekdays(as.Date(k$Report.Date))
# k$Month <- months(as.Date(k$Report.Date))
# k <- k[!is.na(k$UCR.Literal),]
# k <- k[!k$UCR.Literal == 26,]
# k$UCR.Literal <- droplevels(k$UCR.Literal)
# levels(k$UCR.Literal)
# length(k$UCR.Literal)
# table(is.na(k$UCR.Literal))
# head(k)
# str(k)
# 
# k1 <- k
# k <- k1
# 
# 
# k$UCRLiteral[k$UCR.Literal == "ROBBERY-COMMERCIAL" |
#                k$UCR.Literal == "ROBBERY-PEDESTRIAN" |
#                k$UCR.Literal == "ROBBERY-RESIDENCE"] <- "ROBBERY"
# 
# k$UCRLiteral[k$UCR.Literal == "MANSLAUGHTER" |
#                k$UCR.Literal == "HOMICIDE"] <- "MURDER"
# 
# k$UCRLiteral[k$UCR.Literal == "BURGLARY-NONRES" |
#                k$UCR.Literal == "BURGLARY-RESIDENCE"] <- "BURGLARY"
# 
# k$UCRLiteral[k$UCR.Literal == "LARCENY-FROM VEHICLE" | 
#                k$UCR.Literal == "LARCENY-NON VEHICLE"] <- "LARCENY"
# k$UCRLiteral[k$UCR.Literal == "AGG ASSAULT"] <- "AGG ASSAULT"
# k$UCRLiteral[k$UCR.Literal == "AUTO THEFT"] <- "AUTO THEFT"
# k$UCRLiteral[k$UCR.Literal == "RAPE"] <- "RAPE"
# 
# 
# k1 <- k
# 
# k$UCRLiteral <- factor(k$UCRLiteral)
# 
# levels(k$UCRLiteral)
# head(k)
# k1 <- k
# 
# k <- k[!is.na(k$OccurTime),]
# k <- k[!is.na(k$NPU),]
# 
# levels(k$UCRLiteral)
# 
# head(k)
# k <- k[,c("UCRLiteral","NPU","Month","OccurTime","Weekday")]
# k$Month <- as.factor(k$Month)
# k$OccurTime <- as.factor(k$OccurTime)
# k$Weekday <- as.factor(k$Weekday)
# 
# k1 <- k
# 
# k <- k[,c("NPU","Month","OccurTime","Weekday")]
# str(k)
# 
# 
# 
# rules <- apriori(data = k,
#                  parameter = list(minlen = 5,supp = 0.000005, conf = 0.8),
#                  appearance = list(default = "lhs", rhs = c("UCRLiteral=ROBBERY",
#                                                             "UCRLiteral=MURDER",
#                                                             "UCRLiteral=BURGLARY",
#                                                             # "UCRLiteral=LARCENY",
#                                                             "UCRLiteral=AGG ASSAULT",
#                                                             "UCRLiteral=AUTO THEFT",
#                                                             "UCRLiteral=RAPE")), control = list(verbose = F))
# 
# 
# 
# rules <- apriori(data = k,
#                  parameter = list(minlen = 2, maxlen = 4,supp = 0.00000005, conf = 0.5)
#                  # appearance = list(default = "lhs", rhs = c("OccurTime=Morning",
#                  #                                            "OccurTime=Afternoon",
#                  #                                            "OccurTime=Evening",
#                  #                                            "OccurTime=Night")), control = list(verbose = F))
# )
# 
# rules <- sort(rules, decreasing = TRUE, by = "count")
# 
# # subsetRules <- which(colSums(is.subset(rules, rules)) > 1) 
# # length(subsetRules)  
# # rules <- rules[-subsetRules] 
# k <- k[!k$UCRLiteral == "LARCENY",]
# k <- k[!k$UCRLiteral == "BURGLARY",]
# k <- k[!k$UCRLiteral == "AUTO THEFT",]
# rules <- apriori(data = k,
#                  parameter = list(minlen = 4, supp = 0.0005, conf = 0.8))
# rules <- sort(rules, decreasing = T, by = "confidence")
# inspect(rules)
# head(k)
# 
# 
# #----Converting the data frame of crime data set to a transaction type of data
# k <- k1
# k <- k[!k$UCRLiteral == "LARCENY",]
# summary(k)
# nrow(k1);
# inspect(k[1:100])
# k <- as(as.data.frame(k),"transactions")
# rules <- apriori(k,
#                  parameter = list(minlen = 3, support = 0.0001, confidence = 0.8))
# inspect(rules)
# 




#-----Replacing ucr codes with crime type
PreProcess <- function(k){
  k$OccurTime[k$Occur.Time  > 459 & k$Occur.Time <=1200] <- "Morning"
  k$OccurTime[k$Occur.Time  > 1200 & k$Occur.Time <=1800] <- "Afternoon"
  k$OccurTime[k$Occur.Time  > 1800 & k$Occur.Time <=2100] <- "Evening"
  k$OccurTime[(k$Occur.Time  > 2100 & k$Occur.Time <=2400) | (k$Occur.Time  > 0 & k$Occur.Time <=459)] <- "Night"
  
  k <- k[!is.na(k$Report.Date),]
  
  k$Weekday <- weekdays(as.Date(k$Report.Date))
  k$Month <- months(as.Date(k$Report.Date))
  
  k <- k[!is.na(k$UCR.Literal),]
  k <- k[!k$UCR.Literal == 26,]
  k$UCR.Literal <- droplevels(k$UCR.Literal)
  levels(k$IBR.Code)
  
  # --- Crime homicide
  
  k$UCRLiteral[k$IBR.Code == "0008"] <- "CH-FAMILY-GUN"
  k$UCRLiteral[k$IBR.Code == "0901"] <- "CH-FAMILY-GUN"
  k$UCRLiteral[k$IBR.Code == "0902"] <- "CH-FAMILY-OTH WEAP"
  k$UCRLiteral[k$IBR.Code == "0903"] <- "CH-NONFAMILY-GUN"
  k$UCRLiteral[k$IBR.Code == "0904"] <- "CH-NONFAMILY-OTH WEAP"
  k$UCRLiteral[k$IBR.Code == 0905] <- "CH-PUB OFF-GUN"
  k$UCRLiteral[k$IBR.Code == 0906] <- "CH-PUB OFF-OTH WEAP"
  k$UCRLiteral[k$IBR.Code == 0907] <- "CH-POLICE-GUN"
  k$UCRLiteral[k$IBR.Code == 0908] <- "CH-POLICE-OTH WEAP"
  k$UCRLiteral[k$IBR.Code == 0909] <- "CH-VEHICLE"
  k$UCRLiteral[k$IBR.Code == "0910"] <- "CH-OTHER WEAP"
  k$UCRLiteral[k$IBR.Code == "0911"] <- "CH-GUN"
  k$UCRLiteral[k$IBR.Code == "0912"] <- "CH-OTHER WEAP"
  k$UCRLiteral[k$IBR.Code == "0999"] <- "CH-OTHER WEAP"
  
  k$UCRLiteral[k$IBR.Code == 1101] <- "RAPE-FORCIBLE FIREARM"
  k$UCRLiteral[k$IBR.Code == "1101A"] <- "RAPE-ATTEMPTS FIREARM"
  k$UCRLiteral[k$IBR.Code == 1102] <- "RAPE-FORCIBLE OTHER"
  k$UCRLiteral[k$IBR.Code == "1102A"] <- "RAPE-ATTEMPTS OTHER"
  k$UCRLiteral[k$IBR.Code == 1103] <- "RAPE-FORCIBLE STRONGARM"
  k$UCRLiteral[k$IBR.Code == "1103A"] <- "RAPE-ATTEMPTS STRONGARM"
  
  # ---ROBBERY
  k$UCRLiteral[k$IBR.Code == 1201] <- "R-COMMERCIAL-FIREARM"
  k$UCRLiteral[k$IBR.Code == "1201C"] <- "R-GAS-STATION-FIREARM"
  k$UCRLiteral[k$IBR.Code == "1201D"] <- "R-CONVENIENCE-FIREARM"
  k$UCRLiteral[k$IBR.Code == "1202"] <- "R-COMMERCIAL-OTHER"
  k$UCRLiteral[k$IBR.Code == "1202J"] <- "R-GAS-STATION-KNIFE"
  k$UCRLiteral[k$IBR.Code == "1202K"] <- "R-COMMERCIAL-KNIFE"
  k$UCRLiteral[k$IBR.Code == "1202L"] <- "R-CONVENIENCE-KNIFE"
  k$UCRLiteral[k$IBR.Code == "1202Q"] <- "R-GAS-STATION-OTHER"
  k$UCRLiteral[k$IBR.Code == "1202R"] <- "R-CONVENIENCE-OTHER"
  k$UCRLiteral[k$IBR.Code == "1203"] <- "R-COMMERICIAL-STRONGARM"
  k$UCRLiteral[k$IBR.Code == "1203X"] <- "R-GAS-STATION-STRONGARM"
  k$UCRLiteral[k$IBR.Code == "1203Y"] <- "R-CONVENIENCE-STRONGARM"
  k$UCRLiteral[k$IBR.Code == "1204"] <- "R-HIGHWAY-FIREARM"
  k$UCRLiteral[k$IBR.Code == "1205"] <- "R-HIGHWAY-OTHER"
  k$UCRLiteral[k$IBR.Code == "1205K"] <- "R-HIGHWAY-KNIFE"
  k$UCRLiteral[k$IBR.Code == "1206"] <- "R-HIGHWAY-STRONGARM"
  k$UCRLiteral[k$IBR.Code == "1207"] <- "R-RESIDENCE-FIREARM"
  k$UCRLiteral[k$IBR.Code == "1208"] <- "R-RESIDENCE-OTHER"
  k$UCRLiteral[k$IBR.Code == "1208K"] <- "R-RESIDENCE-KNIFE"
  k$UCRLiteral[k$IBR.Code == "1209"] <- "R-RESIDENCE-STRONGARM"
  k$UCRLiteral[k$IBR.Code == "1211G"] <- "R-BANK-FIREARM"
  k$UCRLiteral[k$IBR.Code == "1211K"] <- "R-BANK-KNIFE"
  k$UCRLiteral[k$IBR.Code == "1211O"] <- "R-BANK-OTHER"
  k$UCRLiteral[k$IBR.Code == "1211S"] <- "R-BANK-STRONGARM"
  k$UCRLiteral[k$IBR.Code == "1212"] <- "R-HIGHWAY-FIREARM"
  k$UCRLiteral[k$IBR.Code == "1216"] <- "R-MISCLENNANOUS-STRONGARM"
  k$UCRLiteral[k$IBR.Code == "1299G"] <- "R-MISCELLANOUS-FIREARM"
  k$UCRLiteral[k$IBR.Code == "1299K"] <- "R-MISCELLANOUS-KNIFE"
  k$UCRLiteral[k$IBR.Code == "1299O"] <- "R-MISCELLANOUS-OTHER"
  k$UCRLiteral[k$IBR.Code == "1299S"] <- "R-MISCELLANOUS-STRONGARM"
  k$UCRLiteral[k$IBR.Code == "1216"] <- "R-HIGHWAY_OTHER"
  
  
  #-------Assault
  k$UCRLiteral[k$IBR.Code == 1313] <- "ASLT-SIMPLE ASSAULT" 
  k$UCRLiteral[k$IBR.Code == 1314] <- "ASLT-FIREARM" 
  k$UCRLiteral[k$IBR.Code == 1315] <- "ASLT-OTHER" 
  k$UCRLiteral[k$IBR.Code == "1315K"] <- "ASLT-KNIFE" 
  k$UCRLiteral[k$IBR.Code == 1316] <- "O-TERRORISTIC THREATS-INTIMIDATION" 
  k$UCRLiteral[k$IBR.Code == 1318] <- "ASLT" 
  k$UCRLiteral[k$IBR.Code == 1375] <- "ASLT-STRONGARM" 
  k$UCRLiteral[k$IBR.Code == 1376] <- "ASLT-STRONGARM" 
  k$UCRLiteral[k$IBR.Code == 1377] <- "ASLT-STRONGARM" 
  k$UCRLiteral[k$IBR.Code == 1399] <- "ASLT-STRONGARM" 
  
  #-----------Burglary
  
  k$UCRLiteral[k$IBR.Code == "0512"] <- "B-NON RESIDENCE FORCE" 
  k$UCRLiteral[k$IBR.Code == 2202] <- "B-RESIDENCE FORCE" 
  k$UCRLiteral[k$IBR.Code == "2202A"] <- "B-RESIDENCE ATTEMPT" 
  k$UCRLiteral[k$IBR.Code == 2203] <- "B-NON RESIDENCE FORCE" 
  k$UCRLiteral[k$IBR.Code == "2203A"] <- "B-NON RESIDENCE ATTEMPT" 
  k$UCRLiteral[k$IBR.Code == 2204] <- "B-RESIDENCE NO FORCE" 
  k$UCRLiteral[k$IBR.Code == 2205] <- "B-NON RESIDENCE NO FORCE" 
  
  #-----Larceny
  k$UCRLiteral[k$IBR.Code == 2301] <- "LAR-PICKPOCKET" 
  k$UCRLiteral[k$IBR.Code == 2302] <- "LAR-PURSESNATCH" 
  k$UCRLiteral[k$IBR.Code == 2303] <- "LAR-SHOPLIFTING" 
  k$UCRLiteral[k$IBR.Code == 2304] <- "LAR-FROM M/V ACCESSORY" 
  k$UCRLiteral[k$IBR.Code == 2305] <- "LAR-FROM MOTOR VEHICLE" 
  k$UCRLiteral[k$IBR.Code == 2307] <- "LAR-FROM COIN OP.DEVICE" 
  k$UCRLiteral[k$IBR.Code == 2308] <- "LAR-THEFT FROM BUILDING" 
  k$UCRLiteral[k$IBR.Code == 2310] <- "LAR-MAIL" 
  k$UCRLiteral[k$IBR.Code == 2314] <- "LAR-GOVT PROPERTY" 
  k$UCRLiteral[k$IBR.Code == 2316] <- "LAR-MAILBOX" 
  k$UCRLiteral[k$IBR.Code == 2317] <- "LAR-THEFT OF BICYCLE" 
  k$UCRLiteral[k$IBR.Code == 2318] <- "LAR-LIVESTOCK"
  k$UCRLiteral[k$IBR.Code == 2361] <- "LAR-I"
  k$UCRLiteral[k$IBR.Code == 2373] <- "LAR-SHOPLIFTING"
  k$UCRLiteral[k$IBR.Code == 2374] <- "LAR-GAS DRIVE-OFF" 
  k$UCRLiteral[k$IBR.Code == 2382] <- "LAR-I" 
  k$UCRLiteral[k$IBR.Code == 2399] <- "LAR-ALL OTHER" 
  
  #--------Larceny Autotheft
  k$UCRLiteral[k$IBR.Code == 2404] <- "LA-AUTO THEFT THEFT" 
  k$UCRLiteral[k$IBR.Code == "2404A"] <- "LA-AUTO THEFT ATTEMPT" 
  k$UCRLiteral[k$IBR.Code == 2424] <- "LA-TRUCK/BUS THEFT" 
  k$UCRLiteral[k$IBR.Code == "2424A"] <- "LA-TRUCK/BUS ATTEMPT" 
  k$UCRLiteral[k$IBR.Code == 2434] <- "LA-OTHER THEFT" 
  k$UCRLiteral[k$IBR.Code == "2434A"] <- "LA-OTHER ATTEMPT" 
  
  #----------Other offenses
  k$UCRLiteral[k$IBR.Code == 2599] <- "F-OTHER COUNTERFEITING" 
  k$UCRLiteral[k$IBR.Code == 2803] <- "SP-RECEIVING STOLEN PROPERTY" 
  k$UCRLiteral[k$IBR.Code == 2804] <- "SP-POSSESSING STOLEN PROPERTY (AUTO)" 
  k$UCRLiteral[k$IBR.Code == 2899] <- "SP-OTHER STOLEN PROPERTY OFFENSE" 
  k$UCRLiteral[k$IBR.Code == 2902] <- "V-PRIVATE" 
  k$UCRLiteral[k$IBR.Code == 3562] <- "N-MARIJUANA-POSSESSION" 
  k$UCRLiteral[k$IBR.Code == 5299] <- "W-OTHER WEAPON OFFENSE" 
  k$UCRLiteral[k$IBR.Code == 5311] <- "DC-DISORDERLY CONDUCT" 
  k$UCRLiteral[k$IBR.Code == 5707] <- "O-CRIMINAL TRESPASS" 
  k$UCRLiteral[k$IBR.Code == 7399] <- "O-MISCELLANEOUS OFFENSE" 
  k$UCRLiteral[k$IBR.Code == 9920] <- "LA-WITHOUT CONSENT THEFT" 
  k$UCRLiteral[k$IBR.Code == 9999] <- "LA-WITHOUT CONSENT THEFT" 
  k
}
k <- d
k <- PreProcess(k)

#----------Dividing data set into training data set and sample data set

#k1 contains data from 2009-2015 crime reports
k1 <- k[!as.numeric(substr(k$Report.Date,1,4)) > 2016,]
k1 <- k1[,c("UCRLiteral","NPU","Month","OccurTime","Weekday")]

#k2 contains data from 2016-2018 crime reports
k2 <- k[!as.numeric(substr(k$Report.Date,1,4)) < 2016,]
k2 <- k2[,c("UCRLiteral","NPU","Month","OccurTime","Weekday")]


k1$Month <- as.factor(k1$Month)
k1$OccurTime <- as.factor(k1$OccurTime)
k1$Weekday <- as.factor(k1$Weekday)
k1$UCRLiteral <- as.factor(k1$UCRLiteral)

#----------REMOVING CERTAIN TYPES OF CRIMES TO GENRATE RULES RELATED TO CRIMES THAT ARE LESS FREQUENT THAN OTHERS
# k1 <- k1[!k1$UCRLiteral == "LAR-FROM MOTOR VEHICLE",]
# k1 <- k1[!k1$UCRLiteral == "LAR-SHOPLIFTING",]
# k1 <- k1[!k1$UCRLiteral == "B-NON RESIDENCE FORCE",]
# k1 <- k1[!k1$UCRLiteral == "B-RESIDENCE FORCE",]
# k1 <- k1[!k1$UCRLiteral == "R-HIGHWAY-FIREARM",]
# k1 <- k1[!k1$UCRLiteral == "LAR-ALL OTHER",]
# k1 <- k1[!k1$UCRVLiteral == "LA-AUTO THEFT THEFT",]
# k1 <- k1[!k1$UCRLiteral == "LAR-FROM M/V ACCESSORY",]
# k1 <- k1[!k1$UCRLiteral == "LAR-THEFT FROM BUILDING",]

k3 <- k1
k1 <- as(as.data.frame(k1),"transactions")

rules <- apriori(k1,
                 parameter = list(minlen = 5, support = 0.00001, confidence = 0.5),
                 appearance = list(default = "lhs", 
                                   rhs=paste0("UCRLiteral=", unique(k3$UCRLiteral[!is.na(k3$UCRLiteral)])), default="none"))


inspect(rules)

#---------Creating NPU, Month, Occurtime, Weekday and UCR Literal column based on the rules generated to further analyse for accuracy
inspectrules_df <- data.frame(
  lhs = labels(lhs(rules)),
  rhs = labels(rhs(rules)), 
  rules@quality)

head(inspectrules_df)

a <- inspectrules_df[,"lhs"]
a <- droplevels(a)
l <- strsplit(as.character(a), ",")
l <- matrix(unlist(l),ncol = 4,byrow=TRUE)
colnames(l) <- c("NPU","Month","OccurTime","Weekday")
l <- as.data.frame(l)

b <- inspectrules_df[,"rhs"]
b <- droplevels(b)
m <- strsplit(as.character(b),",")
m <- matrix(unlist(m), ncol = 1, byrow = TRUE)
colnames(m) <- "UCRLiteral"
m <- as.data.frame(m)


inspectrules_df$npu <- substr(l$NPU,6,nchar(as.character(l$NPU)))
inspectrules_df$month <- substr(l$Month,7,nchar(as.character(l$Month)))
inspectrules_df$occurtime <- substr(l$OccurTime,11,nchar(as.character(l$OccurTime)))
inspectrules_df$weekday <- substr(l$Weekday,9,nchar(as.character(l$Weekday))-1)
inspectrules_df$ucrliteral <- substr(m$UCRLiteral,13,nchar(as.character(m$UCRLiteral))-1)
View(inspectrules_df)


#---------Comparing the accuracy of the most relevant rules and generating rules that give us a good prediction when compared to future data
Prediction_accuracy <- {}
for (i in 1:nrow(inspectrules_df)){
  table1 <- table(k2[k2$NPU == as.character(inspectrules_df$npu[i]) & k2$Month == as.character(inspectrules_df$month[i]) & k2$OccurTime == as.character(inspectrules_df$occurtime[i]) & k2$Weekday == as.character(inspectrules_df$weekday[i]), "UCRLiteral"])
  if(nrow(table1) > 0 ){
    s <- as.data.frame(table(k2[k2$NPU == as.character(inspectrules_df$npu[i]) & k2$Month == as.character(inspectrules_df$month[i]) & k2$OccurTime == as.character(inspectrules_df$occurtime[i]) & k2$Weekday == as.character(inspectrules_df$weekday[i]), "UCRLiteral"]))
    n <- s[s$Var1 == inspectrules_df$ucrliteral[i],"Freq"]/sum(s$Freq)*100
    if(length(n) ==0 ){Prediction_accuracy[i] <- 0}
    if(length(n) > 0){
      Prediction_accuracy[i] <- n
    if((n > 60) & (n <95)){
      Prediction_accuracy[i] <- n
    
  print(table1)
  cat("Prediction accuracy:",(s[s$Var1 == inspectrules_df$ucrliteral[i],"Freq"]/sum(s$Freq))*100,"%","\n","Support:", inspectrules_df$support[i]*100,"%","\n","Confidence:",inspectrules_df$confidence[i], "","\n","Lift:", inspectrules_df$lift[i])
  print(inspectrules_df[i,c("lhs","rhs")])
    }
    }
  }
}
Prediction_accuracy
inspectrules_df$Prediction_accuracy <- Prediction_accuracy
inspectrules_df <- inspectrules_df[!is.na(inspectrules_df$Prediction_accuracy),]
inspectrules_df$support <- inspectrules_df$support*100
inspectrules_df <- inspectrules_df[inspectrules_df$Prediction_accuracy > 60 & inspectrules_df$Prediction_accuracy < 95 ,c("lhs","rhs","support","confidence","lift","Prediction_accuracy")]
View(inspectrules_df)


#Storing results for setting support = 0.0001, confidence = 0.5 and LAR-FROM MOTOR VEHICLE included in the data
irdf1 <- inspectrules_df

#Storing results for setting support = 0.0001, confidence = 0.5 and LAR-FROM MOTOR VEHICLE excluded from the data
irdf2 <- inspectrules_df

#Storing results for setting support = 0.00001, confidence = 0.5 and LAR-FROM MOTOR VEHICLE and LAR-FROM SHOP LIFTING excluded from the data
irdf3 <- inspectrules_df


irdf <- rbind(irdf1, irdf2, irdf3)
rownames(irdf) <- NULL
write.csv(irdf, file = "Prediction Results.csv")




#---------FP growth algorithm to generate rules
rules = rCBA::fpgrowth(k1, support=0.0001, confidence=0.5, maxLength=5, consequent="UCRLiteral", parallel=FALSE)
inspect(rules)

# 
# table(is.na(k$UCRLiteral))
# table(is.na(k$IBR.Code))
# table(k$IBR.Code[k$IBR.Code == 0008])
# k$UCR.Literal[k$IBR.Code == 0008]
# summary(k$UCRLiteral)
# levels(k$IBR.Code)
# levels(factor(k$UCRLiteral))
# k$UCRLiteral
# View(k[is.na(k$UCRLiteral),c("UCR.No","IBR.Code","UCRLiteral")])
# 

# ROUGH STUFF
# typeof(k$UCR.Literal[k$UCR.Literal == 26])
# names(sort(table(j$Neighborhood)))
# names(j[j==max(j)])
# j[which.max(j$Neighborhood),]
# max(j)
# typeof(j[1])
# l<-summary(j)
# as.character(l)
# l[1]
# typeof(l[1])
# typeof(j)
# order(j)
# max(j)
# d[is.na(d$Neighborhood),"District"]
# levels(d$NPU)
# ?complete.cases()
# levels(d$Latitude)
# 
# 
# typeof(titanic.raw)
# 
# d[!complete.cases(d),]
