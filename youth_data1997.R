
# Set working directory and remove comment
#Be sure to include youthData1997.dat in path directory

#setwd()


new_data <- read.table('youthData1997.dat', sep=' ')
names(new_data) <- c('R0000100',
  'R0322600',
  'R0322700',
  'R0356800',
  'R0357200',
  'R0359300',
  'R0536300',
  'R0536401',
  'R0536402',
  'R1235800',
  'R1482600')


# Handle missing values

  new_data[new_data == -1] = NA  # Refused 
  new_data[new_data == -2] = NA  # Dont know 
  new_data[new_data == -3] = NA  # Invalid missing 
  new_data[new_data == -4] = NA  # Valid missing 
  new_data[new_data == -5] = NA  # Non-interview 


# If there are values not categorized they will be represented as NA

vallabels = function(data) {
  data$R0000100[1.0 <= data$R0000100 & data$R0000100 <= 999.0] <- 1.0
  data$R0000100[1000.0 <= data$R0000100 & data$R0000100 <= 1999.0] <- 1000.0
  data$R0000100[2000.0 <= data$R0000100 & data$R0000100 <= 2999.0] <- 2000.0
  data$R0000100[3000.0 <= data$R0000100 & data$R0000100 <= 3999.0] <- 3000.0
  data$R0000100[4000.0 <= data$R0000100 & data$R0000100 <= 4999.0] <- 4000.0
  data$R0000100[5000.0 <= data$R0000100 & data$R0000100 <= 5999.0] <- 5000.0
  data$R0000100[6000.0 <= data$R0000100 & data$R0000100 <= 6999.0] <- 6000.0
  data$R0000100[7000.0 <= data$R0000100 & data$R0000100 <= 7999.0] <- 7000.0
  data$R0000100[8000.0 <= data$R0000100 & data$R0000100 <= 8999.0] <- 8000.0
  data$R0000100[9000.0 <= data$R0000100 & data$R0000100 <= 9999.0] <- 9000.0
  data$R0000100 <- factor(data$R0000100, 
    levels=c(0.0,1.0,1000.0,2000.0,3000.0,4000.0,5000.0,6000.0,7000.0,8000.0,9000.0), 
    labels=c("0",
      "1 TO 999",
      "1000 TO 1999",
      "2000 TO 2999",
      "3000 TO 3999",
      "4000 TO 4999",
      "5000 TO 5999",
      "6000 TO 6999",
      "7000 TO 7999",
      "8000 TO 8999",
      "9000 TO 9999"))
  data$R0322600 <- factor(data$R0322600, 
    levels=c(0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0,12.0), 
    labels=c("0",
      "1",
      "2",
      "3",
      "4",
      "5",
      "6",
      "7",
      "8",
      "9",
      "10",
      "11",
      "12"))
  data$R0322700[1.0 <= data$R0322700 & data$R0322700 <= 49.0] <- 1.0
  data$R0322700[50.0 <= data$R0322700 & data$R0322700 <= 99.0] <- 50.0
  data$R0322700[100.0 <= data$R0322700 & data$R0322700 <= 149.0] <- 100.0
  data$R0322700[150.0 <= data$R0322700 & data$R0322700 <= 199.0] <- 150.0
  data$R0322700[200.0 <= data$R0322700 & data$R0322700 <= 249.0] <- 200.0
  data$R0322700[250.0 <= data$R0322700 & data$R0322700 <= 299.0] <- 250.0
  data$R0322700[300.0 <= data$R0322700 & data$R0322700 <= 349.0] <- 300.0
  data$R0322700[350.0 <= data$R0322700 & data$R0322700 <= 399.0] <- 350.0
  data$R0322700[400.0 <= data$R0322700 & data$R0322700 <= 449.0] <- 400.0
  data$R0322700[450.0 <= data$R0322700 & data$R0322700 <= 499.0] <- 450.0
  data$R0322700[500.0 <= data$R0322700 & data$R0322700 <= 999999.0] <- 500.0
  data$R0322700 <- factor(data$R0322700, 
    levels=c(0.0,1.0,50.0,100.0,150.0,200.0,250.0,300.0,350.0,400.0,450.0,500.0), 
    labels=c("0",
      "1 TO 49",
      "50 TO 99",
      "100 TO 149",
      "150 TO 199",
      "200 TO 249",
      "250 TO 299",
      "300 TO 349",
      "350 TO 399",
      "400 TO 449",
      "450 TO 499",
      "500 TO 999999: 500+"))
  data$R0356800 <- factor(data$R0356800, 
    levels=c(0.0,1.0,2.0), 
    labels=c("NOT TRUE",
      "SOMEWHAT/SOMETIMES TRUE",
      "OFTEN TRUE"))
  data$R0357200 <- factor(data$R0357200, 
    levels=c(0.0,1.0,2.0), 
    labels=c("NOT TRUE",
      "SOMEWHAT/SOMETIMES TRUE",
      "OFTEN TRUE"))
  data$R0359300 <- factor(data$R0359300, 
    levels=c(1.0,2.0,3.0,4.0,5.0), 
    labels=c("VERY UNDERWEIGHT",
      "SLIGHTLY UNDERWEIGHT",
      "ABOUT THE RIGHT WEIGHT",
      "SLIGHTLY OVERWEIGHT",
      "VERY OVERWEIGHT"))
  data$R0536300 <- factor(data$R0536300, 
    levels=c(0.0,1.0,2.0), 
    labels=c("No Information",
      "Male",
      "Female"))
  data$R0536401 <- factor(data$R0536401, 
    levels=c(1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0,12.0), 
    labels=c("1: January",
      "2: February",
      "3: March",
      "4: April",
      "5: May",
      "6: June",
      "7: July",
      "8: August",
      "9: September",
      "10: October",
      "11: November",
      "12: December"))
  data$R1235800 <- factor(data$R1235800, 
    levels=c(0.0,1.0), 
    labels=c("Oversample",
      "Cross-sectional"))
  data$R1482600 <- factor(data$R1482600, 
    levels=c(1.0,2.0,3.0,4.0), 
    labels=c("Black",
      "Hispanic",
      "Mixed Race (Non-Hispanic)",
      "Non-Black / Non-Hispanic"))
  return(data)
}

varlabels <- c("PUBID - YTH ID CODE 1997",
  "HEIGHT OF R IN INCHES 1997",
  "WEIGHT OF R - POUNDS 1997",
  "R UNHAPPY, SAD, DEPRESSED (FEM) 1997",
  "R UNHAPPY, SAD, DEPRESSED (MALE) 1997",
  "R DESC WEIGHT 1997",
  "KEY!SEX (SYMBOL) 1997",
  "KEY!BDATE M/Y (SYMBOL) 1997",
  "KEY!BDATE M/Y (SYMBOL) 1997",
  "CV_SAMPLE_TYPE 1997",
  "KEY!RACE_ETHNICITY (SYMBOL) 1997"
)


# Use qnames rather than rnums

qnames = function(data) {
  names(data) <- c("ID",
    "HEIGHT",
    "WEIGHT",
    "MOOD_F",
    "MOOD_M",
    "WEIGHT_CAT",
    "SEX",
    "BDATE_M",
    "BDATE_Y",
    "CV_SAMPLE_TYPE_1997",
    "RACE")
  return(data)
}


#********************************************************************************************************

# Remove the '#' before the following line to create a data file called "categories" with value labels. 
categories <- vallabels(new_data)

# Remove the '#' before the following lines to rename variables using Qnames instead of Reference Numbers
new_data <- qnames(new_data)
categories <- qnames(categories)

# Produce summaries for the raw (uncategorized) data file
summary(new_data)

# Remove the '#' before the following lines to produce summaries for the "categories" data file.
#categories <- vallabels(new_data)
#summary(categories)

#************************************************************************************************************

print("This data was collected from the NLSinfo.org youth survey in 1997")

men <- hist(new_data$WEIGHT_CAT[new_data$SEX == 2])
women <- hist(new_data$WEIGHT_CAT[new_data$SEX == 1])
plot (men, col=rgb(0,0,1,1/4),main="Weight Catagory Distribution by Gender",xlab="Weight Catagories")
plot (women, col=rgb(1,0,0,1/4), add = T)
legend("topleft", c("Men","Women"), fill=c(rgb(0,0,1,1/4),rgb(1,0,0,1/4)))

print("By this Histogram, we can see that the males from the survey tended to be more overweight than the females.")

plot(new_data$WEIGHT,new_data$BDATE_Y,xlab = "Weight", ylab = "Age",main = "Weight affected by Age")
print("By this graph, we see the negative relation between birth year and weight.")


