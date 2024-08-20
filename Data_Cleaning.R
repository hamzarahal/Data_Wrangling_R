##########Data Cleaning in R##################
################################################

### Detecting Outliers

# Load the tidyverse
library(tidyverse)

# Read in the Medicare payments dataset
names <- c("DRG", "ProviderID", "Name", "Address", "City", "State", "ZIP", "Region", "Discharges", "AverageCharges", "AverageTotalPayments", 
           "AverageMedicarePayments")
types = 'ccccccccinnn'
inpatient <- read_tsv('http://594442.youcanlearnit.net/inpatient.tsv', col_names = names, skip=1, col_types = types)

# Let's look at a histogram
ggplot(data=inpatient) + 
  geom_histogram(mapping=aes(x=AverageCharges))

# What if we change the limit of the y axis
ggplot(data=inpatient) + 
  geom_histogram(mapping=aes(x=AverageCharges)) +
  coord_cartesian(ylim=c(0,25))

# We could also view this with a single boxplot
ggplot(data=inpatient) + 
  geom_boxplot(mapping=aes("charges",AverageCharges))

# Or we can use a series of boxplots broken out by state
ggplot(data=inpatient) + 
  geom_boxplot(mapping=aes(State,AverageCharges))

# Plenty to investigate here, but let's dig in to those over $500,000
highCharges <- filter(inpatient, AverageCharges>500000)

unique(highCharges$DRG)

ggplot(data=highCharges) + 
  geom_point(mapping=aes(DRG,AverageCharges)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust=1))

### Missing and Special Values in R


names <- c("ID", "DBAName", "AKAName", "License", "FacilityType", "Risk", "Address", 
           "City", "State", "ZIP", "InspectionDate", "InspectionType", "Results",
           "Violations", "Latitude","Longitude","Location")

inspections <- read_csv('http://594442.youcanlearnit.net/inspections.csv', 
                        col_names=names, skip=1)

# Look at a summary of the data
summary(inspections)

# Which inspections have NA values for license?
unlicensed <- inspections %>%
  filter(is.na(License))

licensed <- inspections %>%
  filter(!is.na(License))

# What happens if I divide a number by 0

badmath <- c(1,2,3,4/0,0/0,NA)
badmath
is.na(badmath)
is.nan(badmath)
is.infinite(badmath)
is.finite(badmath)

### Breaking Apart Columns With Separate

inpatient <- read_tsv('http://594442.youcanlearnit.net/inpatient.tsv', col_names = names, skip=1, col_types = types)

# Take a look at the diagnosis-related group unique values
unique(inpatient$DRG)

# Let's try separating this on the hyphen
inpatient_separate <- separate(inpatient,DRG,c('DRGcode','DRGdescription'),'-')

# What's going on with those warning rows?  Let's look at row 45894
inpatient$DRG[45894]

# Let's separate with character position instead
inpatient_separate <- separate(inpatient,DRG,c('DRGcode','DRGdescription'),4)

# And take a look at the data now
glimpse(inpatient_separate)

### Combining Columns with unite()


names <- c("ID", "DBAName", "AKAName", "License", "FacilityType", "Risk", "Address", 
           "City", "State", "ZIP", "InspectionDate", "InspectionType", "Results",
           "Violations", "Latitude","Longitude","Location")

inspections <- read_csv('http://594442.youcanlearnit.net/inspections.csv', 
                        col_names=names, skip=1)

glimpse(inspections)

# Create a new column called Regions that combines City and State
regional_inspections <- unite(inspections,Region,City,State,sep=", ")

# Let's look at the data
glimpse(regional_inspections)

# Whoops. I didn't want to DELETE the City and State columns.  Let's try again.
regional_inspections <- unite(inspections,Region,City,State,sep=", ", remove=FALSE)
glimpse(regional_inspections)

# And take a look at the unique regions
unique(regional_inspections$Region)

### Manipulating Strings in R with stringr


names <- c("ID", "DBAName", "AKAName", "License", "FacilityType", "Risk", "Address", 
           "City", "State", "ZIP", "InspectionDate", "InspectionType", "Results",
           "Violations", "Latitude","Longitude","Location")

inspections <- read_csv('http://594442.youcanlearnit.net/inspections.csv', 
                        col_names=names, skip=1)

# Create a new column called Regions that combines City and State
regional_inspections <- unite(inspections,Region,City,State,sep=", ", remove=FALSE)

# And take a look at the unique regions
unique(regional_inspections$Region)

# We need to load stringr separately
library(stringr)


# Let's handle the uppercase/lowercase issues by converting everything to uppercase
regional_inspections <- regional_inspections %>%
  mutate(Region=str_to_upper(Region))


# What were the results of that?
unique(regional_inspections$Region)

# Let's take care of a few misspellings of Chicago
badchicagos <- c('CCHICAGO, IL', 'CHCICAGO, IL', 'CHICAGOCHICAGO, IL', 'CHCHICAGO, IL', 'CHICAGOI, IL')

regional_inspections <- regional_inspections %>%
  mutate(Region=ifelse(Region %in% badchicagos, 'CHICAGO, IL', Region)) 

# And see what's left
unique(regional_inspections$Region)

# There are some "CHICAGO, NA" values that we can clearly correct to "CHICAGO, IL"
regional_inspections <- regional_inspections %>%
  mutate(Region=ifelse(Region=='CHICAGO, NA', 'CHICAGO, IL', Region)) 

# But we don't know what to do with "NA, IL", "NA, NA", or "INACTIVE, IL"
# so let's set those to missing values
nachicagos <- c('NA, IL', 'NA, NA', 'INACTIVE, IL')

regional_inspections <- regional_inspections %>%
  mutate(Region=ifelse(Region %in% nachicagos, NA, Region)) 

# How did we do?
unique(regional_inspections$Region)

