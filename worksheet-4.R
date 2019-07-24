library(dplyr)
cbp2 <- filter(cbp,
grepl("----",NAICS),
!grepl('------', NAICS))
cbp2
library(stringr)
cbp2 <- filter(cbp,
str_detect(NAICS,"[0-9]{2}----"))
cbp3 <- mutate(cbp2,
FIPS=str_c(FIPSTATE,FIPSCTY))
cbp3 <- mutate(cbp2,
FIPS = str_c(FIPSTATE, FIPSCTY),
NAICS = str_remove(NAICS, '-+'))
c(1,3,5(%>%sum()))
c(1,3,5)%>% sum()
c(1,3,5,NA) %>% sum(na.rm=TRUE)
c(1,3,5,NA) %>% sum(na.rm=TRUE)
cbp<-cbp %>%
filter(
str_detect(NAICS, '[0-9]{2}----')
)  %>%
mutate(
FIPS = str_c(FIPSTATE, FIPSCTY),
NAICS = str_remove(NAICS, '-+')
)
cbp
names(cbp)
cbp<-cbp%>%
select(
FIPS,
NAICS,
starts_with('N')
)
cbp
sector <- fread(
'data/ACS/sector_naics.csv',
colClasses = c(NAICS = 'character'))
sector
cbp <- cbp %>%
inner_join(sector)
cbp <- cbp %>%
inner_join(sector)
View(cbp)
cbp_grouped <- cbp %>%
group_by(FIPS,Sector)
str(cbp_grouped)
cbp <- cbp %>%
group_by(FIPS, Sector) %>%
select(starts_with("N"),-NAICS)%>%
summarize_all(sum)
cbp_grouped <- cbp %>%
group_by(FIPS,Sector)
str(cbp_grouped)
## Summarize
cbp <- cbp %>%
group_by(FIPS, Sector) %>%
select(starts_with("N"),-NAICS)%>%
summarize_all(sum)
## Tidy Concept
trial <- read.delim(sep = ',', header = TRUE, text = "
block, drug, control, placebo
1, 0.22,    0.58,    0.31
2, 0.12,    0.98,    0.47
3, 0.42,    0.19,    0.40
")
## Gather
library(tidyr)
tidy_trial <- gather(trial,
key = treatment,
value = response,
-block) #specifies everything excetp the block column
## Spread
survey <- read.delim(sep = ',', header = TRUE, text = "
participant,   attr, val
1          ,    age,  24
2          ,    age,  57
3          ,    age,  13
1          , income,  30
2          , income,  60
")
tidy_survey <- spread(survey,
key = attr,
value = val)
tidy_survey <- spread(survey,
key = attr,
value = val,
fill = 0)
## Sample Data
library(data.table)
cbp <- fread('data/cbp15co.csv')
#head(cbp)
str(cbp)
cbp <- fread(
'data/cbp15co.csv',
na.strings=NULL,
colClasses=c(FIPSTATE="character",
FIPSCTY="character"))
acs <- fread(
'data/ACS/sector_ACS_15_5YR_S2413.csv',
colClasses = c(FIPS = 'character'))
str(acs)
## dplyr Functions
library(dplyr)
cbp2 <- filter(cbp,
grepl("----",NAICS),
!grepl('------', NAICS))
library(stringr)
cbp2 <- filter(cbp,
str_detect(NAICS,"[0-9]{2}----"))
cbp3 <- mutate(cbp2,
FIPS=str_c(FIPSTATE,FIPSCTY))
cbp3 <- mutate(cbp2,
FIPS = str_c(FIPSTATE, FIPSCTY),
NAICS = str_remove(NAICS, '-+'))
#
#c(1,3,5,NA) %>% sum(na.rm=TRUE)
#
cbp<-cbp %>%
filter(
str_detect(NAICS, '[0-9]{2}----')
)  %>%
mutate(
FIPS = str_c(FIPSTATE, FIPSCTY),
NAICS = str_remove(NAICS, '-+')
)
cbp<-cbp%>%
select(
FIPS,
NAICS,
starts_with('N')
)
## Join
sector <- fread(
'data/ACS/sector_naics.csv',
colClasses = c(NAICS = 'character'))
cbp <- cbp %>%
inner_join(sector)
## Group By
cbp_grouped <- cbp %>%
group_by(FIPS,Sector)
str(cbp_grouped)
## Summarize
cbp <- cbp %>%
group_by(FIPS, Sector) %>%
select(starts_with("N"),-NAICS)%>%
summarize_all(sum)
View(cbp)
acs_cbp <- cbp %>%
inner_join(acs)
acs_cbp
head(survey)
survey
trial
tidy_trial <- gather(trial,
key = treatment,
value = response,
-block)
tidy_trial
trial
survey
head(tidy_survey)
long_survey<-gather(participant,
key=attr,
value=val)
long_survey<-gather(tidy_survey,
key=attr,
value=val)
long_survey
tidy_survey
long_survey<-gather(tidy_survey,
key=attr,
value=val,
-participant)
long_survey
#Exercise 2: Use filter and select to return just the annual payroll data for the top level
#construction sector (“23—-").
head(cbp)
str(cbp)
cbp_23_hw_e2 <- fread('data/cbp15co.csv', na.strings = '')
cbp_23_hw_e2
cbp_23_hw_e2 <- fread('data/cbp15co.csv', na.strings = '') %>%
filter(NAICS == '23----')
cbp_23_hw_e2
cbp_23_hw_e2 <- fread('data/cbp15co.csv', na.strings = '') %>%
filter(NAICS == '23----') %>%
starts_with('AP'))
cbp_23_hw_e2 <- fread('data/cbp15co.csv', na.strings = '') %>%
filter(NAICS == '23----') %>%
starts_with('AP')
cbp_23_hw_e2 <- fread('data/cbp15co.csv', na.strings = '') %>%
filter(NAICS == '23----')
cbp_23_hw_e2
cbp_23_hw_e2 <- fread('data/cbp15co.csv', na.strings = '') %>%
filter(NAICS == '23----') %>%
select(starts_with('AP')
cbp_23_hw_e2
cbp_23_hw_e2 <- fread('data/cbp15co.csv', na.strings = '') %>%
filter(NAICS == '23----') %>%
select(starts_with('AP'))
cbp_23_hw_e2
cbp_23_hw_e2 <- fread('data/cbp15co.csv', na.strings = '') %>%
filter(NAICS == '23----') %>%
select('AP')
cbp_23_hw_e2
head(cbp_23_hw_e2)
head(cbp)
cbp_23_hw_e2 <- fread('data/cbp15co.csv', na.strings = '') %>%
filter(NAICS == '23----') %>%
select("FIPS","AP")
head(cbp_23_hw_e2)
head(cbp)
head(cbp)
cbp_23_hw_e2 <- fread('data/cbp15co.csv', na.strings = '')
head(cbp_23_hw_e2)
cbp_23_hw_e2 <- fread('data/cbp15co.csv', na.strings = '') %>%
filter(NAICS == '23----') %>%
select("AP",starts_with("FIPS"))
head(cbp_23_hw_e2)
cbp_23_hw_e2 <- fread('data/cbp15co.csv', na.strings = '') %>%
filter(NAICS == '23----') %>%
select(starts_with("FIPS"),"AP")
head(cbp_23_hw_e2)
cbp_21 <- fread('data/cbp15co.csv', na.strings = '') %>%
#only include oil and gas counties (NAICS = "21____")
filter(NAICS == "21----") %>%
#group by FIPSTATE and FIPSCITY
group_by(FIPSTATE, FIPSCTY) %>%
#summarise by employment
summarize(EMP = sum(EMP))
cbp_21
cbp_21 <- fread('data/cbp15co.csv', na.strings = '') %>%
#only include oil and gas counties (NAICS = "21____")
filter(NAICS == "21----") %>%
#group by FIPSTATE and FIPSCITY
group_by(FIPSTATE, FIPSCTY) %>%
#summarise by employment
summarize(EMP = sum(EMP)) %>%
summarize(EMP = sum(EMP), counties = n())
cbp_21
cbp_21_emp <- fread('data/cbp15co.csv', na.strings = '') %>%
#only include oil and gas counties (NAICS = "21____")
filter(NAICS == "21----") %>%
#group by FIPSTATE and FIPSCITY
group_by(FIPSTATE, FIPSCTY) %>%
#sum employment in each
summarize(EMP = sum(EMP))
cbp_21_emp
cbp_21_counties <- fread('data/cbp15co.csv', na.strings = '') %>%
#only include oil and gas counties (NAICS = "21____")
filter(NAICS == "21----") %>%
#group by FIPSTATE and FIPSCITY
group_by(FIPSTATE, FIPSCTY) %>%
#sum employment in each
# summarize(EMP = sum(EMP))
summarize(EMP = sum(EMP), counties = n())
cbp_21_counties
cbp_21_counties <- fread('data/cbp15co.csv', na.strings = '') %>%
#only include oil and gas counties (NAICS = "21____")
filter(NAICS == "21----") %>%
#group by FIPSTATE and FIPSCITY
group_by(FIPSTATE, FIPSCTY) %>%
#sum employment in each
summarize(EMP = sum(EMP))
summarize(EMP = sum(EMP), counties = n())
cbp_21_counties
cbp_21_counties <- fread('data/cbp15co.csv', na.strings = '') %>%
#only include oil and gas counties (NAICS = "21____")
filter(NAICS == "21----") %>%
#group by FIPSTATE and FIPSCITY
group_by(FIPSTATE, FIPSCTY) %>%
#sum employment in each
summarize(EMP = sum(EMP))%>%
summarize(EMP = sum(EMP), counties = n())
cbp_21_counties
cbp_21_counties <- fread('data/cbp15co.csv', na.strings = '') %>%
#only include oil and gas counties (NAICS = "21____")
filter(NAICS == "21----") %>%
#group by FIPSTATE and FIPSCITY
group_by(FIPSTATE, FIPSCTY) %>%
#sum employment in each
summarize(EMP = sum(EMP))
cbp_21_counties
cbp_21_counties <- fread('data/cbp15co.csv', na.strings = '') %>%
#only include oil and gas counties (NAICS = "21____")
filter(NAICS == "21----") %>%
#group by FIPSTATE and FIPSCITY
group_by(FIPSTATE, FIPSCTY) %>%
#sum employment in each
summarize(EMP = sum(EMP))%>%
#sum counties in each state
summarize(EMP = sum(EMP), counties = n())
cbp_21_counties
pivot <- fread('data/cbp15co.csv', na.strings = '')%>%
#select out what to use
str_detect(NAICS,"[0-9]{2}----"))
#group by state anf 2 digit NAICS code
group_by(FIPSTATE, NAICS) %>%
#get the sum of the # of employees
summarize(EMP = sum(EMP))
## Tidy Concept
trial <- read.delim(sep = ',', header = TRUE, text = "
block, drug, control, placebo
1, 0.22,    0.58,    0.31
2, 0.12,    0.98,    0.47
3, 0.42,    0.19,    0.40
")
## Gather
library(tidyr)
tidy_trial <- gather(trial,
key = treatment,
value = response,
-block) #specifies everything excetp the block column
## Spread
survey <- read.delim(sep = ',', header = TRUE, text = "
participant,   attr, val
1          ,    age,  24
2          ,    age,  57
3          ,    age,  13
1          , income,  30
2          , income,  60
")
tidy_survey <- spread(survey,
key = attr,
value = val)
tidy_survey <- spread(survey,
key = attr,
value = val,
fill = 0)
## Sample Data
library(data.table)
cbp <- fread('data/cbp15co.csv')
#head(cbp)
str(cbp)
cbp <- fread(
'data/cbp15co.csv',
na.strings=NULL,
colClasses=c(FIPSTATE="character",
FIPSCTY="character"))
acs <- fread(
'data/ACS/sector_ACS_15_5YR_S2413.csv',
colClasses = c(FIPS = 'character'))
str(acs)
## dplyr Functions
library(dplyr)
cbp2 <- filter(cbp,
grepl("----",NAICS),
!grepl('------', NAICS))
library(stringr)
cbp2 <- filter(cbp,
str_detect(NAICS,"[0-9]{2}----"))
cbp3 <- mutate(cbp2,
FIPS=str_c(FIPSTATE,FIPSCTY))
cbp3 <- mutate(cbp2,
FIPS = str_c(FIPSTATE, FIPSCTY),
NAICS = str_remove(NAICS, '-+'))
#
#c(1,3,5,NA) %>% sum(na.rm=TRUE)
#
cbp<-cbp %>%
filter(
str_detect(NAICS, '[0-9]{2}----')
)  %>%
mutate(
FIPS = str_c(FIPSTATE, FIPSCTY),
NAICS = str_remove(NAICS, '-+')
)
cbp<-cbp%>%
select(
FIPS,
NAICS,
starts_with('N')
)
## Join
sector <- fread(
'data/ACS/sector_naics.csv',
colClasses = c(NAICS = 'character'))
cbp <- cbp %>%
inner_join(sector)
## Group By
cbp_grouped <- cbp %>%
group_by(FIPS,Sector)
str(cbp_grouped)
## Summarize
cbp <- cbp %>%
group_by(FIPS, Sector) %>%
select(starts_with("N"),-NAICS)%>%
summarize_all(sum)
acs_cbp <- cbp %>%
inner_join(acs)
##HOMEWORK
#Exercise 1:Now that we have a tidy form of survey, convert it to a long_survey data frame using gather.
#The only difference between survey and long_survey should be an additional row for zero income.
head(tidy_survey)
long_survey<-gather(tidy_survey,
key=attr,
value=val,
-participant)
#Exercise 2: Use filter and select to return just the annual payroll data for the top level
#construction sector (“23—-").
head(cbp)
str(cbp)
cbp_23_hw_e2 <- fread('data/cbp15co.csv', na.strings = '') %>%
    #reads in the data, considers missing data to be NAs
filter(NAICS == '23----') %>%
    #includes only NAICS that start with 23
select(starts_with("FIPS"),"AP")
  #selects only rows that are either named "AP", or start with "FIPS"
head(cbp_23_hw_e2)
  #look at it
#Exercise 3: Write code to create a data frame giving, for each state, the number of counties in
#the CBP survey with establishements in mining or oil and gas extraction (‘21—-‘) along with
#their total employment (“EMP”). Group the data using bothFIPSTATE and FIPSCTY and use the fact
#that one call to summarize only combines across the lowest level of grouping. The dplyr
#function n counts rows in a group.
#read in data, assign nas as any blanks
cbp_21_emp <- fread('data/cbp15co.csv', na.strings = '') %>%
#only include oil and gas counties (NAICS = "21____")
filter(NAICS == "21----") %>%
#group by FIPSTATE and FIPSCITY
group_by(FIPSTATE, FIPSCTY) %>%
#sum employment in each
summarize(EMP = sum(EMP))
cbp_21_counties <- fread('data/cbp15co.csv', na.strings = '') %>%
#only include oil and gas counties (NAICS = "21____")
filter(NAICS == "21----") %>%
#group by FIPSTATE and FIPSCITY
group_by(FIPSTATE, FIPSCTY) %>%
#sum employment in each
summarize(EMP = sum(EMP))%>%
#sum counties in each state
summarize(EMP = sum(EMP), counties = n())

#Exercise 4: A “pivot table” is a transformation of tidy data into a wide summary table.
#First, data are summarized by two grouping factors, then one of these is “pivoted” into columns.
#Starting from a filtered CBP data file, chain a split-apply-combine procedure into the tidyr
#function spread to get the total number of employees (“EMP”) in each state (as rows) by
#2-digit NAICS code (as columns).
pivot <- fread('data/cbp15co.csv', na.strings = '')%>%
#select out what to use
str_detect(NAICS,"[0-9]{2}----"))
#group by state anf 2 digit NAICS code
group_by(FIPSTATE, NAICS) %>%
#get the sum of the # of employees
summarize(EMP = sum(EMP))


pivot <- fread('data/cbp15co.csv', na.strings = '')%>%
# head(pivot)
#select out what to use
str_detect(NAICS,"[0-9]{2}----"))%>%
#group by state and 2 digit NAICS code
group_by(FIPSTATE, NAICS) %>%
#get the sum of the # of employees
summarize(EMP = sum(EMP))%>%
spread(key = NAICS, value = EMP)

savehistory("~/handouts/worksheet-4.R")
