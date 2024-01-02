## load packages
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)

# load edited dataset from REDCAP. Filtered for dates between 2023/01/01-2024/01/01
Dataset<- read.csv("Data1 (11-30).csv")

## rename for individual OB submission
Dataset<- Dataset %>%
  mutate(record_id = ifelse(row_number() == 87, "237-1", record_id),
         record_id = ifelse(row_number()== 88, "237-2", record_id),
         record_id = ifelse(row_number()== 89, "237-3", record_id),
         record_id = ifelse(row_number()== 90, "237-4", record_id),
         )

## Remove unnecessary rows due to duplicate submissions, survey edits without case updates, and surveys that do not fit childcare, school and employer settings
data <-Dataset %>% 
  drop_na(case_count)

# format date column from character class to date class
data<- data %>%
  mutate(date_hidden = as.Date(mdy(date_hidden)))

## create new columns for school, childcare, and employment and aggregate columns together

data <- data %>%
  mutate(
    school = ifelse(category == 2 & (category_industry_service_desc == 1 | category_industry_service_desc == 6), 1, NA),
    childcare = ifelse(category == 2 & (category_industry_service_desc == 5 | category_industry_service_desc == 4 | category_industry_service_desc == 99), 2, NA),
    employment = ifelse(category %in% c(3, 4, 5, 6, 7, 99), 3, NA),
    SCHD_category= coalesce(school,childcare, employment)
   )

# convert to factors
data <- data %>%
  mutate(school = as.factor(school),
         childcare = as.factor(childcare),
         employment = as.factor(employment),
         SCHD_category = as.factor(SCHD_category)
         )

-------------------------------------
data%>% count(SCHD_category)##check counts



----------------------------------
sum(is.na(data$SCHD_category)) #check NA


# create df for school only, childcare only, and employment only case counts

school <- data %>% filter(!is.na(school))
childcare <- data %>% filter(!is.na(childcare))
employment <- data%>% filter(!is.na(employment))

-----------------------
# check total cases
sum(school$case_count)
sum(childcare$case_count)
sum(employment$case_count)

# create aggregate case counts per group

school_group<-school%>%
  group_by(record_id)%>%
  arrange(date_hidden)%>%
  summarise(first_submission= first(date_hidden),
            case_count_group = sum(case_count)
            )

school_group<- school_group%>%
  mutate(Outbreak = ifelse(case_count_group>=5,1,NA))


childcare_group<-childcare%>%
  group_by(record_id)%>%
  arrange(date_hidden)%>%
  summarise(first_submission= first(date_hidden),
            case_count_group = sum(case_count)
  )

childcare_group<- childcare_group%>%
  mutate(Outbreak = ifelse(case_count_group>=5,1,NA))

employment_group<- employment%>%
  group_by(record_id)%>%
  arrange(date_hidden)%>%
  summarise(first_submission= first(date_hidden),
            case_count_group= sum(case_count)
            )


employment_group <- employment_group %>%
  mutate(Outbreak = ifelse((case_count_group >= 2 & first_submission >= "2023-01-01" & first_submission <= "2023-09-08") |
        (first_submission >= "2023-09-08" & case_count_group >= 5),1,NA)
          )# criteria to meet outbreak changed 09/08/2023



# Combine the dataframes
data_group <- bind_rows(
  mutate(school_group, category = "school"),
  mutate(childcare_group, category = "childcare"),
  mutate(employment_group, category = "employment")
)

# create month column for data_group

month_order <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")

data_group<- data_group%>%
  mutate(Month= factor(format(first_submission, "%B"), levels = month_order))

data_group%>%
  count(Outbreak) #85 outbreaks, 125 non ob

# Outbreak
outbreak<- data_group%>%
  filter(Outbreak==1)

# non-ob
nonob<- data_group%>%
  filter(is.na(Outbreak))

------------------------------
# check number of outbreaks
sum(!is.na(school_group$Outbreak)) #17
sum(!is.na(childcare_group$Outbreak)) #28
sum(!is.na(employment_group$Outbreak)) #40

-------------------------------
# check case counts
sum(school_group$case_count_group) #224
sum(childcare_group$case_count_group) #414
sum(employment_group$case_count_group) #202


----------------------------------
sum(data_group$case_count_group)
sum(data$case_count) #840

# count number of cases
outbreak%>%
  summarise(sum(case_count_group)) #634

# count number of cases
nonob%>%
  summarise(sum(case_count_group)) #206

-----------------------------------
  monthly_total<-data_group%>%
  group_by(Month,category)%>%
  summarise(Total = sum(case_count_group))

  
Month<-monthly_total%>%
  group_by(Month)%>%
  summarise(Total=sum(Total))




####Summary Statistics
# entire dataset
summary(data$case_count)
summary(data_group$case_count_group)
# childcare
summary(childcare$case_count)
summary(childcare_group$case_count_group)
# school
 
# employment
summary(employment$case_count)
summary(employment_group$case_count_group)
# outbreak
summary(outbreak$case_count_group)


#############################################################
#Graphs



#  bar plot for total cases
ggplot(Month, aes(x = Month, y = Total, fill = "skyblue")) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Total), vjust = -0.5, color = "black", size = 3) +  # Display total numbers on top of bars
  labs(x = "Month",
       y = "Total Case Count") +
  theme_minimal() +
  scale_fill_identity()

# bar plot of Monthly COVID-19 total cases
ggplot(monthly_total, aes(x = Month, y = Total, fill = category)) +
  geom_bar(stat = "identity", position = "dodge") +
 
  labs(title = "Comparison of Monthly Reported COVID-19 Cases per Category",
       x = "Month",
       y = "Total Case Count",
       fill = "Category") +
  theme_minimal()







