# Load necessary libraries
library(readr)   
library(tableone) 
library(tidyr)
library(tictoc)
library(dplyr)
library(haven)
library(Hmisc)
library(purrr)

# Start the timer
tic()

# List of file paths
file_paths <- c("Final Data/21600-0001-Data.tsv",
                "Final Data/21600-0002-Data.tsv",
                "Final Data/21600-0008-Data.tsv",
                "Final Data/21600-0022-Data.tsv",
                "Final Data/21600-0029-Data.tsv",
                "Final Data/21600-0030-Data.tsv",
                "Final Data/21600-0032-Data.tsv",
                "Final Data/21600-0034-Data.tsv",
                "Final Data/21600-0035-Data.tsv",
                "Final Data/21600-0037-Data.tsv",
                "Final Data/21600-0039-Data.tsv",
                "Final Data/21600-0040-Data.tsv",
                "Final Data/21600-0041-Data.tsv")

# Read all the data into a list
data_list <- lapply(file_paths, function(file) {
  read.delim(file, header = TRUE, sep = "\t", na.strings = "")
})

data1  <- data_list[[1]]
data2  <- data_list[[2]]
data3  <- data_list[[3]]
data4  <- data_list[[4]]
data5  <- data_list[[5]]
data6  <- data_list[[6]]
data7  <- data_list[[7]]
data8  <- data_list[[8]]
data9  <- data_list[[9]]
data10 <- data_list[[10]]
data11 <- data_list[[11]]
data12 <- data_list[[12]]
data13 <- data_list[[13]]
# Stop the timer
toc()

tic()

data1 <- data_list[[1]] %>%
  # Select the variables
  select(AID, S2, H1GI1M, H1GI1Y, H1GI6A, H1GI6B, H1GI6C, H1GI6D, H1GI6E, 
         H1GI20, H1DA1, H1DA3, H1DA4, H1DA5, H1DA6, 
         H1GH23A, H1GH23B, H1GH23C, H1GH23D, H1GH23E, H1GH23F, 
         H1GH23G, H1GH23H, H1GH23I, H1GH23J, 
         H1GH28, H1GH37, H1GH38, H1GH59A, H1GH59B, H1GH60, H1PF31,
         H1TO2, H1TO3, H1TO4, H1TO5, H1TO7, H1TO15, H1TO16, H1TO17, H1TO31, H1TO35, H1TO38, H1TO41) %>%
  
  # Create the Race column and drop the original race variables
  mutate(Race = case_when(
    H1GI6A == 1 ~ "White",
    H1GI6B == 1 ~ "Black",
    H1GI6C == 1 ~ "American Indian or Native American",
    H1GI6D == 1 ~ "Asian or Pacific Islander",
    H1GI6E == 1 ~ "Other",
    H1GI6A == 6 | H1GI6B == 6 | H1GI6C == 6 | H1GI6D == 6 | H1GI6E == 6 ~ "Refused",
    H1GI6A == 8 | H1GI6B == 8 | H1GI6C == 8 | H1GI6D == 8 | H1GI6E == 8 ~ "Don't Know"
  )) %>%
  
  
  # Rename S2 to Sex
  rename(Sex = S2) %>%
  
  # Label the variables
  mutate(
    Sex = labelled(Sex, c("Male" = 1, "Female" = 2, "Multiple response" = 9)),
    H1DA1 = labelled(H1DA1, c("Not at all" = 0, "1 or 2 times" = 1, "3 or 4 times" = 2, "5 or more times" = 3, "Refused" = 6, "Don't know" = 8)),
    H1DA3 = labelled(H1DA3, c("Not at all" = 0, "1 or 2 times" = 1, "3 or 4 times" = 2, "5 or more times" = 3, "Refused" = 6, "Don't know" = 8)),
    H1DA4 = labelled(H1DA4, c("Not at all" = 0, "1 or 2 times" = 1, "3 or 4 times" = 2, "5 or more times" = 3, "Refused" = 6, "Don't know" = 8)),
    H1DA5 = labelled(H1DA5, c("Not at all" = 0, "1 or 2 times" = 1, "3 or 4 times" = 2, "5 or more times" = 3, "Refused" = 6, "Don't know" = 8)),
    H1DA6 = labelled(H1DA6, c("Not at all" = 0, "1 or 2 times" = 1, "3 or 4 times" = 2, "5 or more times" = 3, "Refused" = 6, "Don't know" = 8)),
    H1PF31 = labelled(H1PF31, c("Strongly agree" = 1, "Agree" = 2, "Neither agree nor disagree" = 3, "Disagree" = 4, "Strongly disagree" = 5, "Refused" = 6, "Don't know" = 8)),
    H1TO2 = labelled(H1TO2, c("Never smoked" = 0, "Refused" = 96, "Legitimate skip" = 97, "Don't know" = 98)),
    H1TO3 = labelled(H1TO3, c("No" = 0, "Yes" = 1, "Refused" = 6, "Legitimate skip" = 7)),
    H1TO4 = labelled(H1TO4, c("Under 1 year" = 0, "Refused" = 96, "Legitimate skip" = 97, "Don't know" = 98)),
    H1TO5 = labelled(H1TO5, c("No days" = 0, "Refused" = 96, "Legitimate skip" = 97, "Don't know" = 98)),
    H1TO7 = labelled(H1TO7, c("Refused" = 96, "Legitimate skip" = 97, "Don't know" = 98)),
    H1TO15 = labelled(H1TO15, c("Every day or almost every day" = 1, "3 to 5 days a week" = 2, "1 or 2 days a week" = 3, "2 or 3 days a month" = 4, "Once a month or less" = 5, "1 or 2 days in past 12 months" = 6, "Never" = 7, "Refused" = 96, "Legitimate skip" = 97, "Don't know" = 98)),
    H1TO16 = labelled(H1TO16, c("Refused" = 96, "Legitimate skip" = 97, "Don't know" = 98, "Not applicable" = 99)),
    H1TO17 = labelled(H1TO17, c("Every day or almost every day" = 1, "3 to 5 days a week" = 2, "1 or 2 days a week" = 3, "2 or 3 days a month" = 4, "Once a month or less" = 5, "1 or 2 days in past 12 months" = 6, "Never" = 7, "Refused" = 96, "Legitimate skip" = 97, "Don't know" = 98)),
    H1TO31 = labelled(H1TO31, c("Refused" = 996, "Legitimate skip" = 997, "Don't know" = 998, "Not applicable" = 999)),
    H1TO35 = labelled(H1TO35, c("Refused" = 996, "Legitimate skip" = 997, "Don't know" = 998, "Not applicable" = 999)),
    H1TO38 = labelled(H1TO38, c("Refused" = 996, "Legitimate skip" = 997, "Don't know" = 998, "Not applicable" = 999)),
    H1TO41 = labelled(H1TO41, c("Refused" = 996, "Legitimate skip" = 997, "Don't know" = 998, "Not applicable" = 999))
  ) %>%
  
  # Handle special codes for height in feet and inches
  mutate(
    H1GH59A = ifelse(H1GH59A %in% c(996, 998, 999), 0, H1GH59A),
    H1GH59B = ifelse(H1GH59B %in% c(996, 998, 999), 0, H1GH59B),
    HeightInW1 = (H1GH59A * 12) + H1GH59B
  ) %>%
  
  # Recode H1GH60
  mutate(
    H1GH60 = labelled(H1GH60, c("Refused" = 996, "Don't know" = 998, "Not applicable" = 999))
  ) %>%
  
  # Recode H1GI20 for school grade
  mutate(
    SchlGrdW1 = labelled(H1GI20, c("Refused" = 96, "Not in school" = 97, "Don't know" = 98, "NA-No grade levels" = 99))
  ) %>%
  
  # Create the Breakfast column using breakfast-related variables
  mutate(
    Dairy = ifelse(H1GH23A == 1, 1, 0),
    Grain_Carb = ifelse(H1GH23C == 1 | H1GH23H == 1, 1, 0),
    Fruit_Veg = ifelse(H1GH23D == 1, 1, 0),
    Protein = ifelse(H1GH23E == 1 | H1GH23F == 1, 1, 0),
    Other = ifelse(H1GH23B == 1 | H1GH23G == 1 | H1GH23I == 1, 1, 0),
    Nothing = ifelse(H1GH23J == 1, 1, 0),  # 1 if nothing is marked
    
    # Count of marked categories excluding "Nothing"
    Breakfast_Count = Dairy + Grain_Carb + Fruit_Veg + Protein + Other,
    
    # Define Breakfast Category based on counts
    Breakfast = case_when(
      Nothing == 1 ~ "No Breakfast",
      Breakfast_Count >= 2 ~ "Complete Breakfast",
      Breakfast_Count == 1 ~ "Minimal Breakfast",
      TRUE ~ NA_character_  # Missing if nothing is clearly categorized
    )
  ) %>%
  
  # Drop original race variables, month/year variables, and other intermediate variables
  select(-H1GI6A, -H1GI6B, -H1GI6C, -H1GI6D, -H1GI6E, 
         -H1GH23A, -H1GH23B, -H1GH23C, -H1GH23D, -H1GH23E, -H1GH23F, 
         -H1GH23G, -H1GH23H, -H1GH23I, -H1GH23J, -H1GH59A, -H1GH59B)

# View the resulting dataset
head(data1)

data2  <- data_list[[2]]

data2 <- data2 %>%
  mutate(
    MedHOInW1 = ifelse(BST90P15 %in% c(999998, 999999), NA, BST90P15),
    MedFMInW1 = ifelse(BST90P17 %in% c(999998, 999999), NA, BST90P17)
  ) %>% 
  select(AID,MedHOInW1,MedFMInW1)

head(data2)


data3 <- data_list[[3]]

# Selecting the necessary variables
data3 <- data_list[[3]] %>%
  select(AID, H3ID17, H3ID18, H3ID22, H3ID26E, H3ID26G, H3ID26H, 
         H3TO3, H3TO4, H3TO5, H3TO10, H3TO39, H3WGT, H3HGT_F, H3HGT_I, H3HGT_PI)

# Labelling the variables
data3 <- data3 %>%
  mutate(
    H3ID17 = labelled(H3ID17, c("Legitimate skip" = 97, "Don't know" = 98)),
    H3ID18 = labelled(H3ID18, c("Neither" = 0, "Pills" = 1, "Insulin" = 2, "Both" = 3, "Legitimate skip" = 7)),
    H3ID22 = labelled(H3ID22, c("No" = 0, "Yes" = 1, "Don't know" = 8)),
    H3ID26E = labelled(H3ID26E, c("Not marked" = 0, "Marked" = 1, "Legitimate skip" = 7)),
    H3ID26G = labelled(H3ID26G, c("Not marked" = 0, "Marked" = 1, "Legitimate skip" = 7)),
    H3ID26H = labelled(H3ID26H, c("Not marked" = 0, "Marked" = 1, "Legitimate skip" = 7)),
    H3TO3 = labelled(H3TO3, c( "Refused" = 96, "Legitimate skip" = 97, "Don't know" = 98, "Not applicable" = 99)),
    H3TO4 = labelled(H3TO4, c("No" = 0, "Yes" = 1, "Refused" = 6, "Legitimate skip" = 7, "Don't know" = 8, "Not applicable" = 9)),
    H3TO5 = labelled(H3TO5, c("Refused" = 96, "Legitimate skip" = 97, "Don't know" = 98, "Not applicable" = 99)),
    H3TO10 = labelled(H3TO10, c( "Refused" = 996, "Legitimate skip" = 997, "Don't know" = 998, "Not applicable" = 999)),
    H3TO39 = labelled(H3TO39, c( "Refused" = 96, "Legitimate skip" = 97, "Don't know" = 98, "Not applicable" = 99)),
    H3WGT = labelled(H3WGT, c( "Over 330 pounds" = 888, "Refused" = 996)),
    H3HGT_F = labelled(H3HGT_F, c("4 feet" = 4, "5 feet" = 5, "6 feet" = 6, "7 feet" = 7, "Refused" = 96, "Don't know" = 98)),
    H3HGT_I = labelled(H3HGT_I, c("0-11 inches" = 0:11, "Refused" = 96, "Don't know" = 98)),
    H3HGT_PI = labelled(H3HGT_PI, c("0.0 inches" = 0, "0.125 inches" = 0.125, "0.25 inches" = 0.25, "0.375 inches" = 0.375, "0.5 inches" = 0.5, 
                                    "0.625 inches" = 0.625, "0.75 inches" = 0.75, "0.875 inches" = 0.875, "Refused" = 6, "Don't know" = 8))
  )

# Combine the height columns into a single column and drop the original ones
data3 <- data3 %>%
  # Convert feet to inches, and sum feet, inches, and partial inches to create HeightIn_W2
  mutate(HeightIn_W2 = (H3HGT_F * 12) + H3HGT_I + H3HGT_PI) %>%
  
  # Drop the original height columns
  select(-H3HGT_F, -H3HGT_I, -H3HGT_PI)

# View the resulting dataset
head(data3)


data4  <- data_list[[4]]

# Selecting the necessary variables, renaming, merging, and labeling
data4 <- data4 %>%
  # Select the desired variables
  select(AID, H4OD1M, H4OD1Y, H4GH1, H4HS7, H4ID5B, H4ID5C, H4ID5D, H4ID5E, 
         H4ED1, H4ED2, H4ED9, H4EC1, H4TO2, H4TO3, H4TO4, H4TO5, H4TO6, 
         H4TO14, H4TO34, H4TO35, H4TO36, H4TO64C, H4SBP, H4DBP, H4BPCLS, 
         H4HGT, H4WGT, H4BMI, H4BMICLS) %>%
  
  # Merge H4OD1M and H4OD1Y into BirthMonYrW4
  mutate(BirthMonYrW4 = paste(H4OD1M, H4OD1Y, sep = "-")) %>%
  
  # Rename H4EC1 to MedHOInW4
  rename(MedHOInW4 = H4EC1) %>%
  
  # Drop the original H4OD1M and H4OD1Y variables
  select(-H4OD1M, -H4OD1Y) %>%
  
  # Label the selected variables
  mutate(
    H4GH1 = labelled(H4GH1, c("Excellent" = 1, "Very good" = 2, "Good" = 3, "Fair" = 4, "Poor" = 5)),
    H4HS7 = labelled(H4HS7, c("Within the past 3 months" = 1, "4 to 6 months ago" = 2, "7 to 9 months ago" = 3, 
                              "10 to 12 months ago" = 4, "Longer than 1 year ago but less than 2 years ago" = 5, 
                              "2 years ago or longer" = 6, "Never" = 7, "Don't know" = 98)),
    H4ID5B = labelled(H4ID5B, c("No" = 0, "Yes" = 1, "Refused" = 6, "Don't know" = 8)),
    H4ID5C = labelled(H4ID5C, c("No" = 0, "Yes" = 1, "Refused" = 6, "Don't know" = 8)),
    H4ID5D = labelled(H4ID5D, c("No" = 0, "Yes" = 1, "Refused" = 6)),
    H4ID5E = labelled(H4ID5E, c("No" = 0, "Yes" = 1, "Refused" = 6)),
    H4ED1 = labelled(H4ED1, c("Finished high school with diploma" = 1, "Earned a GED" = 2, 
                              "Earned a certificate of attendance or completion" = 3, 
                              "Did not receive diploma, GED, or certificate" = 4, "Don't know" = 8)),
    H4ED2 = labelled(H4ED2, c("8th grade or less" = 1, "Some high school" = 2, "High school graduate" = 3, 
                              "Some vocational training" = 4, "Completed vocational training" = 5, 
                              "Some college" = 6, "Completed bachelor's degree" = 7, "Some graduate school" = 8, 
                              "Completed master's degree" = 9, "Completed doctoral degree" = 11, 
                              "Some post-baccalaureate education" = 12, "Completed post-baccalaureate education" = 13, 
                              "Don't know" = 98)),
    H4ED9 = labelled(H4ED9, c("Finish high school or GED" = 1, "Vocational school less than 2 years" = 2, 
                              "Vocational school 2 or more years" = 3, "College program less than 2 years" = 4, 
                              "Associate's degree" = 5, "Bachelor's degree" = 6, "Master's degree" = 7, 
                              "PhD or equivalent" = 8, "Professional doctorate" = 9, "Refused" = 96, 
                              "Legitimate skip" = 97, "Don't know" = 98)),
    MedHOInW4 = labelled(MedHOInW4, c("Less than $5,000" = 1, "$5,000 to $9,999" = 2, "$10,000 to $14,999" = 3, 
                                      "$15,000 to $19,999" = 4, "$20,000 to $24,999" = 5, "$25,000 to $29,999" = 6, 
                                      "$30,000 to $39,999" = 7, "$40,000 to $49,999" = 8, "$50,000 to $74,999" = 9, 
                                      "$75,000 to $99,999" = 10, "$100,000 to $149,999" = 11, "$150,000 or more" = 12, 
                                      "Refused" = 96, "Don't know" = 98)),
    H4TO2 = labelled(H4TO2, c( "Refused" = 96, "Legitimate skip" = 97, "Don't know" = 98)),
    H4TO3 = labelled(H4TO3, c("No" = 0, "Yes" = 1, "Refused" = 6, "Legitimate skip" = 7, "Don't know" = 8)),
    H4TO4 = labelled(H4TO4, c( "Refused" = 96, "Legitimate skip" = 97, "Don't know" = 98)),
    H4TO5 = labelled(H4TO5, c( "Refused" = 96, "Don't know" = 98)),
    H4TO6 = labelled(H4TO6, c("Refused" = 996, "Legitimate skip" = 997, "Don't know" = 998)),
    H4TO14 = labelled(H4TO14, c( "Refused" = 96, "Legitimate skip" = 97, "Don't know" = 98)),
    H4TO34 = labelled(H4TO34, c( "Refused" = 96, "Legitimate skip" = 97, "Don't know" = 98)),
    H4TO35 = labelled(H4TO35, c("None" = 0, "1 or 2 days in the past 12 months" = 1, 
                                "Once a month or less (3 to 12 days in the past 12 months)" = 2, 
                                "2 or 3 days a month" = 3, "1 or 2 days a week" = 4, 
                                "3 to 5 days a week" = 5, "Every day or almost every day" = 6, 
                                "Refused" = 96, "Legitimate skip" = 97, "Don't know" = 98)),
    H4TO36 = labelled(H4TO36, c("Refused" = 96, "Legitimate skip" = 97, "Don't know" = 98)),
    H4TO64C = labelled(H4TO64C, c("No" = 0, "Yes" = 1, "Legitimate skip" = 7, "Don't know" = 8)),
    H4SBP = labelled(H4SBP, c( "Refused" = 996, "Legitimate skip" = 997, "Invalid data" = 999)),
    H4DBP = labelled(H4DBP, c( "Refused" = 996, "Legitimate skip" = 997, "Invalid data" = 999)),
    H4BPCLS = labelled(H4BPCLS, c("Normal: systolic <120, diastolic <80" = 1, 
                                  "Prehypertension: systolic 120-139 or diastolic 80-89" = 2, 
                                  "Hypertension I: systolic 140-159 or diastolic 90-99" = 3, 
                                  "Hypertension II: systolic 160+ or diastolic 100+" = 4, 
                                  "Refused" = 6, "Legitimate skip" = 7, "Invalid data" = 9)),
    H4HGT = labelled(H4HGT, c( "Refused" = 996, "Legitimate skip" = 997, "Invalid data" = 999)),
    H4WGT = labelled(H4WGT, c( "Over scale limit" = 888, 
                               "Weight inconsistent with height, waist, and sex" = 889, 
                               "Refused" = 996, "Legitimate skip" = 997, "Invalid data" = 999)),
    H4BMI = labelled(H4BMI, c( "Over scale limit" = 888, 
                               "Weight inconsistent with height, waist, and sex" = 889, 
                               "Refused" = 996, "Legitimate skip" = 997, "Invalid data" = 999)),
    H4BMICLS = labelled(H4BMICLS, c("Underweight: <18.5" = 1, "Normal: 18.5-<25" = 2, 
                                    "Overweight: 25-<30" = 3, "Obese I: 30-<35" = 4, 
                                    "Obese II: 35-<40" = 5, "Obese III: 40+" = 6, 
                                    "Over limit" = 88, "Weight inconsistent with height, waist, and sex" = 89, 
                                    "Refused" = 96, "Legitimate skip" = 97, "Invalid data" = 99))
  )

data4 <- data4 %>%
  # Convert height from cm to inches and rename the variable
  mutate(HeightIn_W4 = H4HGT * 0.393701) %>%
  # Convert weight from kg to pounds and rename the variable
  mutate(WeightIn_W4 = H4WGT * 2.20462) %>%
  # Drop the original height and weight variables
  select(-H4HGT, -H4WGT)

# View the resulting dataset
head(data4)

data5  <- data_list[[5]]

# Select the necessary variables, rename, and label them
data5 <- data5 %>%
  # Select the desired variables
  select(AID, GLUCOSE, HBA1C, C_FGLU, C_NFGLU, C_HBA1C, C_MED, C_JOINT) %>%
  
  # Label the variables without changing existing missing values
  mutate(
    GLUCOSE = labelled(GLUCOSE, c(
      "No result for glucose" = 999
    )),
    HBA1C = labelled(HBA1C, c(
      "No result for glucose" = 99
    )),
    C_FGLU = labelled(C_FGLU, c(
      "Glucose less than or equal to 99 mg/dl" = 1,
      "Glucose 100-125 mg/dl - Impaired Fasting Glucose (IFG)/Pre-Diabetes" = 2,
      "Glucose greater than or equal to 126 mg/dl - Diabetes" = 3,
      "Non-Fasting" = 4,
      "Fasting status unknown" = 5,
      "No result for glucose" = 9
    )),
    C_NFGLU = labelled(C_NFGLU, c(
      "Glucose less than or equal to 199 mg/dl" = 1,
      "Glucose greater than or equal to 200 mg/dl - Diabetes" = 2,
      "Fasting" = 3,
      "Fasting status unknown" = 4,
      "No result for glucose" = 9
    )),
    C_HBA1C = labelled(C_HBA1C, c(
      "HbA1c less than or equal to 5.6" = 1,
      "HbA1c 5.7-6.4 - Pre-Diabetes" = 2,
      "HbA1c greater than or equal to 6.5 - Diabetes" = 3,
      "No result for HbA1c" = 9
    )),
    C_MED = labelled(C_MED, c(
      "Did not report taking anti-diabetic medication" = 0,
      "Reported taking anti-diabetic medication" = 1
    )),
    C_JOINT = labelled(C_JOINT, c(
      "No Diabetes" = 0,
      "Has Diabetes" = 1
    ))
  )

# View the resulting dataset
head(data5)

data6  <- data_list[[6]]


# Create data6 by selecting and labeling the necessary variables, including AIS
data6 <- data6 %>%
  # Select the desired variables
  select(AID,C_MED2, C_JOINT2) %>%
  
  # Label the variables
  mutate(
    C_MED2 = labelled(C_MED2, c(
      "Did not report taking antihyperlipidemic medication" = 0,
      "Reported taking antihyperlipidemic medication" = 1
    )),
    C_JOINT2 = labelled(C_JOINT2, c(
      "No evidence of hyperlipidemia" = 0,
      "Evidence of hyperlipidemia" = 1
    ))
  )

# View the resulting dataset
head(data6)

# Load data7 from the list
data7 <- data_list[[7]]

# Select and label the necessary variables, including AID
data7 <- data7 %>%
  # Select the desired variables
  select(AID, H5OD1M, H5OD1Y, H5OD2A, H5OD8, H5OD11, H5EC1, H5EC2, H5ID1,
         H5ID2F, H5ID2I, H5ID3, H5ID6B, H5ID6BA, H5ID6BM, H5ID6C, H5ID6CA,
         H5ID6CM, H5ID6D, H5ID6DA, H5ID6DM, H5ID6E, H5ID6EA, H5ID6EM,
         H5TO1, H5TO2, H5TO3, H5TO11, H5TO12, H5TO13, H5TO14, H5EL6P) %>%
  
  # Combine H5OD1M and H5OD1Y into BirthMonYrW5 and rename other variables
  mutate(BirthMonYrW5 = paste(H5OD1M, H5OD1Y, sep = "-")) %>%
  rename(MedPIInW5 = H5EC1, MedHOInW5 = H5EC2) %>%
  
  # Create HeightIn_W5 by combining H5ID2F (feet) and H5ID2I (inches)
  mutate(
    H5ID2F = ifelse(H5ID2F == 98, NA, H5ID2F),  # Replace "Don't know" code with NA
    H5ID2I = ifelse(H5ID2I == 998, NA, H5ID2I), # Replace "Don't know" code with NA
    HeightIn_W5 = (H5ID2F * 12) + H5ID2I        # Combine feet and inches
  ) %>%
  
  # Drop the original month, year, feet, and inches columns
  select(-H5OD1M, -H5OD1Y, -H5ID2F, -H5ID2I) %>%
  
  # Label the variables
  mutate(
    H5OD2A = labelled(H5OD2A, c("Male" = 1, "Female" = 2)),
    H5OD8 = labelled(H5OD8, c(
      "White 111" = 1,
      "Black, African American" = 2,
      "Hispanic" = 3,
      "Hispanic: Mexican, Mexican American, Chicano" = 4,
      "Other Hispanic, Latino, or Spanish origin" = 7,
      "Asian: Filipino" = 11,
      "Other Asian" = 15,
      "American Indian or Alaska Native" = 21
    )),
    H5OD11 = labelled(H5OD11, c(
      "Some high school or lower" = 2,
      "High school diploma" = 3,
      "GED" = 4,
      "Some vocational/technical training (after high school)" = 5,
      "Some community college" = 6,
      "Completed vocational/technical training (after high school)" = 7,
      "Associate or junior college degree" = 8,
      "Some college" = 9,
      "Completed college (bachelor's degree)" = 10,
      "Some graduate school" = 11,
      "Completed a master's degree" = 12,
      "Some graduate training beyond a master's degree" = 13,
      "Completed a doctoral degree" = 14,
      "Some post baccalaureate professional education" = 15,
      "Completed a post baccalaureate professional degree" = 16
    )),
    MedPIInW5 = labelled(MedPIInW5, c(
      "Less than $5,000" = 1,
      "$5,000 to $9,999" = 2,
      "$10,000 to $14,999" = 3,
      "$15,000 to $19,999" = 4,
      "$20,000 to $24,999" = 5,
      "$25,000 to $29,999" = 6,
      "$30,000 to $39,999" = 7,
      "$40,000 to $49,999" = 8,
      "$50,000 to $74,999" = 9,
      "$75,000 to $99,999" = 10,
      "$100,000 to $149,999" = 11,
      "$150,000 to $199,999" = 12,
      "$200,000 or more" = 13
    )),
    MedHOInW5 = labelled(MedHOInW5, c(
      "Less than $5,000" = 1,
      "$5,000 to $9,999" = 2,
      "$10,000 to $14,999" = 3,
      "$15,000 to $19,999" = 4,
      "$20,000 to $24,999" = 5,
      "$25,000 to $29,999" = 6,
      "$30,000 to $39,999" = 7,
      "$40,000 to $49,999" = 8,
      "$50,000 to $74,999" = 9,
      "$75,000 to $99,999" = 10,
      "$100,000 to $149,999" = 11,
      "$150,000 to $199,999" = 12,
      "$200,000 or more" = 13
    )),
    H5ID1 = labelled(H5ID1, c(
      "Excellent" = 1,
      "Very Good" = 2,
      "Good" = 3,
      "Fair" = 4,
      "Poor" = 5
    )),
    H5ID6B = labelled(H5ID6B, c("No" = 0, "Yes" = 1)),
    H5ID6BA = labelled(H5ID6BA, c("No" = 0, "Yes" = 1, "Legitimate Skip" = 997)),
    H5ID6BM = labelled(H5ID6BM, c("Legitimate Skip" = 97)),
    H5ID6C = labelled(H5ID6C, c("No" = 0, "Yes" = 1)),
    H5ID6CA = labelled(H5ID6CA, c("Less than a year" = 0, "Legitimate Skip" = 997)),
    H5ID6CM = labelled(H5ID6CM, c("No" = 0, "Yes" = 1)),
    H5ID6D = labelled(H5ID6D, c("No" = 0, "Yes" = 1)),
    H5ID6DA = labelled(H5ID6DA, c("Less than a year" = 0, "Legitimate Skip" = 997)),
    H5ID6DM = labelled(H5ID6DM, c("No" = 0, "Yes" = 1, "Legitimate Skip" = 97)),
    H5ID6E = labelled(H5ID6E, c("No" = 0, "Yes" = 1)),
    H5ID6EA = labelled(H5ID6EA, c(
      "34 and younger" = 34,
      "40 and older" = 40,
      "Legitimate Skip" = 997
    )),
    H5ID6EM = labelled(H5ID6EM, c("No" = 0, "Yes" = 1, "Legitimate Skip" = 97)),
    H5TO1 = labelled(H5TO1, c("No" = 0, "Yes" = 1)),
    H5TO11 = labelled(H5TO11, c("No" = 0, "Yes" = 1)),
    H5TO12 = labelled(H5TO12, c(
      "None" = 0,
      "1 or 2 days in the past 12 months" = 1,
      "Once a month or less (3 to 12 days in the past 12 months)" = 2,
      "2 or 3 days a month" = 3,
      "1 or 2 days a week" = 4,
      "3 to 5 days a week" = 5,
      "Every day or almost every day" = 6
    )),
    H5TO13 = labelled(H5TO13, c(
      "None" = 0,
      "One day" = 1,
      "2 or 3 days" = 2,
      "1 day a week" = 3,
      "2 days a week" = 4,
      "3 to 5 days a week" = 5,
      "Every day or almost every day" = 6
    )),
    H5TO14 = labelled(H5TO14, c("Legitimate Skip" = 997)),
    H5EL6P = labelled(H5EL6P, c("No" = 0, "Yes" = 1))
  )

# View the resulting dataset
head(data7)

data8 <- data_list[[8]] %>%
  # Select the desired variables
  select(AID, H5BMICLS, H5HGT, H5WGT) %>%
  
  # Label the variables
  mutate(
    H5BMICLS = labelled(H5BMICLS, c(
      "Underweight: BMI is < 18.5" = 1,
      "Normal: BMI is 18.5 - 24.9" = 2,
      "Overweight: BMI is 25 - 29.9" = 3,
      "Obesity Stage I: BMI is 30 - 34.9" = 4,
      "Obesity Stage II: BMI is 35 - 39.9" = 5,
      "Obesity Stage III: BMI is >= 40" = 6
    )),
    H5HGT = labelled(H5HGT, c(
      "Exam stopped prior to measurement" = 9994,
      "Invalid data" = 9999
    )),
    H5WGT = labelled(H5WGT, c(
      "Exam stopped prior to measurement" = 9994,
      "Refused" = 9996,
      "Legitimate skip" = 9997,
      "Invalid data" = 9999
    ))
  )

# View the resulting dataset
head(data8)

data9 <- data_list[[9]] %>%
  # Select the desired variables
  select(AID, H5BPCLS5, H5Q045A, H5Q045B, H5Q045F, H5AHT) %>%
  
  # Label the variables
  mutate(
    H5BPCLS5 = labelled(H5BPCLS5, c(
      "Normal: systolic BP is < 120 and diastolic BP is < 80" = 1,
      "Elevated: systolic BP is 120-129 and diastolic BP is < 80" = 2,
      "Hypertension Stage 1: systolic BP is 130-139 or diastolic BP is 80-89" = 3,
      "Hypertension Stage 2: systolic BP is 140-180 or diastolic BP is 90-120" = 4,
      "Hypertension Crisis: systolic BP is > 180 or diastolic BP is > 120" = 5
    )),
    H5Q045A = labelled(H5Q045A, c(
      "No" = 0,
      "Yes" = 1,
      "Not asked of everyone" = 95
    )),
    H5Q045B = labelled(H5Q045B, c(
      "No" = 0,
      "Yes" = 1,
      "Not asked of everyone" = 95
    )),
    H5Q045F = labelled(H5Q045F, c(
      "No" = 0,
      "Yes" = 1,
      "Not asked of everyone" = 95
    )),
    H5AHT = labelled(H5AHT, c(
      "No" = 0,
      "Yes" = 1,
      "Not asked of everyone" = 95
    ))
  )

# View the resulting dataset
head(data9)


# Create data10 by selecting and labeling the necessary variables, including AID
data10 <- data_list[[10]] %>%
  # Select the desired variables
  select(AID, H5GLUCOS, H5HBA1C, H5CFGLU, H5CNFGLU, H5CHBA1C, H5Q045B, H5C_MED, H5DIABJC) %>%
  
  # Label the variables
  mutate(
    H5CFGLU = labelled(H5CFGLU, c(
      "Normal: glucose is < 100 mg/dl" = 1,
      "Impaired Fasting Glucose (IFG)/Pre-Diabetes: glucose is 100-125 mg/dl" = 2,
      "Diabetes: glucose is > 125 mg/dl" = 3,
      "Non-fasting" = 4,
      "Fasting status unknown" = 5
    )),
    H5CNFGLU = labelled(H5CNFGLU, c(
      "Normal: glucose is < 200 mg/dl" = 1,
      "Diabetes: glucose is >= 200 mg/dl" = 2,
      "Fasting" = 3,
      "Fasting status unknown" = 4
    )),
    H5CHBA1C = labelled(H5CHBA1C, c(
      "Normal: HbA1c (%) is < 5.7" = 1,
      "Pre-Diabetes: HbA1c (%) is 5.7 - 6.4" = 2,
      "Diabetes: HbA1c (%) is > 6.4" = 3
    )),
    H5Q045B = labelled(H5Q045B, c(
      "No" = 0,
      "Yes" = 1,
      "Not asked of everyone" = 95
    )),
    H5C_MED = labelled(H5C_MED, c(
      "No" = 0,
      "Yes" = 1,
      "Exam stopped prior to the medication questions" = 94,
      "Refused" = 96,
      "Respondent reported taking prescription medication(s), but specific medication(s) not reported" = 99
    )),
    H5DIABJC = labelled(H5DIABJC, c(
      "No evidence of diabetes" = 0,
      "Evidence of diabetes" = 1
    ))
  )

# View the resulting dataset
head(data10)


# Create data11 by selecting and labeling the necessary variables, including AID
data11 <- data_list[[11]] %>%
  # Select the desired variables
  select(AID, H5CTC, H5CHDL, H5C1TG, H5C1LDL, H5C2TG, H5C2LDL, H5Q045F, H5C_MED2) %>%
  
  # Label the variables
  mutate(
    H5CTC = labelled(H5CTC, c(
      "Desirable: TC is < 200 mg/dl" = 1,
      "Borderline high: TC is 200-239 mg/dl" = 2,
      "High: TC is >= 240 mg/dl" = 3,
      "Non-fasting" = 4,
      "Fasting status unknown" = 5
    )),
    H5CHDL = labelled(H5CHDL, c(
      "Low: HDL-C is < 40 mg/dl" = 1,
      "Optimal: HDL-C is 40-59 mg/dl" = 2,
      "High: HDL-C is >= 60 mg/dl" = 3,
      "Non-fasting" = 4,
      "Fasting status unknown" = 5
    )),
    H5C1TG = labelled(H5C1TG, c(
      "Normal: TG is < 150 mg/dl" = 1,
      "Borderline high: TG is 150-199 mg/dl" = 2,
      "High: TG is 200-499 mg/dl" = 3,
      "Very high: TG is >= 500 mg/dl" = 4,
      "Non-fasting" = 5,
      "Fasting status unknown" = 6
    )),
    H5C1LDL = labelled(H5C1LDL, c(
      "Optimal: LDL-C is < 100 mg/dl" = 1,
      "Near optimal: LDL-C is 100-129 mg/dl" = 2,
      "Borderline high: LDL-C is 130-159 mg/dl" = 3,
      "High: LDL-C is 160-189 mg/dl" = 4,
      "Very high: LDL-C is >= 190 mg/dl" = 5,
      "Non-fasting" = 6,
      "Fasting status unknown" = 7
    )),
    H5C2TG = labelled(H5C2TG, c(
      "Normal: TG is < 175 mg/dl" = 1,
      "Moderate hypertriglyceridemia: TG is 175-499 mg/dl" = 2,
      "Severe hypertriglyceridemia: TG is >= 500 mg/dl" = 3,
      "Non-fasting: TG is >= 500 mg/dl" = 4,
      "Fasting status unknown: TG is >= 500 mg/dl" = 5
    )),
    H5C2LDL = labelled(H5C2LDL, c(
      "Normal: LDL-C is < 160 mg/dl" = 1,
      "Moderate hypercholesterolemia: LDL-C is 160-189 mg/dl" = 2,
      "Severe hypercholesterolemia: LDL-C is >= 190 mg/dl" = 3
    )),
    H5Q045F = labelled(H5Q045F, c(
      "No" = 0,
      "Yes" = 1,
      "Not asked of everyone" = 95
    )),
    H5C_MED2 = labelled(H5C_MED2, c(
      "No" = 0,
      "Yes" = 1,
      "Exam stopped prior to the medication questions" = 94,
      "Refused" = 96,
      "Respondent reported taking prescription medication(s), but specific medication(s) not reported" = 99
    ))
  )

# View the resulting dataset
head(data11)



# Create data11 by selecting and labeling the necessary variables, including AID
data11 <- data_list[[11]] %>%
  # Select the desired variables
  select(AID, H5CTC, H5CHDL, H5C1TG, H5C1LDL, H5C2TG, H5C2LDL, H5Q045F, H5C_MED2) %>%
  
  # Label the variables
  mutate(
    H5CTC = labelled(H5CTC, c(
      "Desirable: TC is < 200 mg/dl" = 1,
      "Borderline high: TC is 200-239 mg/dl" = 2,
      "High: TC is >= 240 mg/dl" = 3,
      "Non-fasting" = 4,
      "Fasting status unknown" = 5
    )),
    H5CHDL = labelled(H5CHDL, c(
      "Low: HDL-C is < 40 mg/dl" = 1,
      "Optimal: HDL-C is 40-59 mg/dl" = 2,
      "High: HDL-C is >= 60 mg/dl" = 3,
      "Non-fasting" = 4,
      "Fasting status unknown" = 5
    )),
    H5C1TG = labelled(H5C1TG, c(
      "Normal: TG is < 150 mg/dl" = 1,
      "Borderline high: TG is 150-199 mg/dl" = 2,
      "High: TG is 200-499 mg/dl" = 3,
      "Very high: TG is >= 500 mg/dl" = 4,
      "Non-fasting" = 5,
      "Fasting status unknown" = 6
    )),
    H5C1LDL = labelled(H5C1LDL, c(
      "Optimal: LDL-C is < 100 mg/dl" = 1,
      "Near optimal: LDL-C is 100-129 mg/dl" = 2,
      "Borderline high: LDL-C is 130-159 mg/dl" = 3,
      "High: LDL-C is 160-189 mg/dl" = 4,
      "Very high: LDL-C is >= 190 mg/dl" = 5,
      "Non-fasting" = 6,
      "Fasting status unknown" = 7
    )),
    H5C2TG = labelled(H5C2TG, c(
      "Normal: TG is < 175 mg/dl" = 1,
      "Moderate hypertriglyceridemia: TG is 175-499 mg/dl" = 2,
      "Severe hypertriglyceridemia: TG is >= 500 mg/dl" = 3,
      "Non-fasting: TG is >= 500 mg/dl" = 4,
      "Fasting status unknown: TG is >= 500 mg/dl" = 5
    )),
    H5C2LDL = labelled(H5C2LDL, c(
      "Normal: LDL-C is < 160 mg/dl" = 1,
      "Moderate hypercholesterolemia: LDL-C is 160-189 mg/dl" = 2,
      "Severe hypercholesterolemia: LDL-C is >= 190 mg/dl" = 3
    )),
    H5Q045F = labelled(H5Q045F, c(
      "No" = 0,
      "Yes" = 1,
      "Not asked of everyone" = 95
    )),
    H5C_MED2 = labelled(H5C_MED2, c(
      "No" = 0,
      "Yes" = 1,
      "Exam stopped prior to the medication questions" = 94,
      "Refused" = 96,
      "Respondent reported taking prescription medication(s), but specific medication(s) not reported" = 99
    ))
  )

# View the resulting dataset
head(data11)



data12 <- data_list[[12]] %>%
  # Select the desired variables
  select(AID, H5EAHT, H5ECMED2, H5ECMED) %>%
  
  # Label the variables
  mutate(
    H5EAHT = labelled(H5EAHT, c(
      "No" = 0,
      "Yes" = 1,
      "Respondent reported taking prescription medication(s), but specific medication(s) not reported" = 99
    )),
    H5ECMED2 = labelled(H5ECMED2, c(
      "No" = 0,
      "Yes" = 1,
      "Respondent reported taking prescription medication(s), but specific medication(s) not reported" = 99
    )),
    H5ECMED = labelled(H5ECMED, c(
      "No" = 0,
      "Yes" = 1,
      "Respondent reported taking prescription medication(s), but specific medication(s) not reported" = 99
    ))
  )

# View the resulting dataset
head(data12)


data13 <- data_list[[13]] %>%
  # Select the desired variables
  select(AID, H5Q045D) %>%
  
  # Label the variables
  mutate(
    H5Q045D = labelled(H5Q045D, c(
      "No" = 0,
      "Yes" = 1,
      "Not asked of everyone" = 95
    ))
  )
head(data13)



toc()



