# Data cleaning after direct download from NoroSurv.org

#load libraries
pacman::p_load("dplyr", "ggplot2", "readxl", "stringr",
               "lubridate", "splitstackshape")

# create "not in" function
'%notin%' <- Negate('%in%')


############ import raw data from Norosurv.org ############
#file name includes download date
# setwd()
# NoroSurv <- read_excel()


########### General data cleaning

#rename headings to make them easier to refer to in R
NoroSurv <- rename(NoroSurv,
                   "Date_creation" = `Creation Date`,
                   "SampleID" = `Sample ID`,
                   "PtypeAuto" = `P-type (auto)`,
                   "CtypeAuto" = `C-type (auto)`,
                   "PtypeUser" = `P-type (user)`,
                   "CtypeUser" = `C-type (user)`,
                   "Date" = `Collection Date`,
                   "Age" = `Patient Age (months)`,
                   "Symptoms" = `Gastroenteritis Symptoms`,
                   "Sample_type" = `Sample Type`
                   )


# Add column to indicate dataset (NoroSurv or GDPS(WHO))
# WHO reference labs:
WHO_ref_countries_exceptIndia <- c("Fiji", "Laos", "Vietnam","Belarus","Armenia", 
                                   "Moldova", "Tajikistan", "Ukraine", "Uzbekistan", 
                                   "Bolivia",  "Ecuador", "Peru", "Honduras", "Paraguay", 
                                   "China", "Ghana", "Benin", "Cote D’Ivoire", "Burkina Faso", 
                                   "Nigeria",  "Myanmar", "Indonesia", "Ethiopia", "Madagascar",  
                                   "Uganda", "Zimbabwe")
# India not included here because there are WHO and NoroSurv sites

NoroSurv_countries_excptIndia <- c("Hong Kong", "Germany", "Canada", "New Zealand", 
                                   "Spain", "Philippines", "Bangladesh", "Chile", 
                                   "Taiwan", "Japan", "Nicaragua", "United States", 
                                   "Australia", "Argentina", "Brazil", "South Africa", 
                                   "Thailand", "Zambia","Costa Rica")
# note there are currently no samples submitted from Argentina

Mixed_NS_WHO_countries <- c("India")
# India has sites that are NoroSurv and sites that are WHO
India_WHO_sites <- c("AP04", "HR09", "KL15", "TN01", "WHO") #Sites in India to not include as they are WHO sites only
India_NoroSurv_sites <- c("CMC", "Vellore")
#two CMC India samples which are WHO samples below: now "site" field is changed to "WHO" on server
#"TN010746GI", "TN010746GII"

#create column for indicating samples are WHO or Norosurv
NoroSurv <- NoroSurv %>%
  mutate(dataset = 
           case_when(
             Country %in% WHO_ref_countries_exceptIndia | 
               Site %in% India_WHO_sites 
             ~ "WHO",
             Country %in% NoroSurv_countries_excptIndia |
               Site %in% India_NoroSurv_sites
             ~ "NoroSurv",
             Country %notin% c(WHO_ref_countries_exceptIndia, 
                               NoroSurv_countries_excptIndia, "India")
             ~ "not listed")) #include variable for countries that are not listed above

# create dataframe for entries with countries that are not listed and need to be checked
check_country <- NoroSurv %>%
  filter(Country == "not listed") %>%
  .$SampleID


# Change string variables to factors
cols <- c("Status", "QC", "PtypeAuto", "CtypeAuto", 
          "PtypeUser", "CtypeUser", "Gender", "Site", 
          "Country", "City", "Setting", "Symptoms", "Sample_type")
NoroSurv[cols] <- lapply(NoroSurv[cols], factor)


# Site
# correct special characters in the site variable and trim whitespace
NoroSurv$Site <- iconv(NoroSurv$Site, from = 'UTF-8', to = 'ASCII//TRANSLIT') #to covert special characters
NoroSurv$Site <- if_else(NoroSurv$Site == "Â ICDDRb", "ICDDRb", NoroSurv$Site) #remove special character
NoroSurv$Site <- trimws(NoroSurv$Site, which = "both")
NoroSurv$Site <- as.factor(NoroSurv$Site)


# City
# Modify cities to title-case
NoroSurv$City <- str_to_title(as.character(NoroSurv$City))
NoroSurv$City <- as.factor(NoroSurv$City)


# Sample type
# modify all to lower case
NoroSurv$Sample_type <- as.factor(tolower(NoroSurv$Sample_type))


# Sample ID: look for spaces " " rather than "_"
IDspace <- " "
check_SampleID <- NoroSurv %>% 
  filter(str_detect(SampleID, IDspace)) %>%
  .$SampleID
#if any are detected, change on norosurv.org and verify

# Sample ID: look for duplicates
# any duplicates in SampleID
duplicated_sampleIDs <- NoroSurv$SampleID[duplicated(NoroSurv$SampleID)]


#Age: convert to numeric
NoroSurv$Age <- as.numeric(NoroSurv$Age)
age_over72m <- NoroSurv %>%
  filter(Age > 72 | is.na(Age)) %>%
  .$SampleID


#Date: Collection date change to lubridate format
NoroSurv$Date <- lubridate::ymd(NoroSurv$Date)
collection_date_range <- NoroSurv %>%
  filter(Date == "1970-01-01" | is.na(Date))  %>%
  .$SampleID
# Ecuador sample collection dates with "1970-01-01" are samples with an unknown sample collection date


#Creation Date: change to lubridate format
NoroSurv$Date_creation <- lubridate::ymd(NoroSurv$Date_creation)



#### Create Date plotting variations and NoroYear ####
# Create categories for plotting data by date (year, month-year, NoroYear)
# "NoroYear" is defined by Norovirus seasons spanning Sept 1 to Aug 31 each year

# Month
NoroSurv$Month <- month(NoroSurv$Date)
#Year
NoroSurv$Year <- year(NoroSurv$Date)
#Month_Year_floor as date rounded to first day of month
NoroSurv$Month_Year_floor <- lubridate::floor_date(NoroSurv$Date, unit="month")
#Month_Year_letter as words/characters- three letter month and two-digit year
NoroSurv$Month_Year_letter <-format(NoroSurv$Month_Year_floor, format = "%b %y")
NoroSurv$Month_Year_letter <- as.factor(NoroSurv$Month_Year_letter)


#NoroYear
NoroSurv <- NoroSurv %>%
  mutate(NoroYear = case_when(
    Date == "1970-01-01" ~ "unknown",  #for samples with unknown collection date
    Date != "1970-01-01" & Date < "2016-09-01" ~ "2015-2016",
    Date >= "2016-09-01" & Date <= "2017-08-31" ~ "2016-2017",
    Date >= "2017-09-01" & Date <= "2018-08-31" ~ "2017-2018",
    Date >= "2018-09-01" & Date <= "2019-08-31" ~ "2018-2019",
    Date >= "2019-09-01" & Date <= "2020-08-31" ~ "2019-2020",
    Date >= "2020-09-01" & Date <= "2021-08-31" ~ "2020-2021",
    Date >= "2021-09-01" & Date <= "2022-08-31" ~ "2021-2022",
    Date >= "2022-09-01" & Date <= "2023-08-31" ~ "2022-2023",
    Date > "2023-08-31" ~ "update",
    TRUE ~ NA_character_))
# set NoroYear as factor
NoroSurv$NoroYear <- as.factor(NoroSurv$NoroYear)
# list those with "unknown" or "update" noroyear
NoroYear_check <- NoroSurv %>%
  filter(NoroYear %in% c("update", "unknown")) %>%
  .$SampleID










######### Update historical classification with updated #########
# The NoroSurv database was updated with the current classification and nomenclature 
# for noroviruses. However, some of the older data uses the old classification 
# for the Auto P and C types. 

# Ptypes (Auto) update with current classification system ####
NoroSurv$PtypeAuto <- as.character(NoroSurv$PtypeAuto) #convert to char to modify
#convert auto types to current classification
NoroSurv <- NoroSurv %>%
  mutate(PtypeAuto = 
           case_when(PtypeAuto == "GI.Pa" ~ "GI.P10",
                     PtypeAuto == "GI.Pb" ~ "GI.P11_PNA1", #could be either P11 or PNA1
                     PtypeAuto == "GI.Pc" ~ "GI.P12",
                     PtypeAuto == "GI.Pd" ~ "GI.P13",
                     PtypeAuto == "GI.Pf" ~ "GI.P14",
                     PtypeAuto == "GII.Pa" ~ "GII.P29",
                     PtypeAuto == "GII.Pc" ~ "GII.P30",
                     PtypeAuto == "GII.Pe" ~ "GII.P31",
                     PtypeAuto == "GII.Pf" ~ "GII.P32",
                     PtypeAuto == "GII.Pg" ~ "GII.P33_P41", #could be P33 or P41
                     PtypeAuto == "GII.Ph" ~ "GII.P34",
                     PtypeAuto == "GII.Pj" ~ "GII.P35",
                     PtypeAuto == "GII.Pk" ~ "GII.P36",
                     PtypeAuto == "GII.Pm" ~ "GII.P37",
                     PtypeAuto == "GII.Pn" ~ "GII.P38",
                     PtypeAuto == "GII.P1" ~ "GII.P1_P39_PNA3", #could be P1, P39 or PNA3
                     PtypeAuto == "GII.P22" ~ "GII.P22_P40_PNA5", #could be P22, P40 or PNA5
                     PtypeAuto == "GII.PNA2" ~ "GII.PNA1_PNA2", #could be PNA1 or PNA2
                     PtypeAuto == "GII.PNA4" ~ "GII.PNA2",
                     PtypeAuto == "GII.P11" ~ "GII.P11_PNA4", #could be P11 or PNA4
                     PtypeAuto == "GI" ~ "GNA1.P1",
                     TRUE ~ PtypeAuto))
NoroSurv$PtypeAuto <- as.factor(NoroSurv$PtypeAuto)
# list sampleIDs for those with no PtypeAuto listed
PtypeAuto_missing <- NoroSurv %>%
  filter(is.na(PtypeAuto)) %>%
  .$SampleID
# list those with ambiguous PtypeAuto to change in code
PtypeAuto_ambig <- NoroSurv %>%
  filter(PtypeAuto %in% c("GI.P11_PNA1", "GII.P33_P41", "GII.P1_P39_PNA3",
                          "GII.P22_P40_PNA5", "GII.PNA1_PNA2", "GII.P11_PNA4")) %>%
  .$SampleID


#change GII.P4 variants (New Orleans) to P4
NoroSurv$PtypeAuto <- as.character(NoroSurv$PtypeAuto)
NoroSurv$PtypeAuto <- if_else(NoroSurv$PtypeAuto == "GII.P4 New Orleans", 
                              "GII.P4", 
                              NoroSurv$PtypeAuto)
NoroSurv$PtypeAuto <- as.factor(NoroSurv$PtypeAuto)


###### Ctypes (Auto) update with current classification system ####
# Next, examine the Auto Ctypes and look for GII.15 which is now GIX.1 
# and GII.22 which is now either GII.22 or GII.25. 
NoroSurv$CtypeAuto <-as.character(NoroSurv$CtypeAuto)
#convert auto types to current classification
NoroSurv <- NoroSurv %>%
  mutate(CtypeAuto =
           case_when(CtypeAuto== "GII.15" ~ "GIX.1",
                     CtypeAuto == "GII.22" ~ "GII.22_25",
                     TRUE ~ CtypeAuto))
NoroSurv$CtypeAuto <-as.factor(NoroSurv$CtypeAuto)
levels(NoroSurv$CtypeAuto)

# list sampleIDs for those with no PtypeAuto listed
CtypeAutotypeAuto_missing <- NoroSurv %>%
  filter(is.na(CtypeAuto)) %>%
  .$SampleID
# list those with ambiguous PtypeAuto to change in code
CtypeAuto_ambig <- NoroSurv %>%
  filter(CtypeAuto == "GII.22_25") %>%
  .$SampleID






######## Check for errors in User entries for PtypeUser and CtypeUser
# change PtypeUser "GII.P4 New Orleans" to "GII.P4"
NoroSurv$PtypeUser <- as.character(NoroSurv$PtypeUser)
NoroSurv$PtypeUser <- if_else(NoroSurv$PtypeUser == "GII.P4 New Orleans", 
                              "GII.P4", 
                              NoroSurv$PtypeUser)
NoroSurv$PtypeUser <- as.factor(NoroSurv$PtypeUser)

# change spelling of "untypeable" to "untypable" for consistency in PtypeUser column
NoroSurv$PtypeUser <-as.character(NoroSurv$PtypeUser)
NoroSurv <- NoroSurv %>%
  mutate(PtypeUser =
           case_when(PtypeUser %in% c("GI untypeable", 
                                      "GI.P untypeable", "GI.P untypable", 
                                      "GI.Puntypeable","GI.Puntypable",
                                      "untypable", "untypeable",
                                      "UNTYPABLE", "UNTYPEABLE") ~ 
                       "GI untypable",
                     PtypeUser %in% c("GII untypeable", 
                                      "GII.P untypeable", "GII.P untypable", 
                                      "GII.Puntypeable","GII.Puntypable",
                                      "untypable", "untypeable",
                                      "UNTYPABLE", "UNTYPEABLE") ~ 
                       "GII untypable",
                     TRUE ~ PtypeUser))
NoroSurv$PtypeUser <-as.factor(NoroSurv$PtypeUser)


# check for PtypeUser entries that contain typos- invalid Ptype

valid_Ptypes <- c("GI.P1", "GI.P2", "GI.P3", "GI.P4", "GI.P5", "GI.P6", 
                  "GI.P7", "GI.P8", "GI.P9", "GI.P10", "GI.P11", "GI.P12", 
                  "GI.P13", "GI.P14", "GI.PNA1", "GI.PNA2", "GI.PNA3", 
                  "GI.PNA4", "GI untypable",
                  "GII.P1","GII.P2","GII.P3", "GII.P4", "GII.P5", "GII.P6",
                  "GII.P7", "GII.P8", "GII.P11", "GII.P12", "GII.P13", 
                  "GII.P15","GII.P16", "GII.P17", "GII.P18", "GII.P19", 
                  "GII.P20", "GII.P21","GII.P22", "GII.P23", "GII.P24", 
                  "GII.P25", "GII.P26", "GII.P27", "GII.P28","GII.P29", 
                  "GII.P30", "GII.P31", "GII.P32", "GII.P33", "GII.P34", 
                  "GII.P35","GII.P36", "GII.P37", "GII.P38", "GII.P39", 
                  "GII.P40", "GII.P41","GII.PNA1", "GII.PNA2","GII.PNA3",
                  "GII.PNA4", "GII.PNA5","GII.PNA6","GII.PNA7", 
                  "GII.PNA8","GII.PNA9", "GII untypable",
                  "GNA1.P1")



# change spelling of "untypeable" to "untypable" for consistency in PtypeUser column
NoroSurv$CtypeUser <-as.character(NoroSurv$CtypeUser)
NoroSurv <- NoroSurv %>%
  mutate(CtypeUser =
           case_when(CtypeUser == "GI untypeable" ~ "GI untypable",
                     CtypeUser == "GII untypeable" ~ "GII untypable",
                     CtypeUser == "GII.4 untypeable" ~ "GII.4 untypable",
                     TRUE ~ CtypeUser))
NoroSurv$CtypeUser <-as.factor(NoroSurv$CtypeUser)

# trim white space
NoroSurv$CtypeUser <- trimws(NoroSurv$CtypeUser, which = "both")



# check for CtypeUser entries that contain typos- invalid Ctype
valid_Ctypes <- c("GI.1","GI.2","GI.3","GI.4","GI.5","GI.6","GI.7","GI.8","GI.9",
                  "GII.1","GII.2","GII.3","GII.4","GII.4 Sydney", "GII.4 New Orleans",
                  "GII.4 Hong Kong", "GII.4 Den Haag", "GII.4 untypable",
                  "GII.5","GII.6","GII.7","GII.8", "GII.9","GII.10","GII.11",
                  "GII.12","GII.13","GII.14","GII.16","GII.17","GII.18","GII.19",
                  "GII.20","GII.21","GII.22","GII.23","GII.24","GII.25","GII.26",
                  "GII.27","GII.NA1","GII.NA2","GIV.1","GIV.2","GIV.NA1",
                  "GVIII", "GIX.1")





#### Add Genogroup information ####
# list all possible genotypes in each genogroup: based on Chhabra et al., 2019 and HuCaT (https://norovirus.ng.philab.cdc.gov/names.cgi)
GI <- c("GI untypable", "GI.1","GI.2","GI.3","GI.4","GI.5","GI.6","GI.7","GI.8","GI.9")
GII <- c("GII untypable","GII.1","GII.2","GII.3","GII.5","GII.6","GII.7","GII.8",
         "GII.9","GII.4", "GII.4 Sydney","GII.4 New Orleans","GII.4 Den Haag",
         "GII.4 Hong Kong","GII.4 untypable", 
         "GII.10","GII.11","GII.12","GII.13","GII.14","GII.16","GII.17","GII.18",
         "GII.19","GII.20","GII.21","GII.22","GII.23","GII.24","GII.25","GII.26",
         "GII.27","GII.NA1","GII.NA2") 
GIX <- c("GIX.1", "GIX")
GIII <- c("GIII.1","GIII.2","GIII.3")
GIV <- c("GIV.1","GIV.2","GIV.NA1")



#create new column for Genogroup
NoroSurv$CtypeUser <- as.character(NoroSurv$CtypeUser)
NoroSurv <-  NoroSurv %>%
  mutate(Genogroup = 
           case_when(CtypeUser %in% GI ~ "GI",
                     CtypeUser %in% GII ~ "GII",
                     CtypeUser %in% GIX ~ "GIX.1",
                     CtypeUser %in% GIII ~"GIII",
                     CtypeUser %in% GIV ~ "GIV",
                     is.na(CtypeUser) ~ "blank",
                     TRUE ~ "check")) #included to check that any other genogroups 
                                      #detected or those without fasta file are identified
NoroSurv$CtypeUser <- as.factor(NoroSurv$CtypeUser)
NoroSurv$Genogroup <- as.factor(NoroSurv$Genogroup)


# list those that are not blank, but contain an invalid genogroup
Check_genogroup <- NoroSurv %>% 
  filter(Genogroup == "check") %>% 
  .$SampleID



#### GII.4 variant collapse (Ctype4) ####
#Create a column (Ctype4) that removes the variant names for GII.4 samples, 
# collapsing them into a single genotype

GII.4 <- c("GII.4", "GII.4 Den Haag",  "GII.4 Hong Kong", "GII.4 Sydney", 
           "GII.4 untypable", "GII.4 New Orleans")

NoroSurv$CtypeUser <- as.character(NoroSurv$CtypeUser)
NoroSurv <- NoroSurv %>%
  mutate(Ctype4 =  if_else(CtypeUser %in% GII.4,
                           "GII.4",
                           CtypeUser))
NoroSurv$Ctype4 <- as.factor(NoroSurv$Ctype4)












#### Create column for dual type ####
# some modifications are first needed in order to achieve the dual type nomenclature 
# modifications done first to copy of NoroSurv for easier back tracking if necessary
NoroSurv_test <- NoroSurv 

#split PtypeUser to capture Ptype after "." 
NoroSurv_test <- cSplit(NoroSurv_test, "PtypeUser", ".", drop = FALSE)

# if there are NAs present, they are due to "no fasta" for Region B -OR-
# deleted because they are WHO samples -OR-
# because they are P untypable -OR-
# due to an invalid Ptype
#we lose the GI untypable and GII untypable samples because 
# they do not contain a ".". They are converted to NAs. Same for invalid P types.

# if there are invalid Ptypes or untypables, denote those with "invalid" or "P untypable"
NoroSurv_test <- NoroSurv_test %>%
  mutate(PtypeUser_2 = case_when(
    PtypeUser_1 %notin% c("GI","GII","GIII","GIV","GIX") & 
      !is.na(PtypeUser_1) &
      PtypeUser_1 %notin% c("GI.P untypable", "GII.P untypable") ~ 
      "invalid",
    PtypeUser_1 %in% c("GI.P untypable", "GII.P untypable") ~ 
      "P untypable",
    is.na(PtypeUser_1) ~ "missing",
    TRUE ~ as.character(PtypeUser_2)
  ))

#remove PtypeUser_1 column as it is no longer needed
NoroSurv_test <- within(NoroSurv_test, rm("PtypeUser_1"))

#add brackets in new column and remove spaces for P types
NoroSurv_test <-  NoroSurv_test %>%
  mutate(PtypeUser_3 = paste("[",PtypeUser_2,"]", sep=""))

#create new column CtypeUser_1 to change NAs to "missing" for 
# easier filtering after merging for dual type
# also indicate those that have an invalid CtypeUser
NoroSurv_test <- NoroSurv_test %>%
  mutate(CtypeUser_1 = case_when(
    is.na(CtypeUser) ~ "missing", 
    CtypeUser %notin% valid_Ctypes ~ "invalid",
    TRUE ~ CtypeUser)
    )
    
#paste together CtypeUser_1 and PtypeUser_3 and name it dual_type
NoroSurv_test <-  NoroSurv_test %>%
  mutate(dual_type = paste0(CtypeUser_1, PtypeUser_3))

#check the levels to look for errors in naming dual types
NoroSurv_test$dual_type <- as.factor(NoroSurv_test$dual_type)


# remove unnecessary columns
NoroSurv_test <- within(NoroSurv_test, rm("PtypeUser_2"))
NoroSurv_test <- within(NoroSurv_test, rm("PtypeUser_3"))
NoroSurv_test <- within(NoroSurv_test, rm("CtypeUser_1"))

#convert NoroSurv_test to NoroSurv
NoroSurv <- NoroSurv_test
# remove the test df
rm(NoroSurv_test)



# there was a work around where WHO sample P and C User types were removed and
# the dual types were listed in the Comments section
# We will extract those and combine them with the dual_type column

# find all entries with dualtype in Comments section (all contain "[")
# create column for those manual dual types
NoroSurv <- NoroSurv %>%
  mutate(manual_dualtypes = case_when(
    (grepl("\\[", Comment)) ~ Comment,
     TRUE ~ "")
    )

# make "" NAs
NoroSurv$manual_dualtypes[NoroSurv$manual_dualtypes == ""] <- NA


# check that the manual_dualtypes contain valid C and P types
# first separate the C and P types from manual_dualtypes
NoroSurv <- cSplit(NoroSurv, "manual_dualtypes", "P", drop = FALSE)
# then remove the "[" and "]"
NoroSurv$manual_dualtypes_1 <- gsub("\\[", "", 
                                    NoroSurv$manual_dualtypes_1)
NoroSurv$manual_dualtypes_2 <- gsub("\\]", "", 
                                    NoroSurv$manual_dualtypes_2)
# rename columns to reflect content
NoroSurv <- rename(NoroSurv,
                   "Ctype_manual" = manual_dualtypes_1,
                   "Ptype_manual" = manual_dualtypes_2)


# Split Ctype_manual again to extract Genogroup info for the manual entries
NoroSurv <- cSplit(NoroSurv, "Ctype_manual", ".", drop = FALSE)
# then rename
NoroSurv <- rename(NoroSurv, "genogroup_manual" = Ctype_manual_1)
# and remove unneeded column
NoroSurv <- NoroSurv %>%
  dplyr::select(-Ctype_manual_2)


# modify the Ptype_manual column to read as Ptype format
NoroSurv <-  NoroSurv %>%
  mutate(Ptype_manual = if_else(
    !is.na(Ptype_manual), 
    paste0(genogroup_manual, ".P", Ptype_manual),
    Ptype_manual)
  )

# check the validity of the manual C and P types
# list those with invalid Ptype_manual
invalid_Ptype_manual <- NoroSurv %>%
  filter(Ptype_manual %notin% valid_Ptypes & !is.na(Ptype_manual)) %>%
  .$SampleID

# list those with invalid Ctype_manual
# first correct those with "untypeable" to "untypable"
NoroSurv$Ctype_manual <- gsub("untypeable", "untypable", 
                                    NoroSurv$Ctype_manual)
invalid_Ctype_manual <- NoroSurv %>%
  filter(Ctype_manual %notin% valid_Ctypes & !is.na(Ctype_manual)) %>%
  .$SampleID


# merge manual_dualtypes and dual_type columns
# first a test
NoroSurv <- NoroSurv %>%
  mutate(dual_type = if_else(
    !is.na(manual_dualtypes) & grepl("missing", dual_type),
    as.character(manual_dualtypes),
    as.character(dual_type)
  ))

# change "untypeable" to "untypable"
NoroSurv$dual_type <- gsub("untypeable", "untypable", 
                              NoroSurv$dual_type)

# merge Ctype_manual to CtypeUser column
NoroSurv <- NoroSurv %>%
  mutate(CtypeUser = if_else(
    !is.na(Ctype_manual) & is.na(CtypeUser),
    as.character(Ctype_manual),
    as.character(CtypeUser)
  ))

# merge Ptype_manual to PtypeUser column
NoroSurv <- NoroSurv %>%
  mutate(PtypeUser = if_else(
    !is.na(Ptype_manual) & is.na(PtypeUser),
    as.character(Ptype_manual),
    as.character(PtypeUser)
  ))

# determine Ctype4 for manual entries and modify
NoroSurv <- NoroSurv %>%
  mutate(Ctype4 =  if_else(CtypeUser %in% GII.4,
                           "GII.4",
                           CtypeUser))



# merge genotype_manual to Genotype column
NoroSurv <- NoroSurv %>%
  mutate(Genogroup = if_else(
    !is.na(genogroup_manual), 
    as.character(genogroup_manual),
    as.character(Genogroup)
  ))



# Change string variables to factors
cols <- c("PtypeUser","CtypeUser","dataset", "Genogroup",
          "Ctype4","dual_type")
NoroSurv <- as.data.frame(NoroSurv)
NoroSurv[cols]  <- lapply(NoroSurv[cols], factor)





####### Remove (filter out SampleIDs with missing/invalid/other error info)

# list those with invalid PtypeUser
invalid_PtypeUser <- NoroSurv %>%
  filter(PtypeUser %notin% valid_Ptypes & !is.na(PtypeUser)) %>%
  .$SampleID
# missing PtypeUser
missing_PtypeUser <- NoroSurv %>%
  filter(is.na(PtypeUser)) %>%
  .$SampleID
# list those with invalid CtypeUser
invalid_CtypeUser <- NoroSurv %>%
  filter(CtypeUser %notin% valid_Ctypes & !is.na(CtypeUser)) %>%
  .$SampleID
# missing CtypeUser
missing_CtypeUser <- NoroSurv %>%
  filter(is.na(CtypeUser)) %>%
  .$SampleID
# GII.4 with no variant name
GII.4_no_variant <-  NoroSurv %>%
  filter(dual_type %in% c("GII.4[P4]", "GII.4[P31]", "GII.4[P16]", "GII.4[P12]")) %>%
           .$SampleID

# invalid or missing Genogroup
invalid_genogroup <- NoroSurv %>%
  filter(Genogroup %in% c("invalid", "blank")) %>%
  .$SampleID



# create vector of all SampleIDs for this list
check_on_NoroSurv_server <- c(check_country, duplicated_sampleIDs,
                              age_over72m, collection_date_range,
                              invalid_PtypeUser, missing_PtypeUser,
                              invalid_CtypeUser, missing_CtypeUser,
                              GII.4_no_variant,invalid_genogroup)
check_on_NoroSurv_server <- unique(check_on_NoroSurv_server)
length(check_on_NoroSurv_server)

# filter out the samples from this list
NoroSurv_filtered <- NoroSurv %>%
  filter(SampleID %notin% check_on_NoroSurv_server)


# create dataframe with those that need to be checked
df_check_server <- NoroSurv %>%
  filter(SampleID %in% check_on_NoroSurv_server)




#### Filter out WHO dataset
NoroSurv_filtered <- NoroSurv_filtered %>%
  filter(dataset == "NoroSurv")


################Next Steps ##################


#### overwrite the file below when changes are made
write.csv(NoroSurv_filtered, 
          "", 
          row.names = FALSE)



################################################end












