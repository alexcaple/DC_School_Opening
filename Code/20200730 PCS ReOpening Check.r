### PCS Reopening Choices by School
# Start Date: 7/30/2020
# Sources & Notes----------------------------------------------------------
# * * NEED PCS Citation 
#
# Library Load ------------------------------------------------------------
# General
library(readxl)
library(writexl)
library(tidyr)
library(dplyr)
library(reshape2)
library(tidyverse)
library(lubridate)

# Scraping
library(xml2)
library(rvest)
library(magrittr) # grabs iframes
library(RCurl)

# Graphical
library(ggplot2)
library(ggrepel)
library(scales)
library(Cairo)
library(viridisLite)
library(extrafont)
font_import(pattern = "calibri", prompt = FALSE)

# Mapping
library(ggmap)
library(maps)
library(sf)

# Here Set Up --------------------------------------------------------------------
library(here)

# Source ------------------------------------------------------------------

# Functions ---------------------------------------------------------------

# Data Online Grabs / Updates ---------------------------------------------

# * * PCS Current Data Download
# Actual Webpage: 
# CURRENT iframe link:

url_pcs_googledoc <- "https://docs.google.com/document/u/1/d/e/2PACX-1vSblyJB7-0cmT7kgYPzVb6VTjrdsWldpku0-DjYPQ2EuuluQG12RgQ_E4X3hr5UsYL_mpNiK01omHGy/pub?embedded=true"

table_pcs_raw <- url_pcs_googledoc %>% 
  read_html() %>% 
  html_nodes("table") %>%
  extract(1) %>% 
  html_table(fill=TRUE) %>% 
  map_df(., extract, c(1, 2, 3, 4)) %>% 
  as_tibble(.) %>% 
  rename(PCS.Name = X1,
         Start.Date = X2,
         Model = X3,
         Link = X4) %>% 
  mutate(Date = Sys.Date()) %>% 
  filter(!(PCS.Name == "Public Charter School (PCS)"))

write.csv(table_pcs_raw, file=here("Data", paste0(gsub("-", "", Sys.Date()), "_PCS_Reopening_Plans.csv")))

# * * OSSE 2019-20 Enrollment Data
temp <- tempfile(fileext = ".xlsx")
url_osse <- "https://osse.dc.gov/sites/default/files/dc/sites/osse/page_content/attachments/SY19-20%20Annual%20Enrollment%20Audit%20Report%20Supplemental%20Tables.xlsx"
download.file(url_osse, destfile=temp, mode='wb')
table_osse_enroll <- read_excel(temp, sheet = 3)
colnames(table_osse_enroll) <- make.names(names(table_osse_enroll), unique = TRUE)

# * * DME 2019-20 Location File
temp <- tempfile(fileext = ".xlsx")
url_dme <- "https://dme.dc.gov/sites/default/files/dc/sites/dme/publication/attachments/SY19-20_Facility%20Summary%20for%20Download_2.xlsx"
download.file(url_dme, destfile=temp, mode='wb')
table_dme <- read_excel(temp, sheet = 1)
colnames(table_dme) <- make.names(names(table_dme), unique = TRUE)

# Data Cleaning ----------------------------------------------------------------

# * * PCS Data

table_pcs <- table_pcs_raw %>% 
  mutate(LEA.Name = ifelse(PCS.Name == "Achievement Prep PCS", "Achievement Preparatory Academy PCS",
                    ifelse(PCS.Name == "BASIS DC PCS", "Basis DC PCS",
                    ifelse(PCS.Name == "YouthBuild PCS", "Youthbuild PCS",
                    ifelse(PCS.Name == 'AppleTree PCS', 'AppleTree Early Learning PCS',
                    ifelse(PCS.Name == 'The Children's Guild PCS', "The Children's Guild DC PCS",
                    ifelse(PCS.Name == 'Community College Prep Academy PCS', 'Community College Preparatory Academy PCS',
                    ifelse(PCS.Name == 'DC International School', 'District of Columbia International School',
                    ifelse(PCS.Name == 'E.L. Haynes PCS (all campuses)', 'E.L. Haynes PCS',
                    ifelse(PCS.Name == 'The Family Place', 'The Family Place PCS',
                    ifelse(PCS.Name == 'Friendship PCS (all campuses and online)', 'Friendship PCS',
                    ifelse(PCS.Name == 'Howard University Middle School of Math and Science PCS', 'Howard University Middle School of Mathematics and Science PCS',
                    ifelse(PCS.Name == 'KIPP DC (all campuses)', 'KIPP DC PCS',
                    ifelse(PCS.Name == 'Monument PCS', 'Monument Academy PCS',
                    ifelse(PCS.Name == 'Mundo Verde Bilingual PCS (all campuses)', 'Mundo Verde Bilingual PCS',
                    ifelse(PCS.Name == 'Perry Street Prep PCS', 'Perry Street Preparatory PCS',
                    ifelse(PCS.Name == 'Richard Wright PCS for the Performing Arts', 'Richard Wright PCS for Journalism and Media Arts',
                    ifelse(PCS.Name == 'Rocketship DC PCS (all campuses)', 'Rocketship DC PCS',
                    ifelse(PCS.Name == 'St.Coletta PCS', 'St. Coletta Special Education PCS',
                    ifelse(PCS.Name == 'Statesmen College Preparatory Academy for Boys', 'Statesman College Preparatory Academy for Boys PCS',
                    ifelse(PCS.Name == 'Two Rivers PCS (all campuses)', 'Two Rivers PCS', as.character(PCS.Name))))))))))))))))))))))

table_pcs <- merge(table_pcs,
                   table_osse_enroll %>% 
                     distinct(LEA.Code, .keep_all = TRUE) %>% 
                     select(LEA.Code, LEA.Name),
                   by="LEA.Name",
                   all.x=TRUE)

# Note the stand alone schools ....
table_pcs %>% 
  filter(is.na(LEA.Code)) %>% 
  select(LEA.Name)

# Correct Start Date

table_pcs <- table_pcs %>% 
  mutate(Start.Date = ifelse(LEA.Name == "Bridges PCS", "August 31",
                      ifelse(LEA.Name == "Community College Prep Academy PCS", "August 10",
                      ifelse(LEA.Name == "Friendship PCS", "August 31",
                      ifelse(LEA.Name == "KIPP DC", "August 24",
                      ifelse(LEA.Name == "Shining Stars Montessori Academy PCS", "August 31",
                      ifelse(LEA.Name == "Washington Global PCS", "August 10", as.character(Start.Date))))))),
         Start.Date = mdy(paste0(Start.Date, ", 2020"))) # Note that 10th is for 6th grade, all others Aug 31st

table_pcs %>% 
  distinct(Start.Date, .keep_all = TRUE) %>% 
  select(LEA.Name, Start.Date) %>% 
  View()

table(table_pcs$Start.Date)

# Build First Check Date

# * * DME Location File
table_dme_short <- table_dme %>% 
  select(LEA_Code_1,
         School_Code_1,
         MAR_Latitude,
         MAR_Longitude,
         School_Name_1,
         Ward)


# Test Map ----------------------------------------


# Write Up ----------------------------------------------------------------

# I read through the PCS current online reopening plans and wanted to share high level notes:
#   .	Only 27 of 67 (?) SY20-21 LEA's have shared their re-opening plans, of which a few (like KIPP's) are out of date - the PCS form states KIPP is opening in Hybrid.
# .	There are 6 different start dates, ranging from Aug 10th (prioritizied students) to September 2nd. Most are starting on August 31st.
# .	Most PCSs are also going to reassess at the end of their first term, for which there are 9 different ending dates. Both LAYC and Howard U. break this mold by remaining in their choice through the first semester.
# .	The majority of PCSs currently have similarly announced an all virtual start (21 schools), however the following six are giving families a choice between Hybrid and Virtual:
#   o	Capitol Village, DC Bilingual, DC Prep, I Dream PCS, Perry Street, and Shining Stars Montessori.
# .	Lastly, so far five PCSs that are starting virtually are also offering either some form of learning hub or a similar in-person prioritization:
#   o	DC International - Priority Students
# o	DC Scholars PCS - Students with IEPS (Maybe)
# o	Friendship PCS - Expressed Needs
# o	Howard University MS - Highest Need
# o	Sojourner Truth PCS - Highest Need
# 
# Reading through the PCS' online ReOpening status, I'd first note that its partially out of date. KIPP is listed in the document at Hybrid, yet on their website they state they'll begin fully virtual, suggesting all plans are likely still in flux.
# As of this afternoon, 27 of 66 PCS LEA's had shared their plan. Of those 21 will begin in virtual, mostly through the first term, while 6 will begin with an in-person hybrid 

