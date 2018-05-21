-----
title: "Homework 1"
output: html_document

#run tidyverse
```{r}
library(tidyverse)
```

First need to upload the | seperated text file, and turn into a tibble
#import
```{r include= FALSE}
gaz_raw<- read.delim("hw1.txt", header = TRUE, sep = "|", quote = "\"",
           dec = ".", fill = TRUE, comment.char = "", na.strings = "NA")
as_tibble(gaz_raw)
```

solect the columns i want and rename to match assignment
#column select and rename 
```{r include=FALSE}
colnames(gaz_raw)
newgaz<-select(gaz_raw, FEATURE_ID: STATE_ALPHA, COUNTY_NAME, ends_with("DEC"), ends_with("M"), starts_with("map"), starts_with("DATE"))

colnames(newgaz)<- c("feature ID", "feature name", "feature class", "state alpha", "county name", "primary latitude(decimal)", "primary longitude(decimal)", "source latitutde(decimal)", "source longitude(decimal)", "elevation in meters", "map name", "date created", "date endited")

```

re-write the data types
#tranform data types and get data ready for analysis
```{r}
gaz_tibble<-as.tibble(newgaz)
write_csv(gaz_tibble, "gaz_tibble.csv")
gaz_tibble<-read_csv("gaz_tibble.csv",
                      col_types = cols( 
                        "feature ID"= col_double(),
                        "feature name"= col_character(),
                        "feature class"=col_character(),
                        "country name"=col_character(),
                        "primary latitude(decimal)"=col_double(),
                        "primary longitude(decimal)"=col_double(),
                        "source latitutde(decimal)"=col_double(),
                        "source longitude(decimal)"=col_double(),
                        "elevation in meters"=col_double(),
                        "map name"= col_character(),
                        "date created"=col_date(),
                        "date endited"=col_date()
                      )
)
gaz_tibble %>% 
  drop_na("primary latitude(decimal)","primary longitude(decimal)")

gaz_tibble<-gaz_tibble %>% filter(`state alpha`== "CA")

write_delim(gaz_tibble, "gaz_tibble.csv", delim = "|" )
```
--------------------------
ANALYSIS
--------------------------

Q1 What is the most-frequently-occuring feature name?
#Quetion 1
```{r}
gaz_tibble %>% count(`feature name`, sort = TRUE)
```



Q2What is the least-frequently-occuring feature class?
#Question 2
```{r }
tail(gaz_tibble %>% count(`feature class`, sort=TRUE))
```



Q3What is the approximate center point of each county?

#Question 3; select data and get ride of 0s
```{include= FALSE}
coordgaz<- select(gaz_tibble, "county name", "feature class" , "primary latitude(decimal)","primary longitude(decimal)") %>% group_by(`county name`) 
#get the variabes i want
coordgaz<-filter(coordgaz, `primary latitude(decimal)` !=0 , `primary longitude(decimal)` !=0)
# get rid of the 0's. aka the unkown
```

# Question 3; add columns for the max and mins of the lat and long. Then add center columns
```{r include=FALSE}
summarizegaz<-summarise(coordgaz,
    maxlat=max(`primary latitude(decimal)`, na.rm=TRUE),
    minlat=min(`primary latitude(decimal)`, na.rm=TRUE), 
    maxlong=max(`primary longitude(decimal)`, na.rm=TRUE),
    minlong=min(`primary longitude(decimal)`, na.rm=TRUE))

summarizegaz<-summarizegaz %>% mutate(centerlat= maxlat+minlat/2, 
                        centerlong=maxlong+minlong/2
                        )
```

#Question 3; find the center and put in dt
```{r}
select(summarizegaz, `county name`, `centerlat`,`centerlong`)
```



Q4What are the fractions of the total number of features in each county that are natural? man-made?
  
#Question 4; create tibble with country name and feature class
```{r include= FALSE}
q4gaz<-tibble("county name"= gaz_tibble$`county name`, "feature class"= gaz_tibble$`feature class`)
```

#Question 4; include man_made and natural columns
```{r include= FALSE}
q4gaz<-q4gaz %>%
  mutate(type= case_when(
    `feature class` == "Airport" ~ "man_made",
    `feature class` == "Bridge" ~ "man_made",
    `feature class` == "Building" ~ "man_made", 
    `feature class` == "Canal" ~ "man_made", 
    `feature class` == "Cemetery" ~ "man_made",
    `feature class` =="Census" ~ "man_made", 
    `feature class` == "Church" ~ "man_made",
    `feature class` == "Civil" ~ "man_made",
    `feature class` == "Crossing" ~ "man_made",
    `feature class` == "Dam" ~ "man_made",
    `feature class` == "Harbor" ~ "man_made",
    `feature class` == "Hospital" ~ "man_made",
    `feature class` == "Locale" ~ "man_made",
    `feature class` == " Military" ~ "man_made",
    `feature class` == "Mine" ~ "man_made",
    `feature class` == "Oilfield" ~ "man_made",
    `feature class` == "Park" ~ "man_made",
    `feature class` == "Pillar"~ "man_made",
    `feature class` == "Populated Place"~ "man_made", 
    `feature class` == "Post Office"~ "man_made",
    `feature class` == "Reserve"~ "man_made", 
    `feature class` == "Reservoir"~ "man_made",
    `feature class` == "School"~ "man_made",
    `feature class` == "Tower"~ "man_made",
    `feature class` == "Trail"~ "man_made",
    `feature class` == "Tunnel"~ "man_made",
    `feature class` == "Well"~ "man_made",
    TRUE ~ "natural"))
```


# Question 4;get the number of man_made and natural for each county
```{r include=FALSE}
q4gaz<-q4gaz %>% group_by(`county name`, `type`) %>% tally() 
q4gaz<-spread(q4gaz, key= `type`, value= `n`)

```


#Question 4; find the fraction of man made and natural features
```{r}
q4gaz<-q4gaz %>% mutate(total= man_made + natural,fraction_man_made= man_made/total,fraction_natural= natural/ total)
```
