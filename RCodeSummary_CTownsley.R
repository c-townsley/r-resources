#R Code Summary
#Charlie Townsley

#GENERAL ####
#Michael Fitchman's "Intro to R and FAQ for Planners" - https://mafichman.github.io/R_FAQ_For_Planners/ 

#SETUP ####

rm(list=ls())
#install.packages("tidycensus") 
install.packages("tidycensus")
library(tidycensus)
#install.packages("tidyverse") 
library(tidyverse)
#install.packages("dplyr")
install.packages("dplyr")
library(dplyr)
#learn more about a function
?

#IMPORT/EXPORT DATA ####
setwd("c:/Documents/my/working/directory") #Set working directory. Either need forward slash (/) or double backslash (\\)
dataframe <- read_csv("filepath/filename") #Import a CSV
write.csv(filename, file = "filepath/filename", row.names = FALSE) #Export a CSV with Base R
write_csv(dataframe, "filepath/filename") #Export a CSV with dplyr

#To compare two files
library(diffr)
diffr(filename1, filename2)
diffr(CPLN505_Assignment2_CT, CPLN505_Assignment2_Master)


#DATA MANAGEMENT BASICS ####

#Looking at Data
head(dat) # shows the first five rows
head(dat, 10) #Look at the first ten lines of the data
str(dat) #It's a data frame composed of factors, integers, and numbers
dim(dat) #Find the dimensions (rows, columns)
dim(dat)[1] #Find the number of rows
dim(dat)[2] #Find the number of columns
class() # data type
names() # column names
colnames(dat) #See column names
nrow()  # number of rows
ncol()  # number of columns
str()   # shows 1st 10 values of each column
glimpse() # summarizes dataframe

dataframename$columnname # call a column using the "$" operator
typeof(dataframename$columnname) # What type of data is a column?
summary(dat) #gives breakdown of each field, including NA, factors, etc.
summary(dataframe$columnname) # summarize a dataframe's column
summary(dataframe$columnname) # summarize column's values (min, 1st quantile, median, mean 3rd quantile & max)
max(dataframe$columnname) # column's max value
median(dataframe$columnname) # column's median value
quantile(dataframe$columnname) # column's quantile values
median.Vacant <- median((dataframe$columnname)) # store calculated value
operation(na.omit(dataframe$columnname)) # perform operation while omitting NA values


#DATAFRAME COMMON OPERATIONS ####

#Reclassify Variables
dataframe <- dataframe %>% 
  mutate(TextHold = case_when(CLASS == "X" ~ "Label 1",
                              CLASS == "Y" ~ "Label 2",
                              CLASS == "Z" ~ "Label 3"))

col_types = cols("ColumnName" = col_number())) #Reclassify a column type to numbers

#can string multiple commands together using the pipe (%>%) annotation

daframe$newColumnName <- ("value1", "value2", ...) # create new column using the "$" & "<-" operators

datframe$newColumnName <- "column1" - "column2" # create new column using an arithmetic expression to manipulate data

unique(dataframe) #Eliminate duplicate rows in a dataframe

unite("New Column Name", Column1:ColumnN, sep = " ") #Merge or concatenate columns into one new column and a space as a separator btwn each value when merged.

#Subset data by selecting columns using dplyr [package]
newdataframe <- largerdataframe %>%
  dplyr::select(colname1, colname2, ...)

# stringing multiple commands together using the pipe (%>%) annotation
# i.e. 
newdataframe <- largerdataframe %>%
                dplyr::select(colname1, colname2,) %>%
                mutate(colname = "colvalue1, clvalue2, ...")

dataframe <- dataframe %>%
  mutate(newcolumnname = ifelse(existingcolname > 0, TRUE, FALSE)) #Use "ifelse" statement to sort data

#to create a new dataframe with only certain columns from an existing dataframe 
newdataframe <- existingdataframe %>%
  select(column1, column2, ...)

#to rename column
dataframename <- dataframename %>%
  rename(columnnewname = columnoldname)

dataframe1 <- dataframe1 %>%
  rename(newcolumn1name = oldcolumn1name,
         ...)

# to check if column names are the same, either use a boolean expression OR
# 'setdiff' command to see if there are character differences
# names(dataframe1) == names(datafram2)
# setdiff(names(dataframe1), names(dataframe2))

# to summarize data use 'group' command
  summarydataframe <- originaldataframe %>%
  group_by(year, MUNI_NAME) %>%
    summarize(countPatch = sum(gridcode),                      #count of unique patches
              mean_Compactness = mean(iso),                    #mean 'roundness' of patch 
              mean_Area = mean(area),                          #mean area of patch 
              sum_Area = sum(area),                            #sum area of patch
              sum_Perimter = sum(perimeter)) %>%
    filter(MUNI_NAME != " ") %>%
    as.data.frame()

#to check if dataframes have the same number of rows in each column:
length(dataframe1) == length(dataframe2)


#FILTER DATA ####

#Using summary
newsummarrydataframe  <- filteredsummarydatafram %>% 
  filter(columnvalue == "value")

#Using dplyr
dataframe <- existingdataframe %>% 
  filter(clmnname == "Column Value") %>% #Filter by value
  filter(between (clmnname, 1, 4 )) %>% #Filter by a range of values
  filter(grepl("text string", clmnname, ignore.case = TRUE)) #Filter rows that contain certain text


#JOINS ####

# to combine two data frames into one new one, if the two tables have the same column names use:
dataframe3 <- rbind(dataframe1, dataframe2)

#left join (dataframe1 stays on the left & the dataframe is attached to the end/right)
new_dataframe <- left_join(dataframe1, dataframe2, by = c("uniquecolumnname"))


#R MARKDOWN ####

#Resources#
#Cheat sheet - https://www.rstudio.com/wp-content/uploads/2015/02/rmarkdown-cheatsheet.pdf 
#Formatting - https://r4ds.had.co.nz/r-markdown-formats.html 
#Formatting - https://bookdown.org/yihui/rmarkdown/html-document.html#code-folding 
#Detailed description of knitr - https://yihui.org/knitr/ 
#R markdown syntax highlighting examples - https://www.garrickadenbuie.com/blog/pandoc-syntax-highlighting-examples/ 
#Info on changing html output font - https://rstudio4edu.github.io/rstudio4edu-book/doc-fancy.html 

#To specify font of r markdown output to html, can use inline css
<style>
  body, p {
    background-color: lightgray;
    color: black; #font color
    font-family: font name;
  }
</style>


#AESTHETICS ####

#Resources#
#Overview of ggplot themes - https://ggplot2.tidyverse.org/reference/ggtheme.html
#Rayshader package for 3D visualization - https://www.rayshader.com/ 

#create color palette
palettename <- c("#050234","#2C0078","#7F00BF","#F600FF","#FF0DBE","#FF569F","#FF9BA8","#FFE8E4")

#divide plot into two
par(mfrow=c(2,1))

#Add legend
legend("location", c("X", "Y"), col=c("red", "blue"))
#location examples are "bottomright", "topleft", etc.


#CHARTS/PLOTS ####

#resources#
#Great resource on data visualization in r with documentation: https://r-graph-gallery.com/ 

#histogram
hist(datafram$column)

#Density Plot
plot(density(dataframe$column), xlim=c(num1,num2))
#xlim sets bounds of plot

#Scatter plot with full syntax
plot(dataframe$clmn1, dataframe$clmn2,
     main="Plot Title",
     xlab="X Axis Label", ylab="Y Axis Label",
     xlim=c(mins, maxs), ylim=c(mins, maxs), #set mins and maxes of plot
     col="red", pch=10) #pch is point size?

#Add average line
abline(v=mean(dataframe$clmn), col="red")
#Add median line
abline(v=median(dataframe$clmn))
#Add line for y column
abline(h=mean(dataframe$clmn))

# Correlation matrix - http://www.sthda.com/english/wiki/correlation-matrix-a-quick-start-guide-to-analyze-format-and-visualize-a-correlation-matrix-using-r-software 

# using ggplot
# Resource on formatting ggplot2 bar plots:http://www.sthda.com/english/wiki/ggplot2-barplots-quick-start-guide-r-software-and-data-visualization
ggplot()+
  geom_point(data = dataframe, 
             aes(x = column1, y = column2),
             color = "blue") #for scatter plot


# MAPPING ####
# Resource for mapping with ggplot: http://joshuamccrain.com/tutorials/ggplot_maps/maps_tutorial.html 
# Mapping in r resource from Robin Lovelace's "Geocomputation with R": https://geocompr.robinlovelace.net/adv-map.html
# Finding projection systems: https://spatialreference.org/ 

#create map theme
mapTheme <- theme(
  text = element_text( color = "black"),
  plot.title = element_text(size = 14,colour = "black"),
  plot.subtitle=element_text(face="italic"),
  plot.caption=element_text(hjust=0),
  axis.ticks = element_blank(),
  panel.background = element_blank(),axis.title = element_blank(),
  axis.text = element_blank(),
  axis.title.x = element_blank(),
  axis.title.y = element_blank(),
  panel.grid.minor = element_blank(),
  panel.border = element_rect(colour = "black", fill=NA, size=2)
)

#to plot map
ggplot() + 
  geom_sf(data=shapefilename, aes(fill=chosencolumn)) + 
  coord_sf() +
  scale_fill_gradientn(colors=colorpalette, name = "legendtitle") +
  labs(title="maptitle") +
  mapTheme


#WORK WITH CENSUS DATA ####

#Install the API key
census_api_key("API_Key", overwrite = TRUE)
census_api_key("API_Key", install = TRUE)

#Download Dataset
#Load variable table
dd19_5 <- load_variables(year = 2019,dataset = "acs5")
#^table name ^function    ^year of variables    ^can also pull census from API
write.csv(filename, file = "filepath w file name at the end", row.names = FALSE) 
#^can download table as excel for side by side reference

#Download Data
#For ACS
newtablename <- get_acs(geography = "place",
                    state = "MA", 
                    variables = c("B19013_001","B25031_001"), ##find varible from table above
                    year = 2019,
                    survey = "acs5")
#For Decennial Census
newtablename <- get_decennial(geography = "tract",
                              state = 42,
                              county= 101,
                              variables = vars_race_2010sf1,
                              year = 2010,
                              survey = "sf1", geometry=FALSE)

#geography can be "place", "tract", "county", "state"
# need FIPS code
#state: https://www.census.gov/library/reference/code-lists/ansi.html#par_statelist_1 
#other geographies: https://www.census.gov/library/reference/code-lists/ansi.html

#To subset data
MH<-subset(dat_19_5_tract,dat_19_5_tract$GEOID==25025080900|dat_19_5_tract$GEOID==25025081100|dat_19_5_tract$GEOID==25025080801)

