
# NYC schools - chronic absenteeism map #

# Libraries.
library(readr)
library(dplyr)
library(stringr)
library(ggplot2)


#- Download web data.

# Download attendance data using the link with extensions: https://data.cityofnewyork.us/resource/vww9-qguh
# Note: ?$limit=... needs to be added for large files
df0 <- read_csv("https://data.cityofnewyork.us/resource/vww9-qguh.csv?$limit=900000", na = c("s")) %>% #recoding missing values
  as.data.frame() %>% 
  # keep complete cases only.
  filter(complete.cases(.)) %>% 
  # detect letters in DBN and create new variables for Borough names and district codes.
  mutate(borough = str_extract(dbn, "[[:alpha:]]")) %>% 
  # recode by district full name
  mutate(borough = recode(borough, "M"="Manhattan", "X"="Bronx", "K"="Brooklyn", "Q"="Queens", "R"="Staten Island")) %>% 
  # remove trailing zeros
  mutate(district = str_remove(str_extract(dbn, "[^a-zA-Z]+"), "^0+")) %>% 
  # remove special education schools.
  filter(district != 75) %>% 
  # keep year 2018-19 only.
  filter(year == "2018-19")
# QC: 
# df0 %>% count(borough)
# df0 %>% count(borough, district)

# For future reference: Load json file for attendance data using the API.
# resp_json <- httr::GET("https://data.cityofnewyork.us/resource/vww9-qguh.json?$limit=900000") 
# jsontext <- httr::content(resp_json, as = "text") #default is "parsed".
# json_data <- jsonlite::fromJSON(content(resp_json, as = "text")) #parsing the text data.


# Download spatial NYC school district data using the link with extensions: https://data.cityofnewyork.us/resource/cuae-wd7h
nyc_districts <- geojsonio::geojson_read("https://data.cityofnewyork.us/resource/cuae-wd7h.geojson", what="sp")
# data frame for coordinates.
nyc_districts_map <- nyc_districts %>% fortify(region="school_dist")
# mid points of the districts to append ids on the map later.
mids <- rgeos::gCentroid(nyc_districts, byid=TRUE) %>% as.data.frame() %>% 
  mutate(id = nyc_districts$school_dist) %>% 
  arrange(id) %>% 
  #for repeating district ids, take the first occurrence
  group_by(id) %>% 
  slice(1) %>%
  ungroup() %>% 
  #district 2, 4, and 27 are not exactly in the middle, so adjust the value
  #first, clean the coordinate values by remove trailing spaces
  mutate(across(c(x,y), str_trim)) %>% 
  mutate(across(c(x,y), as.numeric)) %>% 
  mutate(x=ifelse(id=="2", -73.98273, x)) %>% 
  mutate(y=ifelse(id=="2", 40.74875, y)) %>% 
  mutate(x=ifelse(id=="4", -73.94200, x)) %>% 
  mutate(y=ifelse(id=="27", 40.66850, y)) 
  
# check the empty map
ggplot() + 
  geom_map(data=nyc_districts_map, map=nyc_districts_map,
                    aes(x=long, y=lat, map_id=id),
                    color="#2b2b2b", size=0.15, fill=NA) +
  geom_text(data=mids, aes(x=x, y=y, label=id), size=2) +
  coord_map() + 
  ggthemes::theme_map()



#- Map chronic absenteeism for all students in all grades.

# Process and filter data.
df1 <- df0 %>% 
  filter(is.na(chronically_absent_1)==F) %>% 
  filter(grade=="All Grades") %>% 
  filter(demographic_variable=="All Students") %>% 
  select(district, chronically_absent_1) %>% 
  group_by(district) %>% 
  summarise(value = median(chronically_absent_1)) %>% 
  ungroup() %>% as.data.frame()

# Plot.
ggplot() + 
  # empty map of nyc school districts
  geom_map(data=nyc_districts_map, map=nyc_districts_map, 
           aes(x=long, y=lat, map_id=id), color="#2b2b2b", size=0.15, fill=NA) + 
  # map absenteeism data 
  geom_map(data=df1, map=nyc_districts_map, 
           aes(fill=value, map_id=district), color="#2b2b2b", size=0.15) + 
  # append district ids
  geom_text(data=mids, aes(x=x, y=y, label=id), size=3.5) + 
  # refine the theme
  coord_map() +
  ggthemes::theme_map() + 
  theme(legend.position=c(0.01,0.5), legend.key.size = unit(10, "mm"),
        legend.text = element_text(size = 10), legend.title = element_text(size = 12)) +
  scale_fill_gradient(low = "lightgreen", high = "dodgerblue4", limits=c(0, max(df1$value)), name = "Median percentage")







