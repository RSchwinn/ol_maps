library(tidyverse)
library(maps)

full_df = readRDS("complete_Yelp_dataset.RDS")

df = full_df %>%
    select(search_term,
           Zip.Code,
           County.1,
           Average.Rating,
           Price.Level
           )

state_names = data.frame(state.abb, state.name)

x = "Ada County, ID"

fix_county = function(x){
    county = gsub("(.*), (.*)", "\\1", x)
    county = gsub(" County", "", county) 
    s_abb = gsub("(.*), (.*)", "\\2", x)
    s_abb = substr(s_abb, 1, 2)
    full = filter(state_names, state.abb == s_abb)
    input = paste0(full$state.name, ",",county) %>%
        tolower
    x = filter(county.fips, polyname == input)$fips
}

df$fips = NA

for(i in 1:nrow(df)){
    df$fips[i] = fix_county(df$County.1[i])
}

map_df = df %>%
    mutate(search_term = as.character(search_term)) %>%
    filter(!is.na(fips)) %>%
    group_by(search_term, fips) %>%
    summarise(value = mean(Average.Rating))  %>%
    rename(region = fips)
    
    library("choroplethr")




county_choropleth(map_df %>%
                   filter(region != "",
                          search_term == "hair cut",
                          !is.na(value))
)

county.names

library(maps)

county.fips
map_df$region[2]

x = "Ada County, ID"

fix_county = function(x){
        x = gsub("(.*), (.*)", "\\2, \\1", x)
        x = gsub(" County", "", x) 
        x = tolower(x)
        x
}

fix_county(map_df$region)

 substr()

gsub("(.*)c","\\1","abcd")


# 
# df = inner_join(cleaned_df,
#                 df_1[, c("state",
#                          "cost_of_licensing", "search_term")],
#                 by = c("state", "search_term"))
# 
# it = lm("Average.Rating ~ cost_of_licensing + search_term", df)
# 
# summary(it)

# install.packages("devtools")
# library(devtools)
# install_github('arilamstein/choroplethrZip')


map_df = df %>%
    group_by(search_term, Zip.Code) %>%
    summarise(value = mean(Average.Rating)) %>%
    mutate(search_term = as.character(search_term)) %>%
    rename(region = Zip.Code) %>%
    
    library("choroplethrZip")

zip_choropleth(map_df %>%
                   filter(region != "",
                          search_term == "cosmetologist")
)

