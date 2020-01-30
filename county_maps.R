library(tidyverse)
library(acs)

full_df = readRDS("complete_Yelp_dataset.RDS")

df = full_df %>%
    filter(!is.na(County.1)) %>%
    select(search_term,
           Zip.Code,
           County.1,
           Average.Rating,
           Price.Level
           )

df$State = gsub("(.*), (.*)", "\\2", df$County.1)
df$County.Name = gsub("(.*), (.*)", "\\1", df$County.1)

df_merged = merge(df, fips.county, by = c("State", "County.Name"))

library(stringr)
df_merged$fips = paste0(str_pad(df_merged$State.ANSI, 2, pad = "0"),
                        str_pad(df_merged$County.ANSI, 3, pad = "0"))

saveRDS(df_merged, "map_data.RDS")

df_merged = readRDS( "map_data.RDS")

map_df = df_merged %>%
    mutate(search_term = as.character(search_term)) %>%
    filter(!is.na(fips)) %>%
    group_by(search_term, fips) %>%
    summarise(value = mean(Average.Rating))  %>%
    rename(region = fips) %>%
    mutate(region = as.character(region)) %>%
    filter(region != "",
           !is.na(region),
           !is.na(value)) %>%
    mutate(region = as.numeric(region)) 

map_df2 = df_merged %>%
    mutate(search_term = as.character(search_term)) %>%
    filter(!is.na(fips)) %>%
    group_by(fips) %>%
    summarise(value = mean(Average.Rating))  %>%
    rename(region = fips) %>%
    mutate(region = as.character(region)) %>%
    filter(region != "",
           !is.na(region),
           !is.na(value)) %>%
    mutate(region = as.numeric(region)) 

library(choroplethr)


figures = list()
i = 1
for(occupation in unique(df$search_term)){
    temp = county_choropleth(filter(map_df,
                                       search_term == occupation)) +
        scale_fill_brewer(
            na.value="grey") +
        ggtitle(paste0("Average Yelp Ratings: ", tools::toTitleCase(occupation)))
    temp$layers[[1]]$aes_params$size = 0.01
    figures[[i]] = temp
    i = i+1
}

fig1 = county_choropleth(filter(map_df2)) +
    scale_fill_brewer(
        na.value="grey") +
    ggtitle("Average Yelp Ratings: All Occupations")
fig1$layers[[1]]$aes_params$size = 0.01

library(gridExtra)

grid.arrange(fig1,
             figures[[1]],
             figures[[2]],
             figures[[3]],
             figures[[4]],
             figures[[5]],
             figures[[6]],
             figures[[7]],
             figures[[8]],
             ncol = 3)



    # scale_fill_continuous(low="green", 
    #                       high="blue", 
    #                       # middle = "white",
    #                       guide="colorbar",
    #                       na.value="grey")




# install.packages("devtools")
# library(devtools)
# install_github('arilamstein/choroplethrZip')

# 
# map_df = df %>%
#     group_by(search_term, Zip.Code) %>%
#     summarise(value = mean(Average.Rating)) %>%
#     mutate(search_term = as.character(search_term)) %>%
#     rename(region = Zip.Code) %>%
#     
#     library("choroplethrZip")
# 
# zip_choropleth(map_df %>%
#                    filter(region != "",
#                           search_term == "cosmetologist")
# )
# 
