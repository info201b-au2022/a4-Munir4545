library(tidyverse)

# The functions might be useful for A4
source("/Users/munir45/Documents/info201/assignments/a4-Munir4545/source/a4-helpers.R")

## Test queries ----
#----------------------------------------------------------------------------#
# Simple queries for basic testing
#----------------------------------------------------------------------------#
# Return a simple string
test_query1 <- function() {
  return ("Hello world")
}

# Return a vector of numbers
test_query2 <- function(num=6) {
  v <- seq(1:num)
  return(v)
}

## Section 2  ---- 
#----------------------------------------------------------------------------#
# Your functions and variables might go here ... 
## total incarcerations in the US
total_incarcerations <- sum(incarceration_df$total_jail_pop, na.rm = TRUE)
black_incarcerations <- sum(incarceration_df$black_jail_pop, na.rm = TRUE)
## incarceration proportion for black people
black_prop <- black_incarcerations/total_incarcerations * 100
black_prop <- prettyNum(black_prop)
total_incarcerations <- prettyNum(total_incarcerations, big.mark = ",")
black_incarcerations <- prettyNum(black_incarcerations, big.mark = ",")
## black population percentage
black_population <- prettyNum(sum(incarceration_df$black_pop_15to64, na.rm = TRUE)/
                                sum(incarceration_df$total_pop, na.rm = TRUE) * 100)
#----------------------------------------------------------------------------#

## Section 3  ---- 
#----------------------------------------------------------------------------#

#----------------------------------------------------------------------------#
# This function ... 
# create a function that get_year_jail_pop that creates a dataframe suitable for 
# the visualization
get_year_jail_pop <- function(){
  df <- incarceration_df %>% 
    group_by(year) %>%
    summarise(year, total_jail_pop)
  return(df)
  
}

# This function ... 
# create a function plot_jail_pop_for_us that creates a chart from the data frame
plot_jail_pop_for_us <- function(){
  bargraph <- ggplot(get_year_jail_pop(), aes(year, total_jail_pop)) +
    geom_bar(stat = "identity", width = 0.85, fill = "black") + 
    labs(title = "Increase of Jail Population in U.S. (1970-2018)", x = "Year", y = "Total Jail Population") +
    scale_y_continuous(labels = scales::comma)
  return(bargraph)
}

## Section 4  ---- 
#----------------------------------------------------------------------------#
# Growth of Prison Population by State 
# Your functions might go here ... 
# create a function that returns a data frame of the state, with the parameter states
get_jail_pop_by_states <- function(states){
  df <- incarceration_df  %>% group_by(year, state) %>%
    filter(state %in% states)%>%
    summarise(total_jail_pop = sum(total_jail_pop, na.rm = TRUE) )

    
  return(df)
}
plot_jail_pop_by_states <- function(states){
  lineplot <- ggplot(get_jail_pop_by_states(states), aes(x = year, y = total_jail_pop, group = state)) +
    labs(title = "Jail Population by States (1970-2018)", x = "Year", y = "Total Jail Population") +
    geom_line(aes(color=state)) +
    geom_point(aes(color=state))
  return(lineplot)
}
# See Canvas
#----------------------------------------------------------------------------#

## Section 5  ---- 
#----------------------------------------------------------------------------#
# <variable comparison that reveals potential patterns of inequality>
# Your functions might go here ... 
# create a function that creates a data frame with jail population by urbanicity
get_jail_pop_by_urban <- function(){
  df <- incarceration_df %>% 
    group_by(year, urbanicity) %>%
    summarise(total_jail_pop = sum(total_jail_pop, na.rm = TRUE))
  df  <- df[!grepl(0.00, df$total_jail_pop),]
  return(df)
}

plot_jail_pop_urban <- function(){
 gg <- ggplot(get_jail_pop_by_urban(), aes(x = year, y = total_jail_pop, group = urbanicity)) +
   geom_line(aes(color=urbanicity)) +
   geom_point(aes(color=urbanicity)) +
   labs(title = "Jail population Based Off Urbanicity", x = "Year", y = "Jail Population")
 return(gg)
}
# See Canvas
#----------------------------------------------------------------------------#

## Section 6  ---- 
#----------------------------------------------------------------------------#
# <a map shows potential patterns of inequality that vary geographically>
# Your functions might go here ... 
# create a function that gets the location and the total black jail population
get_black_jail_pop <- function(){
    df <- incarceration_df %>% 
      mutate(county_name = tolower(word(county_name,1))) %>%
      summarise(county_name, total_jail_pop, black_jail_pop)
    county <- map_data("county") %>% 
      rename(county_name = subregion) %>%
      left_join(df, by="county_name" )%>%
      distinct(group,county_name,region, total_jail_pop, black_jail_pop, .keep_all = TRUE )
      
    return(county)
}

map_black_jail_pop <- function(){
  p <- ggplot(get_black_jail_pop(), aes(long, lat, group = group))+
    geom_polygon(aes(fill = black_jail_pop), color = "black")
  return(p)
}
# See Canvas
#----------------------------------------------------------------------------#

## Load data frame ---- 


