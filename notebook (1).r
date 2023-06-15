
# Load the packages
library(tidyverse)
library(lubridate)

# Read in the crime data
crime_raw <- read_csv("datasets/Violent_Crime_by_County_1975_to_2016.csv")

# Select and mutate columns the needed columns
crime_use <- crime_raw %>% 
    select(JURISDICTION, YEAR, POPULATION, crime_rate = `VIOLENT CRIME RATE PER 100,000 PEOPLE`) %>%
    mutate(YEAR_2 = year(mdy_hms(YEAR)))

# Peek at the data
head(crime_use)

# These packages need to be loaded in the first @tests cell
library(testthat) 
library(IRkernel.testthat)

## Test to see if data loaded and correctly manipulated
crime_raw_soln <- read_csv("datasets/Violent_Crime_by_County_1975_to_2016.csv")

# Select columns JURISDICTION, YEAR, POPULATION, and `VIOLENT CRIME RATE PER 100,000 PEOPLE`. 
# Rename the last to be crime_rate. Make sure the columns are in this order
crime_use_soln <- 
    crime_raw %>% 
    select(JURISDICTION, YEAR, POPULATION, crime_rate = `VIOLENT CRIME RATE PER 100,000 PEOPLE`) %>%
    mutate(YEAR_2 = year(mdy_hms(YEAR)))

run_tests({
    test_that("the correct packages are loaded", {
        expect_true("tidyverse" %in% .packages(), info = "Did you load the tidyverse package?")
        expect_true("lubridate" %in% .packages(), info = "Did you load the lubridate package?")
    })
        
    test_that("the dataset is loaded correctly and correct columns selected and mutated", {
        expect_equal(crime_raw, crime_raw_soln,
            info = "crime_raw contains the wrong values. Did you import the CSV correctly?")
        expect_identical(colnames(crime_use), colnames(crime_use_soln),
            info = "crime_use has the wrong column names. Did you select its columns correct?")    
        expect_equal(crime_use$YEAR_2, crime_use_soln$YEAR_2,
            info = "YEAR_2 contains the wrong values. Did you mutate YEAR correctly with the lubridate functions?")
    })
})

# Plot the data as lines and linear trend lines
ggplot(crime_use, aes(x = YEAR_2, y = crime_rate, group = JURISDICTION)) + 
    geom_line() + 
    geom_smooth(method = "lm", se = FALSE, size = 0.5)

# create student plot
stud_plot <- last_plot()

# create solution plot
soln_plot <- 
    ggplot(crime_use_soln, aes(x = YEAR_2, y = crime_rate,
                          group = JURISDICTION)) + 
    geom_line() +
    stat_smooth(method = 'lm', se = FALSE, size = 0.5)

run_tests({
    test_that("plot is drawn correctly", {
        expect_s3_class(stud_plot, "ggplot") 
        expect_identical(
            stud_plot$data,
            soln_plot$data,
            info = 'The plot data is incorrect. Did you use `crime_use`?'
        )      
        expect_identical(
            deparse(stud_plot$mapping$x),
            deparse(soln_plot$mapping$x),
            info = 'The `x` aesthetic is incorrect. Did you map it to `YEAR_2`?'
        )      
        expect_identical(
            deparse(stud_plot$mapping$y),
            deparse(soln_plot$mapping$y),
            info = 'The `y` aesthetic is incorrect. Did you map it to `crime_rate`?'
        )      
        expect_identical(
            deparse(stud_plot$mapping$group),
            deparse(soln_plot$mapping$group),
            info = 'The `group` aesthetic is incorrect. Did you map it to `JURISDICTION`?'
        )      
        expect_identical(
            class(stud_plot$layers[[1]]$geom)[1],
            class(soln_plot$layers[[1]]$geom)[1],
            info = 'There is no line layer. Did you call `geom_line()`?'
        )     
        expect_identical(
            class(stud_plot$layers[[2]]$geom)[1],
            class(soln_plot$layers[[2]]$geom)[1],
            info = 'The `stat_smooth()` is incorrect . Did you call `stat_smooth()`?'
        )     
        expect_identical(
            stud_plot$layers[[2]]$geom_params$se,
            soln_plot$layers[[2]]$geom_params$se,
            info = 'The `stat_smooth()` entries are incorrect. Did you set `se` to `FALSE`?'
        )    
        expect_identical(
            stud_plot$layers[[2]]$stat_params$method,
            soln_plot$layers[[2]]$stat_params$method,
            info = 'The `stat_smooth()` entries are incorrect. Did you set `method` to `"lm"`?'
        )
        expect_identical(
            stud_plot$layers[[2]]$aes_params$size,
            soln_plot$layers[[2]]$aes_params$size,
            info = 'The `stat_smooth()` entries are incorrect. Did you correct set `size`?'
        )
    })
})

# Mutate data to create another year column, YEAR_3
crime_use <-
  crime_use %>%
  mutate(YEAR_3 = YEAR_2 - min(YEAR_2))

crime_use_soln <- 
    crime_use_soln %>%
    mutate(YEAR_3 = YEAR_2 - min(YEAR_2))

run_tests({
    test_that("manipulated data correctly", {
        expect_equal(
            crime_use,
            crime_use_soln,
            info = 'Did you correctly `mutate()` the data, `crime_use`?\nIf you previously entered a wrong value,\nyou may need to run the previous cell\nto recreate `crime_use`. '
        )
    }) 
})

# load the lmerTest package
library(lmerTest)

# Build a lmer and save it as lmer_crime
lmer_crime <- lmer(crime_rate ~ YEAR_3 + (YEAR_3|JURISDICTION), data = crime_use)

# Print the model output
lmer_crime

lmer_crime_soln <- lmer(crime_rate ~ YEAR_3 + (YEAR_3|JURISDICTION), crime_use_soln)

run_tests({
        test_that("lmerTest is loaded", {
            expect_true("lmerTest" %in% .packages(), info = "Did you load the lmerTest package?")
        })
        test_that("used correct data.frame", {
            expect_equal(deparse(lmer_crime@call$data), "crime_use",
                info = 'The `lmer()` data is incorrect. Did you use `crime_use`?'
            )      
            expect_identical(lmer_crime_soln@call$formula, lmer_crime@call$formula, 
                info = 'The `lmer()` formual is incorrect. Did you use the correct formula?')
        })
    })

# Examine the model outputs using summary
summary(lmer_crime)

# This is for readability 
noquote("**** Fixed-effects ****")

# Use fixef() to view fixed-effects
fixef(lmer_crime)

# This is for readability 
noquote("**** Random-effects ****")

# Use ranef() to view random-effects
ranef(lmer_crime)

list_of_objects  <-  ls()

run_tests({
    test_that("lmer_crime was inspected", {
    expect_true("lmer_crime" %in% list_of_objects, 
        info = "lmer_crime doesn't seem to be in the environment. Make sure your created it with lmer().")
    })
    # You can have more than one test
})

# Add the fixed-effect to the random-effect and save as county_slopes
county_slopes <- fixef(lmer_crime)["YEAR_3"] + ranef(lmer_crime)$JURISDICTION["YEAR_3"]

# Add a new column with county names
county_slopes <-
    county_slopes %>% 
  rownames_to_column(var = "county")

county_slopes_soln <- fixef(lmer_crime_soln)["YEAR_3"] + ranef(lmer_crime_soln)$JURISDICTION[ "YEAR_3"] 

# Add a new column with county names
county_slopes_soln <-
    county_slopes_soln %>% 
    rownames_to_column("county")

run_tests({
    test_that("Is the `county_slopes` dataframe correct?", {
    expect_identical(county_slopes, county_slopes_soln, 
        info = "`county_slopes` was not created correctly. Did you use the correct slope, `YEAR_3?")
    })
    
})

# Load usmap package
library(usmap)

# load and filter map data
county_map <- us_map(regions = "counties", include = "MD")

county_map_soln <- us_map(regions = 'counties', include = "MD")

run_tests({
    test_that("usmap is loaded", {
        expect_true("usmap" %in% .packages(), info = "Did you load the usmap package?")
    })
    
    test_that("Is the `county_map_soln` correct?", {
    expect_identical(county_map, county_map_soln, 
        info = "`county_map` was not created correctly. Did you use `us_map()` correct?")
    })
    
})

# See which counties are not in both datasets
county_slopes %>% anti_join(county_map, by = "county")
county_map %>% anti_join(county_slopes, by = "county")

# Rename crime_names county
county_slopes  <- county_slopes  %>% 
  mutate(county = ifelse(county == "Baltimore City", "Baltimore city", county))

# rename crime data
county_slopes_soln <-
  county_slopes_soln %>% 
  mutate(county = ifelse(county =="Baltimore City", "Baltimore city", county))

run_tests({
    test_that("Is `crime_names` data.frame correct?", {
    expect_identical(county_slopes_soln, county_slopes, 
        info = "`county_slopes` was not created correctly.\nDid you use correctly change the wrong county?")
    })
})

# Merge the map and slope data frames
both_data <- 
  county_map %>% 
full_join(county_slopes, by = "county")

# Peek at the data
head(both_data)

both_data_soln <- 
  county_map_soln %>% 
  full_join(county_slopes_soln, by = "county") 

run_tests({
    test_that("Is `both_data` data.frame correct?", {
    expect_identical(both_data_soln, both_data, 
        info = "`both_data` was not created correctly.\nDid you correctly merge `county_map` and `county_slopes`?")
    })
})

# Set the notebook's plot settings
options(repr.plot.width=10, repr.plot.height=5)

# Plot the results 
crime_map <- 
   ggplot(both_data, aes(long, lat, group = county, fill = YEAR_3)) +
  geom_polygon() + 
  scale_fill_continuous(name = expression(atop("Change in crime rate","(Number year"^-1*")")),
                        low = "skyblue", high = "gold")

# Look at the map
crime_map

# create solution plot
crime_map_soln <- 
    ggplot(data = both_data_soln, aes(x = long, y = lat, 
                                  group = county,
                                  fill= YEAR_3)) +
    geom_polygon() + 
    scale_fill_continuous(name = expression(atop("Change in crime rate","(Number year"^-1*")")),
                          low = 'skyblue', high = 'gold')


run_tests({
    test_that("plot is drawn correctly", {
        expect_s3_class(crime_map, "ggplot") 
        expect_identical(
            crime_map$data,
            crime_map_soln$data,
            info = 'The plot data is incorrect. Did you use `both_data`?'
        )      
        expect_identical(
            deparse(crime_map$mapping$x),
            deparse(crime_map_soln$mapping$x),
            info = 'The `x` aesthetic is incorrect. Did you map it to `long`?'
        )      
        expect_identical(
            deparse(crime_map$mapping$y),
            deparse(crime_map_soln$mapping$y),
            info = 'The `y` aesthetic is incorrect. Did you map it to `lat`?'
        )      
        expect_identical(
            deparse(crime_map$mapping$group),
            deparse(crime_map_soln$mapping$group),
            info = 'The `group` aesthetic is incorrect. Did you map it to `county`?'
        )
        expect_identical(
            deparse(crime_map$mapping$fill),
            deparse(crime_map_soln$mapping$fill),
            info = 'The `fill` aesthetic is incorrect. Did you map it to `YEAR_3`?'
        )     
        expect_identical(
            class(crime_map$layers[[1]]$geom)[1],
            class(crime_map_soln$layers[[1]]$geom)[1],
            info = 'There is no polygon layer. Did you call `geom_polygon()`?'
        )         
        expect_identical(
            deparse(crime_map$scales$scales[[1]]$name),
            deparse(crime_map_soln$scales$scales[[1]]$name),
            info = 'The fill legend title is incorrect. Did you sue the correct argument for  `name = expression(...)`?'
        )         
        expect_identical(
            layer_data(crime_map),
            layer_data(crime_map_soln),
            info = "The `scale_fill_continuous()` data is incorrect. Did you set `low = 'skyblue'` and `high = 'gold'`?"
        )         
    })
})

# Plot options
options(repr.plot.width=10, repr.plot.height=5)

# Polish figure
crime_map_final <- crime_map + 
  theme_minimal() +
  xlab("") +
  ylab("") +
  theme(axis.line = element_blank(),
       axis.text = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()
       )

# Look at the map
print(crime_map_final)

# create solution plot
crime_map_final_soln <- 
    crime_map_soln + 
    theme_minimal() +
    ylab("") +
    xlab("") +
    theme(axis.line=element_blank(),axis.text=element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank()) 

run_tests({
    test_that("plot is drawn correctly", {
        expect_s3_class(crime_map, "ggplot") 
        expect_identical(
            crime_map_final$labels$x,
            crime_map_final_soln$labels$x,
            info = 'The x-label is incorrect. Did you use `xlab("")`?'
        )
        expect_identical(
            crime_map_final$labels$y,
            crime_map_final_soln$labels$y,
            info = 'The y-label is incorrect. Did you use `ylab("")`?'
        )         
        expect_identical(
            crime_map_final$theme,
            crime_map_final_soln$theme,
            info = 'The theme is incorrect. Did you use correctly change the `theme()` settings and use `theme_minimal()`?'
        )         

    })
})


# Build a lmer with both year and population
lmer_pop <- lmer(crime_rate ~ YEAR_3 + POPULATION + (YEAR_3|JURISDICTION), data = crime_use)

# Inspect the results
summary(lmer_pop)
ranef(lmer_pop)

lmer_pop_soln <- lmer(crime_rate ~ YEAR_3 + POPULATION + (YEAR_3|JURISDICTION), crime_use)

# extract out data.frame from both
#lmer_pop_frame <- lmer_crime@'frame'
#lmer_pop_frame_soln <- lmer_crime_soln@'frame'

# Remove attributes 
#attr(lmer_pop_frame, "terms") <- NULL
#attr(lmer_pop_frame, "formula") <- NULL

#attr(lmer_pop_frame_soln, "terms") <- NULL
#attr(lmer_pop_frame_soln, "formula") <- NULL

run_tests({
    test_that("used correct model inputs", {
        expect_identical(deparse(lmer_pop@call$data), "crime_use", 
            info = 'The `lmer()` data is incorrect. Did you use `crime_use`?' )      
        expect_identical(lmer_pop@call$formula, lmer_pop_soln@call$formula,
            info = 'The `lmer()` formual is incorrect. Did you use the correct formula?' )
    })
})
