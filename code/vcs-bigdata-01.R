#| label: "08-parallel-01"
#| message: FALSE
library(here)         # manage file paths
library(socviz)       # data and some useful functions
library(tidyverse)    # your friend and mine
library(broom)        # tidy model summaries
library(scales)       # scale fns
library(colorspace)   # palettes and scales
library(gapminder)    # inescapable
library(covdata)      # some data on covid
library(demog)        # us demographic data
library(nycflights13) # 2013 LGA flights


## -----------------------------------------------------------------------------
palmerpenguins::penguins


## -----------------------------------------------------------------------------
gapminder::gapminder


## -----------------------------------------------------------------------------
socviz::gss_lon


## -----------------------------------------------------------------------------
nycflights13::flights


## -----------------------------------------------------------------------------
covdata::stmf


## -----------------------------------------------------------------------------
covdata::apple_mobility


## -----------------------------------------------------------------------------
gapminder |> 
  ggplot(aes(x = gdpPercap, 
             y = lifeExp)) + 
  geom_point() + 
  scale_x_log10()


## -----------------------------------------------------------------------------
gapminder |> 
  ggplot(aes(x = gdpPercap, 
             y = lifeExp)) + 
  geom_point(alpha = 0.3) + 
  scale_x_log10()


## -----------------------------------------------------------------------------
gapminder |> 
  ggplot(aes(x = gdpPercap, 
             y = lifeExp)) + 
  geom_point(shape = 1) + 
  scale_x_log10()


## -----------------------------------------------------------------------------
flights |> 
  ggplot(aes(x = sched_dep_time, 
             y = dep_delay)) + 
  geom_point(shape = 1)


## -----------------------------------------------------------------------------
#| output-location: column
#| fig-width: 5
#| fig-height: 6


flights |> 
  ggplot(aes(x = sched_dep_time, 
             y = dep_delay)) + 
  geom_hex() + 
  scale_fill_binned_sequential(palette = "Reds2") + 
  theme(legend.text = element_text(size = rel(0.5)))


## -----------------------------------------------------------------------------
#| output-location: column
#| fig-width: 5
#| fig-height: 6

diamonds |> 
  ggplot(aes(x = carat, 
             y = price)) + 
  geom_hex() + 
  scale_fill_binned_sequential(palette = "Reds2") + 
  theme(legend.text = element_text(size = rel(0.5)))
  


## -----------------------------------------------------------------------------
#| output-location: column
#| fig-width: 5
#| fig-height: 6

diamonds |> 
  ggplot(aes(x = carat, 
             y = price)) + 
  geom_hex() + 
  scale_fill_binned_sequential(palette = "Plasma") + 
  theme(legend.text = element_text(size = rel(0.5)))


## -----------------------------------------------------------------------------
#| output-location: column
#| fig-width: 5
#| fig-height: 7

diamonds |> 
  ggplot(aes(x = carat, 
             y = price)) + 
  geom_hex() + 
  scale_fill_binned_sequential(palette = "Plasma") + 
  facet_wrap(~ cut, ncol = 2, nrow = 3) +
  theme(legend.text = element_text(size = rel(0.5)))



## -----------------------------------------------------------------------------
diamonds |> 
  ggplot(aes(x = carat, y = price, fill = cut)) + 
  geom_point(shape = 1, alpha = 0.3) 


## -----------------------------------------------------------------------------
#| label: "reveal-boomertile"
#| include: FALSE
okboomer |>
  filter(country == "United States") |>
    mutate(year_fct = 
             factor(year,  
                    levels = unique(year),
                    ordered = TRUE),
           month_fct = factor(month,
                              levels = rev(c(1:12)),
                              labels = rev(c("Jan", "Feb", 
                                  "Mar", "Apr", "May", 
                                  "Jun", "Jul", "Aug",
                                  "Sep", "Oct", "Nov", "Dec")),
                              ordered = TRUE)) |>
    select(year, month, year_fct, month_fct, everything()) |> 
    ggplot(aes(x = year_fct, y = month_fct)) +
    geom_tile(mapping = aes(fill = births_pct_day), 
              color = "white") + 
   scale_x_discrete(breaks = seq(1940, 2010, 5)) +    
   scale_fill_viridis_c(option = "B") + 
  labs(x = NULL, y = NULL, 
       title = "Monthly Birth Rates",
       fill = "Average births per million people per day",
         caption = "Data: US Census Bureau.") + 
  coord_fixed() +
  guides(fill = guide_legend(keywidth = 3, 
                    label.position = "bottom")) + 
  theme(legend.position = "bottom",
        legend.title.position = "right",
        legend.justification = "left") ->
  p_tileboom



## -----------------------------------------------------------------------------
#| label: "09-case-studies-17"
#| echo: FALSE
#| fig.height: 4.8
#| fig.width: 20
p_tileboom


## -----------------------------------------------------------------------------
#| fig.height: 3.5

palmerpenguins::penguins |> 
  ggplot(aes(x = bill_length_mm, fill = species)) + 
  geom_histogram() + facet_wrap(~ species) + guides(fill = "none")


## -----------------------------------------------------------------------------
#| message: false
#| warning: false
#| fig-height: 4

apple_mobility |> 
  mutate(score = squish(score, range = c(0, 500))) |> 
  ggplot(aes(x = score, fill = transportation_type)) + 
  geom_histogram(bins = 60) + facet_wrap(~ transportation_type) + 
  scale_y_continuous(labels = label_number(scale_cut = cut_short_scale())) + 
  guides(fill = "none") 


## -----------------------------------------------------------------------------
flights |> 
  left_join(airlines, by = "carrier") |> 
  mutate(sname = str_squish(str_extract(name, "^.*? "))) |> 
  filter(sname %in% c("United", "JetBlue", "ExpressJet", "Delta", 
                      "Delta", "American", "Envoy", "US", "Endeavor", 
                      "Southwest")) |> 
  ggplot(aes(x = dep_delay, y = sname)) + 
  geom_boxplot() + 
  labs(y = NULL)



## -----------------------------------------------------------------------------
flights |> 
  left_join(airlines, by = "carrier") |> 
  mutate(sname = str_squish(str_extract(name, "^.*? "))) |> 
  filter(sname %in% c("United", "JetBlue", "ExpressJet", "Delta", 
                      "Delta", "American", "Envoy", "US", "Endeavor", 
                      "Southwest")) |> 
  ggplot(aes(x = dep_delay, y = sname)) + 
  geom_violin() + 
  labs(y = NULL)



## -----------------------------------------------------------------------------
flights |> 
  left_join(airlines, by = "carrier") |> 
  mutate(sname = str_squish(str_extract(name, "^.*? "))) |> 
  filter(sname %in% c("United", "JetBlue", "ExpressJet", "Delta", 
                      "Delta", "American", "Envoy", "US", "Endeavor", 
                      "Southwest")) |> 
  mutate(dep_delay_sq = squish(dep_delay, range = c(0, 120))) |> 
  ggplot(aes(x = dep_delay_sq, y = sname)) + 
  ggdist::stat_slab(density = "histogram", fill = "black") + labs(y = NULL)



## -----------------------------------------------------------------------------
diamonds |> 
  ggplot(aes(y = cut, x = price, fill = cut)) + 
  ggdist::stat_slabinterval() +
  scale_fill_discrete_qualitative(palette = "Dark2") + 
  guides(fill = "none") + labs(y = NULL)



## -----------------------------------------------------------------------------
#| echo: false
#| include: false

mean_x <- 120
mean_y <- 45
sd_x <- 15
sd_y <- 20
r <- 0.2
covariance <- r * sd_x * sd_y
sigma <- matrix(c(sd_x^2, covariance, covariance, sd_y^2), nrow = 2)
xy <- MASS::mvrnorm(n = 50000, mu = c(mean_y, mean_y), Sigma = sigma)
colnames(xy) <- c("x", "y")
df <- as_tibble(data.frame(xy))



## -----------------------------------------------------------------------------
df

cor(df$x, df$y)


## -----------------------------------------------------------------------------
df |> 
  ggplot(aes(x = x, y = y)) + 
  geom_point(shape = 1, alpha = 0.3) + 
  geom_smooth(se = FALSE, method = "lm")


## -----------------------------------------------------------------------------
deciles <- seq(0, 1, 0.1)

df <- df |>
  mutate(x_c = cut(x, quantile(x, deciles))) |> 
  group_by(x_c) |> 
  summarize(y_mean = mean(y)) |> 
  drop_na() 

df



## -----------------------------------------------------------------------------
df |> 
  ggplot(aes(x = x_c, y = y_mean)) + 
  geom_point(size = rel(3)) + 
  labs(title = "Do not do this.", x = "Deciles of x", y = "Mean y within Decile") + 
  theme(axis.text.x = element_text(size = rel(0.75)))


## -----------------------------------------------------------------------------
covid_df <- read_csv(here("examples", "covid-symptoms.csv"))

symptoms <- c("Anosmia", "Cough", "Fatigue", 
              "Diarrhea", "Breath", "Fever")
names(symptoms) <- symptoms
symptoms

covid_df


## -----------------------------------------------------------------------------
#| label: "07-iterating-on-data-59"
#| fig.width: 16
#| fig.height: 9
#| eval: FALSE
# # remotes::install_github("krassowski/complex-upset")
# 
# library(ComplexUpset)
# 
# upset(data = covid_df, intersect = symptoms,
#       name="Symptom Groupings by Frequency. Total pool is 1,764 individuals.",
#       min_size = 0,
#       width_ratio = 0.125) +
#     labs(title = "Co-Occurence of COVID-19 Symptoms",
#          caption = "Data: covid.joinzoe.com/us | Graph: @kjhealy")
# 
# 


## -----------------------------------------------------------------------------
#| label: "07-iterating-on-data-60"
#| fig.width: 12
#| fig.height: 7
#| echo: FALSE

# remotes::install_github("krassowski/complex-upset")

library(ComplexUpset)

upset(data = covid_df, intersect = symptoms, 
      name="Symptom Groupings by Frequency. Total pool is 1,764 individuals.", 
      min_size = 0,
      width_ratio = 0.125) +
    labs(title = "Co-Occurence of COVID-19 Symptoms",
         caption = "Data: covid.joinzoe.com/us | Graph: @kjhealy")




## -----------------------------------------------------------------------------
stmf


## -----------------------------------------------------------------------------
stmf |>
  filter(sex == "b", year > 2014 & year < 2020) |> 
  select(cname, iso3, year:sex, age_group, death_rate, rate_total)


## -----------------------------------------------------------------------------
rate_rank <- stmf |>
  filter(sex == "b", year > 2014 & year < 2020) |>
  group_by(country_code) |>
  summarize(mean_rate = mean(rate_total, na.rm = TRUE)) |>
  mutate(rate_rank = rank(mean_rate))


rate_max_rank <- stmf |>
  filter(sex == "b", year == 2020) |>
  group_by(country_code) |>
  summarize(covid_max = max(rate_total, na.rm = TRUE)) |>
  mutate(covid_max_rank = rank(covid_max))



## -----------------------------------------------------------------------------
rate_rank


## -----------------------------------------------------------------------------
rate_max_rank


## -----------------------------------------------------------------------------
df <- stmf |>
  filter(sex == "b", year > 2014, year < 2021,
         country_code %in% c("AUT", "BEL", "CHE", "DEUTNP", "DNK", "ESP", "FIN", "FRATNP", "GBR_SCO", "GBRTENW",
                             "GRC", "HUN", "ITA", "LUX", "POL", "NLD", "NOR", "PRT", "SWE", "USA")) |>
  filter(!(year == 2020 & week > 53)) |>
  group_by(cname, year, week) |>
  mutate(yr_ind = year %in% 2020) |>
  slice(1) |>
  left_join(rate_rank, by = "country_code") |>
  left_join(rate_max_rank, by = "country_code")

df


## -----------------------------------------------------------------------------
#| label: reveal-stmf

df |> 
  ggplot(aes(x = week, 
             y = rate_total, 
             color = yr_ind, 
             group = year)) +
  scale_color_manual(values = c("gray70", "firebrick"), 
                     labels = c("2015-2019", "2020")) +
  scale_x_continuous(limits = c(1, 52),
                     breaks = c(1, seq(10, 50, 10)),
                     labels = as.character(c(1, seq(10, 50, 10)))) +
  geom_line(linewidth = 0.9) +
  facet_wrap(~ reorder(cname, rate_rank, na.rm = TRUE), ncol = 5) + #<<
  labs(x = "Week of the Year", y = "Total Death Rate",
       color = "Year", title = "Overall Weekly Death Rates",
       caption = "Graph: @kjhealy. Data: Human Mortality Database, mortality.org") -> p_stmf


## -----------------------------------------------------------------------------
#| label: stmf_plot
#| echo: FALSE
#| fig.height: 12
#| fig.width: 15
p_stmf


## -----------------------------------------------------------------------------
gss_sm


## -----------------------------------------------------------------------------
gss_sm |> 
  group_by(bigregion) |> 
  tally()


## -----------------------------------------------------------------------------
gss_sm |> 
  group_by(race, sex, degree) |> 
  summarize(mean_kids = mean(childs, na.rm = TRUE), 
            median_age = median(age, na.rm = TRUE))


## -----------------------------------------------------------------------------
lm(lifeExp ~ log(gdpPercap), data = gapminder) |> 
  summary()


## -----------------------------------------------------------------------------
lm(lifeExp ~ log(gdpPercap), data = gapminder) |> 
  broom::tidy()


## -----------------------------------------------------------------------------
#| label: "r 07-[Iterating]{.fg-green}-20"
eu77 <- gapminder |> filter(continent == "Europe", year == 1977)

eu77


## -----------------------------------------------------------------------------
fit <- lm(lifeExp ~ log(gdpPercap), data = eu77)

tidy(fit)


## -----------------------------------------------------------------------------
fit <- lm(lifeExp ~ log(gdpPercap), data = eu77)

tidy(fit)


## -----------------------------------------------------------------------------
#| label: "r 07-[Iterating]{.fg-green}-22"

df_nest <- gapminder |>
    group_by(continent, year) |>
    nest()

df_nest



## -----------------------------------------------------------------------------
#| label: "r 07-[Iterating]{.fg-green}-23"
df_nest |> filter(continent == "Europe" & year == 1977) |> 
    unnest(cols = c(data))


## -----------------------------------------------------------------------------
#| label: "r 07-[Iterating]{.fg-green}-24"
#| echo: FALSE
old_digits <- getOption("digits")
options(digits = 3)


## -----------------------------------------------------------------------------
#| label: "r 07-[Iterating]{.fg-green}-25"

fit_ols <- function(df) {
    lm(lifeExp ~ log(gdpPercap), data = df)
}

df_nest <- gapminder |>
    group_by(continent, year) |>
    nest() |> 
    mutate(model = map(data, fit_ols)) #<<


## -----------------------------------------------------------------------------
#| label: "07-iterating-on-data-63"
df_nest


## -----------------------------------------------------------------------------
#| label: "r 07-[Iterating]{.fg-green}-26"

fit_ols <- function(df) {
    lm(lifeExp ~ log(gdpPercap), data = df)
}

df_tidy <- gapminder |>
    group_by(continent, year) |>
    nest() |> 
    mutate(model = map(data, fit_ols),
           tidied = map(model, tidy)) |>
    unnest(cols = c(tidied)) |>
    filter(term %nin% "(Intercept)" &
           continent %nin% "Oceania")


## -----------------------------------------------------------------------------
#| label: "07-iterating-on-data-64"
df_tidy


## -----------------------------------------------------------------------------
#| label: "07-iterating-on-data-65"
df_tidy |> 
    ungroup() |>
    sample_n(5)


## -----------------------------------------------------------------------------
#| label: "06-work-with-models-54"

# New model
fit_ols2 <- function(df) {
    lm(lifeExp ~ log(gdpPercap) + log(pop), data = df)
}

df_tidy <- gapminder |>
    group_by(continent, year) |>
    nest() |> 
    mutate(model = map(data, fit_ols2),
           tidied = map(model, tidy)) 

df_tidy


## -----------------------------------------------------------------------------
#| label: "06-work-with-models-55"

# Plot the output from our model
mod_plot <- function(results, title){
  results |> 
    filter(term %nin% "(Intercept)") |> 
    ggplot(mapping = aes(x = estimate,
                         xmin = estimate - std.error,
                         xmax = estimate + std.error,
                         y = reorder(term, estimate))) + 
    geom_pointrange() + 
    labs(title = title, 
         y = NULL)
}


## -----------------------------------------------------------------------------
eu77_results <- fit_ols2(eu77) |> tidy()

eu77_results


## -----------------------------------------------------------------------------
mod_plot(results = eu77_results, title = "The EU 77 Model")


## -----------------------------------------------------------------------------
#| label: "06-work-with-models-56"
df_tidy <- gapminder |> group_by(continent, year) |> 
  nest() |> 
  mutate(title = paste(continent, year),
         model = map(data, fit_ols2),
         tidied = map(model, tidy), 
         ggout = pmap(list(tidied, title), #<<
                      mod_plot)) #<<


## -----------------------------------------------------------------------------
df_tidy


## -----------------------------------------------------------------------------
#| label: "06-work-with-models-57"
#| fig.height: 3
#| fig.width: 6
df_tidy$ggout[[8]]


## -----------------------------------------------------------------------------
#| label: "06-work-with-models-58"
#| fig.height: 3
#| fig.width: 6
df_tidy$ggout[[18]]


## -----------------------------------------------------------------------------
# If a root-level figures directory doesn't exist, create one
if(!dir.exists(here("figures"))) dir.create(here("figures"))

fs::dir_ls(here("figures"))


## -----------------------------------------------------------------------------
#| label: "06-work-with-models-59"

pwalk(
  list(
    filename = paste0(df_tidy$title, ".png"),
    plot = df_tidy$ggout,
    path = here("figures"),
    height = 3, width = 4,
    dpi = 300
  ),
  ggsave
)


                       


## -----------------------------------------------------------------------------
#| label: "06-work-with-models-60"
fs::dir_ls(here("figures")) |> 
  basename()


## -----------------------------------------------------------------------------
fs::dir_delete(here("figures"))

