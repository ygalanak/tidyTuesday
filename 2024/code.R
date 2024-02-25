# R TidyTuesday ---------------------------------------------------------------#
#-Author: Yannis Galanakis; galanakis.gian@gmail.com-----Created: Feb 24, 2023-#
#-R Version: 4.3.2---------------------------------------Revised: Feb 25, 2024-#
#-Output: Plots for week8 2024-------------------------------------------------#

# Load libraries ----
pacman::p_load(
  "tidyverse", "here", "showtext", "ggimage", "fontawesome", "ggtext", "scales",
  "patchwork", "colorspace", "nord", "paletteer", "glue", "janitor", 
  "summarytools", "genderizeR")

# ==============================================================================#
# Data Load-in------------------------------------------------------------------
# ==============================================================================#
tuesdata <- tidytuesdayR::tt_load(2024, week = 8)

isc_grants <- tuesdata$isc_grants
rm(tuesdata)


# ==============================================================================#
# Exploratory Data Analysis-----------------------------------------------------
# ==============================================================================#
dfSummary(isc_grants) |> view()

names(isc_grants)

# ==============================================================================#
# Data Wrangling----------------------------------------------------------------
# ==============================================================================#
# Website hosting these projects
webpages_hosting <- isc_grants %>%
  mutate(website_name = str_extract(website, "(?<=://)[^/]+")) |> 
  count(website_name, sort = TRUE) 


# Funding per year, number of projects funded and average per grant
fundings <- isc_grants |> 
  group_by(year) |> 
  summarise(grant = sum(funded)) |> 
  left_join(isc_grants |> count(year)) |> 
  mutate(average = grant/n) |> 
  rename(number = n) |> 
  pivot_longer(cols = -year)

# Funding per year and group, number of projects funded and average per grant
fundings_by_year_and_cycle <- isc_grants %>%
  mutate(cycle = case_when(
  group == 1 ~ "spring",
  group == 2 ~ "fall",
  TRUE ~ as.character(group) # Handles unexpected values gracefully
  )) %>%
  group_by(year, cycle) %>%
  summarise(
    total_funding = sum(funded), # Total funding per year and cycle
    number_of_projects = n(), # Number of projects funded per year and cycle
    .groups = 'drop' # Drop grouping structure afterwards
  ) %>%
  mutate(
    average_funding = total_funding / number_of_projects # Average funding per grant per year and cycle
  ) |>
  pivot_longer(cols = c(total_funding, number_of_projects, average_funding), 
               names_to = "metric", values_to = "value")


# ==============================================================================#
# Options & Visualization Parameters--------------------------------------------
# ==============================================================================#

# Load fonts
font_add_google("IBM Plex Sans",
                family = "title_font"
) # Font for titles
font_add_google("IBM Plex Sans Condensed",
                family = "caption_font"
) # Font for the caption
font_add_google("IBM Plex Sans",
                family = "body_font"
) # Font for plot text
showtext_auto()

# Options of Colour palettes to use in visualization
mypal1 <- paletteer::paletteer_d("nord::afternoon_prarie")

# Chossing one Palette for the Visualization
mypal <- mypal1[c(1,2,4,6,7, 8)]

# Define colours
bg_col <- "white"  # Background Colour
text_col <- "#2E3440" # Colour for the text
text_hil <- mypal[4] # Colour for highlighted text

# Define Text Size
ts <- unit(20, units = "cm") # Text Size

# Caption stuff
sysfonts::font_add(
  family = "Font Awesome 6 Brands",
  regular = here::here("Font Awesome 6 Brands-Regular-400.otf")
)
sysfonts::font_add(
  family = "Font Awesome 6 Solid",
  regular = here::here("Font Awesome 6 Free-Solid-900.otf")
)
sysfonts::font_add(
  family = "Font Awesome 6 Regular",
  regular = here::here("Font Awesome 6 Free-Regular-400.otf")
)
github <- "&#xf09b"
github_username <- "ygalanak"
xtwitter <- "&#xe61b"
xtwitter_username <- "@YannisGalanakis"
social_caption_1 <- glue::glue("<span style='font-family:\"Font Awesome 6 Brands\";'>{github};</span> <span style='color: {text_col}'>{github_username}  </span>")
social_caption_2 <- glue::glue("<span style='font-family:\"Font Awesome 6 Brands\";'>{xtwitter};</span> <span style='color: {text_col}'>{xtwitter_username}</span>")

# Add text to plot--------------------------------------------------------------
plot_title <- "R Consortium ISC Grants"
plot_caption <- paste0("**Data:** R Consortium Infrastructure Steering Committee (ISC) Grant Program <br> **Code:** ", social_caption_1, " | ", " **Graphics:** ", social_caption_2)

# ==============================================================================#
# Data Visualization------------------------------------------------------------
# ==============================================================================#

# Plot 1: Line Plots -----------------------------------------------------------
labels1 <- c("Average funding per project (US $)",
             "Total funding given each year (US $)",
             "Number of projects funded, by ISC")
names(labels1) <- c("average_funding", "total_funding", "number_of_projects")
g1 <- fundings_by_year_and_cycle |> 
  ggplot(mapping = aes(
    x = year,
    y = value,
    colour = cycle
  )) +
  geom_line(
            alpha = 0.8,
            linewidth = 1.5,
            key_glyph = draw_key_point) +
  geom_point(size = 2.4) +
  facet_wrap(
    ~metric, 
    ncol = 1, 
    scales = "free_y",
    labeller = as_labeller(labels1)
  ) +
  scale_y_continuous(
    labels = scales::label_number(
      big.mark = ",",
      accuracy = 1
    )
  ) +
  scale_x_continuous(
    breaks = 2016:2023
  ) +
  scale_colour_manual(
    values = c("spring" = mypal[1], "fall" = mypal[4])
  ) +
  theme_minimal() +
  labs(
    x = NULL,
    y = NULL,
    subtitle = "ISC Grants: 2016 to 2022",
    color = NULL
  ) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(
      colour = "darkgrey",
      linewidth = 0.2,
      linetype = 2
    ),
    axis.text = element_text(
      size = 1.5 * ts,
      family = "body_font",
      colour = text_col,
      face = "bold",
      margin = margin(0, 0, 0, 0)
    ),
    strip.text = element_text(
      size = 2 * ts,
      family = "body_font",
      colour = text_col,
      hjust = 0.5
    ),
    plot.subtitle = element_text(
      size = 2.5 * ts,
      family = "title_font",
      hjust = 0.5,
      colour = text_hil
    ),
    legend.position = "bottom",
    legend.text = element_text(
      size = 1.5 * ts,
      family = "body_font",
      colour = text_col
    ),
  ) 

# Plot 2: Webpages donut--------------------------------------------------------
g4 <- webpages_hosting |> 
  mutate(
    website_name = fct(website_name),
    website_name = fct_lump_n(website_name, n = 1, w = n,
                              other_level = "other")
  ) |>
  group_by(website_name) |> 
  mutate(website_name = case_when(
    website_name == "other" ~ "Other",
    website_name == "github.com" ~ "github"
  )) |>
  summarise(count = sum(n)) |> 
  mutate(fraction = count / sum(count) * 100,
         ymax = cumsum(fraction),
         ymin = c(0, head(ymax, n = -1)),
         labelPosition = (ymax + ymin) / 2,
         label = paste0(website_name) 
         ) |>
  ggplot(mapping = aes(
    ymax = ymax+2,
    ymin = ymin-2,
    xmax = 4,
    xmin = 3,
    fill = website_name,
    color = website_name,
    label = website_name
  )) +
  geom_rect() +
  #geom_col(color = "white") +
  geom_text(
    family = "body_font",
    size = .6*ts,
    x = 1, aes(y = labelPosition, label = label, color = website_name)
  ) +
  coord_polar(theta = "y") + 
  labs(
    subtitle = "Where are projects hosted?"
  ) +
    scale_fill_manual(values = mypal[c(2, 4, 6)]) +
  scale_color_manual(values = mypal[c(2, 4, 6)]) +
  theme_void() +
  xlim(c(-5,5)) +
  theme(
    legend.position = "none",
    plot.subtitle = element_text(
      size = 1.8 * ts,
      family = "title_font",
      hjust = 0.5,
      colour = text_hil
    )
  )

# Plot 3: gender of applicants lines ---------------------------------------------
library(genderizeR)

gender <- isc_grants |>
  select(year, group, proposed_by) |>
  # one applicant or many?
  mutate(multiple = if_else(str_detect(proposed_by, "and|,"), "multiple", "single")) |>
  # among the single authored
  filter(multiple == "single") |>
  # Splitting the proposed_by column into first_name and last_name
  separate(col = proposed_by, into = c("first_name", "last_name"), sep = " ")

# Search for terms that could be first names
givenNames = findGivenNames(gender$first_name, progress = FALSE)

gender <- gender |>
  inner_join(givenNames |> 
               # make first letter capital to merge
               mutate(first_name = str_to_title(gender)) |>
               select(first_name, probability)
               , by = c("first_name")) |>
  distinct() |>
  group_by(year, probability) |>
  summarise(n = n())

g5 <- gender |>
  ggplot(aes(x = year, y = n, colour = probability)) + 
  geom_line(alpha = 0.8,linewidth = 1.5, key_glyph = draw_key_point) +
  geom_point(size = 2.4) +
  scale_colour_manual(
    values = c("male" = mypal[1], "female" = mypal[4])
  ) +
  theme_minimal() +
  labs(
    x = NULL,
    y = "Number of applicants",
    subtitle = "Applicants' gender gap",
    color = NULL,
    caption = "**Note:** genderizeR package assigns the applicant gender based on the probability of the first name among the single-authored projects."
  ) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(
      colour = "darkgrey",
      linewidth = 0.2,
      linetype = 2
    ),
    axis.title.y = element_text(
      size = 1.5 * ts,
      family = "body_font",
      colour = text_col,
      face = "bold",
      margin = margin(0, 0, 0, 0)
    ),
    axis.text = element_text(
      size = 1.5 * ts,
      family = "body_font",
      colour = text_col,
      face = "bold",
      margin = margin(0, 0, 0, 0)
    ),
    strip.text = element_text(
      size = 2 * ts,
      family = "body_font",
      colour = text_col,
      hjust = 0.5
    ),
    plot.title = element_text(
      size = 3*ts,
      family = "title_font",
      hjust = .5,
      colour = text_col,
    ),
    plot.subtitle = element_markdown(
      size = 2.5 * ts,
      family = "title_font",
      hjust = 0.5,
      colour = text_hil
    ),
    plot.caption = element_markdown(
      size = 1.5 * ts,
      family = "caption_font",
      hjust = 0.5,
      colour = text_col,
      margin = margin(0, 0, 0, 0,
                      unit = "cm"
      )
    ),
    legend.position = "bottom",
    legend.text = element_text(
      size = 1.5 * ts,
      family = "body_font",
      colour = text_col
    )
  ) 

# Plot 4: distribution of funding by gender and cycle among single-authored-----
distribution.df <- isc_grants |>
  select(year, group, proposed_by, funded) |>
  # one applicant or many?
  mutate(multiple = if_else(str_detect(proposed_by, "and|,"), "multiple", "single")) |>
  # among the single authored
  filter(multiple == "single") |>
  # Splitting the proposed_by column into first_name and last_name
  separate(col = proposed_by, into = c("first_name", "last_name"), sep = " ") |>
  inner_join(givenNames |> 
               # make first letter capital to merge
               mutate(first_name = str_to_title(gender)) |>
               select(first_name, probability)
             , by = c("first_name")) |>
  distinct()

options(scipen = 999) # non-scientific numbers

g2 <- distribution.df |>
  mutate(cycle = case_when(
    group == 1 ~ "spring",
    group == 2 ~ "fall",
    TRUE ~ as.character(group) # Handles unexpected values gracefully
  )) |>
  ggplot(aes(x = funded, colour = probability)) +
  geom_density(alpha = 0.8,linewidth = 1.5, key_glyph = draw_key_point) +
  facet_wrap(~cycle, scales = "free", ncol = 1) +
  scale_colour_manual(
    values = c("male" = mypal[1], "female" = mypal[4])
  ) +
  scale_x_continuous(labels=scales::comma) +
  theme_minimal() +
  labs(x = "Amount funded for the project, $",
       y = "Density",
       subtitle = "Distribution of funding, <br>by gender and application cycle",
       color = NULL,
       caption = "**Note:** 'genderizeR' package assigns the applicant gender based on <br>the probability of the first name among the <br>single-authored projects."
  ) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(
      colour = "darkgrey",
      linewidth = 0.2,
      linetype = 2
    ),
    axis.title.y = element_text(
      size = 1.5 * ts,
      family = "body_font",
      colour = text_col,
      face = "bold",
      margin = margin(0, 0, 0, 0)
    ),
    axis.title.x = element_text(
      size = 1.5 * ts,
      family = "body_font",
      colour = text_col,
      face = "bold",
      margin = margin(0, 0, 0, 0)
    ),
    axis.text = element_text(
      size = 1.5 * ts,
      family = "body_font",
      colour = text_col,
      face = "bold",
      margin = margin(0, 0, 0, 0)
    ),
    strip.text = element_text(
      size = 2 * ts,
      family = "body_font",
      colour = text_col,
      hjust = 0.5
    ),
    plot.title = element_text(
      size = 3*ts,
      family = "title_font",
      hjust = .5,
      colour = text_col,
    ),
    plot.subtitle = element_markdown(
      size = 2.5 * ts,
      family = "title_font",
      hjust = 0.5,
      colour = text_hil,
      margin = margin(b=15, t=15),
      lineheight = .5
    ),
    plot.caption = element_markdown(
      size = 1.5 * ts,
      family = "caption_font",
      hjust = 0.5,
      colour = text_col,
      halign = 0,
      margin = margin(b = 15, t = 15),
      lineheight = 0.5
    ),
    legend.position = "bottom",
    legend.text = element_text(
      size = 1.5 * ts,
      family = "body_font",
      colour = text_col
    )
  ) 

  

my_layout <- "
  AABBBB
  AABBBB
  AABBBB
  AACCDD
  AACCDD
"

g <- g1 + g5+ g4 + g2 +
  plot_layout(design = my_layout) +
  plot_annotation(
    title = plot_title,
    caption = plot_caption,
    theme = theme(
      plot.caption = ggtext::element_textbox_simple(
        family = "caption_font",
        colour = text_col,
        size = 2 * ts,
        hjust = 1,
        halign = 0,
        margin = margin(b = 15, t = 15),
        lineheight = 0.5,
      ),
      plot.title = element_text(
        hjust = 0.5,
        size = 6.5 * ts,
        family = "title_font",
        face = "bold",
        colour = text_col,
        margin = margin(1, 0, 1, 0,
                        unit = "cm"
        )
      ),
      plot.background = element_rect(
        fill = bg_col,
        color = bg_col,
        linewidth = 0
      ),
      plot.title.position = "plot"
    )
  )

# =============================================================================#
# Image Saving-----------------------------------------------------------------
# =============================================================================#


ggsave(
  filename = here::here("tidy_isc_grants.png"),
  plot = g,
  width = 30,
  height = 30,
  units = "cm",
  bg = bg_col
)
