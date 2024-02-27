# R TidyTuesday ---------------------------------------------------------------#
#-Author: Yannis Galanakis; galanakis.gian@gmail.com-----Created: Feb 26, 2024-#
#-R Version: 4.3.2---------------------------------------Revised: Feb 27, 2024-#
#-Output: Plots for week9 2024-------------------------------------------------#

# Load libraries ----
pacman::p_load(
  "tidyverse", "here", "showtext", "ggimage", "fontawesome", "ggtext", "scales",
  "patchwork", "colorspace", "nord", "paletteer", "glue", "janitor","emojifont",
  "summarytools", "genderizeR")

# ==============================================================================#
# Data Load-in------------------------------------------------------------------
# ==============================================================================#
tuesdata <- tidytuesdayR::tt_load(2024, week = 9)

births <- tuesdata$births
rm(tuesdata)

# ==============================================================================#
# Exploratory Data Analysis-----------------------------------------------------
# ==============================================================================#
dfSummary(births) |> view()

names(births)

# ==============================================================================#
# Data Wrangling----------------------------------------------------------------
# ==============================================================================#
# by gender and year
gender <- births |>
  select(-year_death) |>
  # separate first to second names
  separate(col = person, into = c("first_name", "last_name"), sep = " ")

# Search for terms that could be first names
givenNames = findGivenNames(gender$first_name, progress = FALSE)

gender <- gender |>
  inner_join(givenNames |> 
               # make first letter capital to merge
               mutate(first_name = str_to_title(gender)) |>
               select(first_name, probability)
             , by = c("first_name")) |>
  distinct() |>
  # create nationality from description and capture New Zealand as phrase
  mutate(nationality = str_extract(description, "New Zealand|South Africa|^[\\w-]+")) |>
  # if nationality has numbers make it NA
  mutate(nationality = ifelse(nationality |> str_detect("\\d"), NA, nationality)) |>
  # improve some odd cases by hand
  mutate(nationality = ifelse(nationality == "Prime", "Spanish", nationality),
         nationality = ifelse(nationality == "founder", NA, nationality),
         nationality = ifelse(nationality == "born", "American",nationality),
         nationality = ifelse(nationality == "Duke", NA, nationality),
         nationality = ifelse(nationality == "Particular", NA, nationality),
         # if English or Welsh, nationality == "British"
         nationality = case_when(
           nationality == "Welsh" ~ "British",
           nationality == "English" ~ "British",
           TRUE ~ nationality
         )
         ) |>
  # dual or single nationality: dual if there is a dash
  mutate(dual = ifelse(nationality |> str_detect("-"), "dual", "single")) 
  
# =============================================================================#
# Options & Visualization Parameters--------------------------------------------
# =============================================================================#

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

# Caption stuff
sysfonts::font_add(
  family = "Font Awesome 6 Brands",
  regular = ("Font Awesome 6 Brands-Regular-400.otf")
)
sysfonts::font_add(
  family = "Font Awesome 6 Solid",
  regular = ("Font Awesome 6 Free-Solid-900.otf")
)
sysfonts::font_add(
  family = "Font Awesome 6 Regular",
  regular = ("Font Awesome 6 Free-Regular-400.otf")
)

# Define colours
bg_col <- "white"  # Background Colour
text_col <- "#2E3440" # Colour for the text

# Define Text Size
ts <- unit(20, units = "cm") # Text Size

github <- "&#xf09b"
github_username <- "ygalanak"
xtwitter <- "&#xe61b"
xtwitter_username <- "@YannisGalanakis"
social_caption_1 <- glue::glue("<span style='font-family:\"Font Awesome 6 Brands\";'>{github};</span> <span style='color: {text_col}'>{github_username}  </span>")
social_caption_2 <- glue::glue("<span style='font-family:\"Font Awesome 6 Brands\";'>{xtwitter};</span> <span style='color: {text_col}'>{xtwitter_username}</span>")

plot_caption <- paste0("**Data:** Wikipedia <br> **Code:** ", social_caption_1, " | ", " **Graphics:** ", social_caption_2)


# Plot 1: Births by year and nationality
labs <- data.frame(probability = c("male", "female"),
                   label = c("\uf183", "\uf182"))
load.fontawesome()

genderplot <- gender %>%
  filter(dual == "single") %>%
  mutate(nationality = ifelse(is.na(nationality), "Unknown", nationality)) %>%
  # Create a temporary count for nationality frequencies
  add_count(nationality, name = "freq") %>%
  # Replace nationality with 'Others' if frequency is <3 or is NA
  mutate(nationality = ifelse(freq < 3 | is.na(freq), "Others", nationality)) %>%
  # Remove the temporary frequency count
  select(-freq) %>%
  left_join(labs, by = "probability") %>%
  # Ensure nationality is a factor and sort levels alphabetically, placing 'Others' last
  mutate(nationality = factor(nationality,
                              levels = c(sort(unique(nationality)[unique(nationality) != "Others"]),
                                         "Others")))

# Proceed with your ggplot code
w9 <- ggplot(genderplot[genderplot$year_birth>1800,], aes(x = year_birth, y = nationality, colour = probability)) +
  #geom_point() # Uncomment this if you want to include points
  geom_text(aes(label=label), family='Font Awesome 6 Solid', size = 25) +
  labs(x = NULL,
       y = NULL,
       title = "Leap Year's births after 1800",
       caption = "**Note:** `genderizeR' package assigns gender based on the probability of the first name of each person.") +
  scale_color_manual(values = c("#D6BBCFFF", "#486090FF")) +
  theme_minimal() + 
  theme(legend.position = "none",
        plot.caption = element_textbox(
          size = 2 * ts
        ),
        axis.text = element_text(
          size = 3 * ts,
          family = "body_font",
          colour = text_col,
          face = "bold",
          margin = margin(0, 0, 0, 0)
        ),
        plot.title = element_text(
          size = 5 * ts,
          family = "title_font",
          colour = text_col,
          face = "bold",
          margin = margin(0, 0, 0, 0)
        ),
        ) +
  plot_annotation(
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

ggsave(
  filename = ("LeapYear_births.png"),
  plot = w9,
  width = 30,
  height = 30,
  units = "cm",
  bg = bg_col
)  
