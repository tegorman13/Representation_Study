pacman::p_load(ggplot2,wesanderson,glue)


theme_blank <- theme(
  panel.grid = element_blank(),
  axis.text = element_blank(),
  axis.title = element_blank(),
  axis.ticks = element_blank()
)


stat_bar <- list(stat_summary(fun=mean, geom="bar", position=position_dodge()),
                 stat_summary(fun.data=mean_se, geom="errorbar", color="black", position=position_dodge(), linewidth=.5))

 
round_tibble <- function(tbl, rn) {
  tbl %>% 
    mutate(across(where(is.numeric), ~round(., rn)))
}




theme_nice_dist <- function() {
  theme_nice() +
    theme(
      panel.grid = element_blank(),
      panel.spacing.x = unit(10, units = "pt"),
      axis.ticks.x = element_line(linewidth = 0.25),
      axis.text.y = element_blank()
    )
}


# theme_set(theme_nice())

theme_nice <- function() {
  #theme_minimal(base_family = "Manrope") +
  theme_minimal() +
    theme(
      panel.grid.minor = element_blank(),
      plot.title = element_text(family = "Manrope Extrabold", face = "plain", size = rel(1.35)),
      plot.subtitle = element_text(family = "Manrope Medium", face = "plain", size = rel(1.2)),
      axis.title = element_text(family = "Manrope SemiBold", face = "plain", size = rel(1)),
      axis.title.x = element_text(hjust = .5),
      axis.title.y = element_text(hjust = .5),
      axis.text = element_text(family = "Manrope Light", face = "plain", size = rel(0.8)),
      strip.text = element_text(
        family = "Manrope", face = "bold",
        size = rel(.95), hjust = 0
      ),
      legend.position = "top",
      legend.text = element_text(family = "Manrope Light", face = "plain", size = rel(0.95)),
      strip.background = element_rect(fill = "grey90", color = NA)
    )
}


theme_nice_pdf <- function() {
 theme_minimal() +
    theme(
      panel.grid.minor = element_blank(),
      plot.title = element_text( face = "plain", size = rel(1.35)), #family = "Manrope Extrabold",
      plot.subtitle = element_text(face = "plain", size = rel(1.2)), # family = "Manrope Medium", 
      axis.title = element_text(face = "plain", size = rel(1)), #family = "Manrope SemiBold", 
      axis.title.x = element_text(hjust = .5),
      axis.title.y = element_text(hjust = .5),
      axis.text = element_text(face = "plain", size = rel(0.8)),
      strip.text = element_text(
         face = "bold", #family = "Manrope SemiBold", 
        size = rel(.95), hjust = 0
      ),
      legend.position = "top",
      legend.text = element_text(face = "plain", size = rel(0.95)), #family = "Manrope Light",
      strip.background = element_rect(fill = "grey90", color = NA)
    )
}

theme_nice_b <- function() {
  theme_minimal(base_family = "Manrope") +
    theme(
      panel.grid.minor = element_blank(),
      plot.title = element_text(family = "Manrope Extrabold", face = "plain", size = rel(1.55)),
      plot.subtitle = element_text(family = "Manrope Medium", face = "plain", size = rel(1.2)),
      axis.title = element_text(family = "Manrope SemiBold", face = "plain", size = rel(1)),
      axis.title.x = element_text(hjust = .5),
      axis.title.y = element_text(hjust = .5),
      axis.text = element_text(face = "plain", size = rel(0.9)),
      strip.text = element_text(
        family = "Manrope", face = "bold",
        size = rel(1.2), hjust = 0
      ),
      legend.position = "top",
      legend.text = element_text( face = "plain", size = rel(1.3)), #family = "Manrope Light",
      strip.background = element_rect(fill = "grey90", color = NA)
    )
}



geom_flat_violin <- function(mapping = NULL, data = NULL, stat = "ydensity",
                             position = "dodge", trim = TRUE, scale = "area",
                             show.legend = NA, inherit.aes = TRUE, ...) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomFlatViolin,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      trim = trim,
      scale = scale,
      ...
    )
  )
}




col_themes <- tibble::lst(darjeeling = c(wes_palette("Darjeeling1"),wes_palette("Darjeeling2"), ggokabeito::palette_okabe_ito(),
  wes_palette("AsteroidCity1"), wes_palette("AsteroidCity2")), 
                          wes2 = wes_palette("AsteroidCity1"), 
                          okabeito = c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#999999", "#000000"))

# okabeito <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#999999", "#000000")
# wes2 <- wes_palette("AsteroidCity1")


#### COLOR THEME ##### 
# darjeeling <- c(wes_palette("Darjeeling1"),wes_palette("Darjeeling2"))
#cat(darjeeling)


scale_colour_discrete <- function(...) {
  scale_colour_manual(..., values = col_themes$darjeeling)
}

scale_fill_discrete <- function(...) {
  scale_fill_manual(..., values = col_themes$darjeeling)
}

options(ggplot2.continuous.colour=col_themes$darjeeling)
options(ggplot2.continuous.fill = col_themes$darjeeling)