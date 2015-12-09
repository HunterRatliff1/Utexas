theme_UT <- function() {
  ## Adds style to ggplot by altering a theme function from
  ## the ggthemes package
  # 
  require(ggthemes)
  
  theme_economist_white() + theme(
    legend.position = "bottom",
    legend.background = element_rect(fill = "transparent"),
    title           = element_text(color="#BF5700", family = "Georgia"),
    axis.title      = element_text(face = "bold"),
    plot.background = element_rect(fill = "#ECE9E2"),
    axis.text       = element_text(color="#333F48", family = "Tahoma"),
    panel.background = element_rect(fill = "#D6D2C4")
  )
}