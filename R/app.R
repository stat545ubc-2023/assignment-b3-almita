library(shiny)
library(lubridate)
library(dplyr)
library(tidyr)
library(stringr)
library(shinyWidgets)
library(bslib)

movies <- read.csv("data/horror_movies.csv") %>%
	mutate(release_date = ymd(release_date))

genres <- movies %>%
	separate_rows(genre_names, sep = ",") %>%
	pull(genre_names) %>%
	str_squish() %>%
	unique() 

ui <- page_sidebar(
	title = h1("horror movies"),
	sidebar = sidebar(
		title = h2("what do you want to watch?"),
		sliderInput("release_date", "release date", 
								min = year(min(movies$release_date)),
								max = year(max(movies$release_date)),
								value = c(year(min(movies$release_date)), year(max(movies$release_date))),
								sep = "" 
								),
		virtualSelectInput("genres", "genres",
											 choices = genres,
											 selected = genres,
											 multiple = TRUE,
											 search = TRUE,
											 showValueAsTags = TRUE
											 )
	),
	tags$head(
		tags$style(
			HTML(
				'
				.vscomp-toggle-button {
						background-color: white;
				}
				span.vscomp-value-tag {
    				border: 1px solid black !important;
				}
				'
			)
		)
	),
	#theme = bslib::bs_theme(),
	"Main content area"
)
server <- function(input, output, session) {
}
shinyApp(ui, server)