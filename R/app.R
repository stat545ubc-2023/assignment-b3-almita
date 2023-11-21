library(shiny)
library(lubridate)
library(dplyr)
library(tidyr)
library(stringr)
library(DT)
library(shinyWidgets)
library(bslib)
library(bsicons)

# import data and transform release date into date object
movies <- read.csv("data/horror_movies.csv") %>%
	mutate(release_date = ymd(release_date))

# get list of unique genres
genres <- movies %>%
	separate_rows(genre_names, sep = ",") %>%
	pull(genre_names) %>%
	str_squish() %>%
	unique() 


# ui
ui <- page_sidebar(
	title = h1("horror movies"),
	sidebar = sidebar(
		title = h2("what do you want to watch?"),
		
		# date slider
		card(
			sliderInput("release_date", "release date", 
									min = year(min(movies$release_date)),
									max = year(max(movies$release_date)),
									value = c(year(min(movies$release_date)), year(max(movies$release_date))),
									sep = "" 
			),
		),
		
		# rating slider
		card(
			sliderInput("rating", "rating",
									min = min(movies$vote_average),
									max = max(movies$vote_average),
									value = c(5, max(movies$vote_average))
			),
		),
		
		# genre selector
		card(
			virtualSelectInput("genres", "genres",
												 choices = genres,
												 selected = genres,
												 multiple = TRUE,
												 search = TRUE,
												 showValueAsTags = TRUE
			),
		),
		
		# movies found
		value_box(
			title = "movies found",
			value = textOutput("resultsFound"),
			showcase = bs_icon("camera-reels-fill")
		)
		),
	
	# movie results table
	card(
		card_body(
			min_height = 500,
			div(
				DTOutput("results")
			))
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
)

# server
server <- function(input, output, session) {
	
	# create reactive dataset
	filtered <- reactive({
		movies %>%
			filter(year(release_date) >= input$release_date[1],
						 year(release_date) <= input$release_date[2],
						 vote_average > input$rating[1],
						 vote_average < input$rating[2],
						 str_detect(genre_names,paste0(input$genres, collapse = "|"))
			) %>%
			select(original_title, overview, release_date, popularity, vote_average, runtime, genre_names) %>%
			rename(title = original_title,
						 `release date` = release_date,
						 rating = vote_average,
						 genre = genre_names)
	})
	
	# datatable output
	output$results <- renderDT(filtered(), 
														 options = list(paging = FALSE,
																						searching = FALSE))
	
	# movies found count
	output$resultsFound <- renderText({
		filtered() %>%
			nrow()
	})
	
}

shinyApp(ui, server)