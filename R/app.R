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
		# date slider
		card(
			h3("release date"),
			sliderInput("release_date", NULL, 
									min = year(min(movies$release_date)),
									max = year(max(movies$release_date)),
									value = c(year(min(movies$release_date)), year(max(movies$release_date))),
									sep = "" 
			),
		),
		
		# rating slider
		card(
			h3("rating"),
			sliderInput("rating", NULL,
									min = min(movies$vote_average),
									max = max(movies$vote_average),
									value = c(5, max(movies$vote_average))
			),
		),
		
		# genre selector
		card(
			h3("genres"),
			virtualSelectInput("genres", NULL,
												 choices = genres,
												 selected = genres[5:9],
												 multiple = TRUE,
												 search = TRUE,
												 showValueAsTags = TRUE,
												 dropboxWrapper = "body"
			),
		),
		
		# movies found
		card(value_box(
			title = "movies found",
			value = textOutput("resultsFound"),
			height = "120px"
			)
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
	
	# div(DTOutput("results")),
	
	# CSS
	tags$head(
		tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
	),
)

# server
server <- function(input, output, session) {
	
	# create reactive dataset
	filtered <- reactive({
		
		if (is.null(input$genres)) {
			return(NULL)
		}
		
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
																						searching = FALSE),
														 class = list(stripe = FALSE),
														 rownames = FALSE)
	
	# movies found count
	output$resultsFound <- renderText({
		filtered() %>%
			nrow()
	})
	
}

shinyApp(ui, server)