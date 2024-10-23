library(shiny)

ui <- fluidPage(
	titlePanel(
		"Data Science Capstone: Word Predictor"
	),
	sidebarLayout(
		sidebarPanel(
			textInput("textinput", "Enter text (words, phrases or part of a sentence):"),
			submitButton("Submit"),
			br(),
			p("Please wait. It might take some time to compute.")
		),
		mainPanel(
			strong("Text input:"),
			textOutput("word"),
			br(),
			strong("Predicted word:"),
			textOutput("pred_word")
		)
	)
)
