function(input, output, session) {
  # Reactive values for story stages and stats
  currentstage <- reactiveVal("start")
  nextstage <- reactiveVal("shower")
  alertness <- reactiveVal(0)
  thoughts <- reactiveVal(0)
  
  observeEvent(input$go, {
    # Takes story stage, choice taken and stats and returns the next stage.
    if (currentstage() == "start") {
      nextstage("shower")
    } else if (currentstage() == "shower") {
      # Choices for shower
      req(input$choice)
      if (input$choice == "A bit too hot") {
        nextstage("hot")
      }
      else if (input$choice == "Icy cold") {
        thoughts(thoughts() + 5) # Update thoughts
        nextstage("cold")
      }
      else if (input$choice == "Just right") {
        nextstage("right")
      }
    } else if (currentstage() %in% c("hot","cold")) {
      nextstage("breakfast")
    } else if (currentstage() == "breakfast") {
      # Choices for breakfast
      req(input$choice)
      if (input$choice == "Cereal") {
        nextstage("cereal")
      } else if (input$choice == "Coffee and Toast") {
        alertness(alertness() + 100) # Update alertness
        nextstage("coffeetoast")
      } else if (input$choice == "Full English Breakfast") {
        nextstage("fullenglish")
      } else if (input$choice == "Something Continental") {
        nextstage("continental")
      } else if (input$choice == "Porridge") {
        thoughts(thoughts() + 10) # Update thoughts
        nextstage("porridge")
      }
    } else if (currentstage() %in% c("cereal","coffeetoast","fullenglish","porridge")) {
      nextstage("travel")
    } else if (currentstage() == "travel") {
      # Choices for travel
      req(input$choice)
      if (input$choice == "Car") {
        nextstage("car")
      } else if (input$choice == "Train") {
        nextstage("train")
      }
    } else if (currentstage() == "train") {
      # Choices for train
      req(input$choice)
      if (input$choice == "Pet the horse") {
        # Stat check thoughts
        if (thoughts() >= 15) {
          nextstage("pet_success")
        } else {
          nextstage("pet_fail")
        }
      } else if (input$choice == "Look in to his eyes") {
        # Stat check thoughts
        if (thoughts() >= 15) {
          nextstage("eyes_success")
        } else {
          nextstage("eyes_fail")
        }
      } else if (input$choice == "To the trains") {
        nextstage("gtfohorace")
      }
    } else if (currentstage() %in% c("pet_fail","eyes_fail", "gtfohorace")) {
      # Train choice fails
      nextstage("station")
    } else if (currentstage() %in% c("pet_success","eyes_success")) {
      # Choices for riding
      req(input$choice)
      if (input$choice == "Accept the offer") {
        nextstage("rideyes")
      } else if (input$choice == "Decline the offer") {
        nextstage("rideno")
      }
    } else if (currentstage() == "car") {
      # Choices for car
      req(input$choice)
      if (input$choice == "Trust in GPS lady") {
        nextstage("radio")
      } else if (input$choice == "Risk it for a biscuit") {
        nextstage("biscuit")
      }
    } else if (currentstage() == "radio") {
      # Choices for radio
      req(input$choice)
      if (input$choice == "Yes") {
        nextstage("radioyes")
      } else if (input$choice == "No") {
        # Stat check alertness
        if (alertness() >= 100) {
          nextstage("radiono_good")
        } else {
          nextstage("radiono_bad")
        }
      }
    } else if (currentstage() == "right") {
      nextstage("right")
    } else if (currentstage() == "continental") {
      nextstage("continental")
    } else if (currentstage() == "rideyes") {
      nextstage("rideyes")
    } else if (currentstage() == "rideno") {
      nextstage("rideno")
    } else if (currentstage() == "station") {
      nextstage("station")
    } else if (currentstage() == "biscuit") {
      nextstage("biscuit")
    } else if (currentstage() == "radioyes") {
      nextstage("radioyes")
    } else if (currentstage() == "radiono_good") {
      nextstage("radiono_good")
    } else if (currentstage() == "radiono_bad") {
      nextstage("radiono_bad")
    }
    currentstage(nextstage()) # Move to next story stage
  })
  
  # Restart to start
  observeEvent(input$restart, {
    currentstage("start")
    nextstage("shower")
    alertness(0)
    thoughts(0)
  })
  
  # Displays selection UI when choices available
  output$choicedialog <- renderUI({
    if (currentstage() %in% c("shower","breakfast","travel","train","pet_success","eyes_success","car","radio")) {
      selectizeInput("choice", "Select your choice",
                     choices = c(selectionchoices()),
                     width = "250px",
                     options = list(placeholder = 'Please select an option below',
                                    onInitialize = I('function() { this.setValue(""); }'))
      )
    }
  })
  
  # Updates choices as story stage changes
  # Choice variables are vectors (not shown)
  selectionchoices <- reactive({
    if (currentstage() == "shower") {
      shower_choice
    } else if (currentstage() == "breakfast") {
      breakfast_choice
    } else if (currentstage() == "travel") {
      travel_choice
    } else if (currentstage() == "train") {
      train_choice
    } else if (currentstage() %in% c("pet_success","eyes_success")) {
      ride_choice
    } else if (currentstage() == "car") {
      car_choice
    } else if (currentstage() == "radio") {
      radio_choice
    } else {
      c("NA")
    }
  })
  
  # Display restart button on ending stages, otherwise next
  output$button <- renderUI({
    if (currentstage() %in% c("right","continental","rideyes","rideno","station","biscuit","radioyes","radiono_good","radiono_bad")) {
      actionButton("restart","Play again")
    } else {
      actionButton("go","Next")
    }
  })
  
  # Chooses the text to display given the story stage
  # Variables are text string (not shown)
  output$storytext <- renderText({
    if (currentstage() == "start") {
      "Press next to continue."
    } else if (currentstage() == "shower") {
      shower
    } else if (currentstage() == "hot") {
      hot
    } else if (currentstage() == "cold") {
      cold
    } else if (currentstage() == "right") {
      right
    } else if (currentstage() == "breakfast") {
      breakfast
    } else if (currentstage() == "cereal") {
      cereal
    } else if (currentstage() == "coffeetoast") {
      coffeetoast
    } else if (currentstage() == "fullenglish") {
      fullenglish
    } else if (currentstage() == "continental") {
      continental
    } else if (currentstage() == "porridge") {
      porridge
    } else if (currentstage() == "travel") {
      travel
    } else if (currentstage() == "train") {
      train
    } else if (currentstage() == "pet_success") {
      pet_success
    } else if (currentstage() == "pet_fail") {
      pet_fail
    } else if (currentstage() == "eyes_success") {
      eyes_success
    } else if (currentstage() == "eyes_fail") {
      eyes_fail
    } else if (currentstage() == "gtfohorace") {
      gtfohorace
    } else if (currentstage() == "rideyes") {
      rideyes
    } else if (currentstage() == "rideno") {
      rideno
    } else if (currentstage() == "station") {
      station
    } else if (currentstage() == "car") {
      car
    } else if (currentstage() == "biscuit") {
      biscuit
    } else if (currentstage() == "radio") {
      radio
    } else if (currentstage() == "radioyes") {
      radioyes
    } else if (currentstage() == "radiono_good") {
      radiono_good
    } else if (currentstage() == "radiono_bad") {
      radiono_bad
    } else {
      "Error, stage not found."
    }
  })
}
