
library(shiny)
library(shinydashboard)


shinyUI(
    dashboardPage(skin='black',

    # Application title
    dashboardHeader(title="2019 Fundraising"),#Closes Header

    dashboardSidebar(
            sidebarMenu(
                menuItem("About", tabName='about', icon=icon('address-card')),
                menuItem("Total Amount Raised", tabName='total_amount', icon=icon('money-bill-alt')),
                menuItem("Unique Donors", tabName='unique_donors', icon=icon('user')),
                menuItem("Average Amount per Donor", tabName='avg_donor', icon=icon('wallet')),
                menuItem("Search Donor List", tabName='query', icon=icon('search')),
                br(),
                selectInput('pick_state', 'Select an Obama-Trump State', choice_state, selected='Florida')
            )#Closes Menu
    ),#Closes Sidebar

    #selectizeInput('id, needs to be unique', 'text that will show up', choice)

    dashboardBody(
        tabItems(
         
            tabItem(
                tabName='about',
                box(h3("Hello World"),
                    br(),
                    tags$p("My name is Jordan Runge, and I created this dashboard. I'm currently a Data Science
                            Fellow at NYC Data Science Academy, and I'm interested in applying data science to the worlds
                            of politics and public policy."),
                    
                    tags$p("While one day I'd love to write algorithms that help campaigns target voters, this dashboard is meant to serve as a lightweight
                           tool to help campaigns visualize fundraising outcomes based on FEC filings. For the sake of the tool's
                            performance, I've limited the data being analyzed to a single candidate (Pete Buttigieg), over one a one-year timespan (2019),
                            in six states (the 'Obama-Trump States'). I've chosen the candidate Pete Buttigieg because he has emerged as a suprisingly formidable fundraiser,
                            and I've selected the 'Obama-Trump States' because they will likely play a large role in determining the winner of the 2020 presidential election."),

                    br(),
                    tags$p("The original data used in this dashboard can be found", tags$a(href="https://www.fec.gov/data/browse-data/?tab=bulk-data", target="_blank", "here"),"."),
                    tags$p("Source code for this project can be found", tags$a(href="https://github.com/jordanrunge/sanders_vs_buttigieg", target = "_blank", "here"),"."),
                    tags$p("Connect with Jordan on ", tags$a(href="https://www.linkedin.com/in/jordanrunge/", target = "_blank", "LinkedIn"),"."))
            ),
        

           tabItem(
               tabName='total_amount',
               fluidRow(
                   box(title = "Total Funds Raised by Zip Code", plotOutput(outputId='p_map_total_amount')),
                   box(title = "Top 10 Zip Codes: Total Funds Raised", plotOutput(outputId='p_total_amount_bar'))),
               fluidRow(
                   box(title = "Total Raised Statewide", width=12, plotOutput(outputId='state_total_amount')))
           ),


           tabItem(
               tabName='unique_donors',
               fluidRow(
                    box(title = "Unique Donors by Zip Code", plotOutput(outputId='p_map_unique_donors')),
                    box(title = "Top 10 Zip Codes: Unique Donors", plotOutput(outputId='p_unique_donors_bar'))),
               fluidRow(
                   box(title = "Unique Donors Statewide", width=12, plotOutput(outputId='state_unique_donors')))
           ),


           tabItem(
               tabName='avg_donor',
               fluidRow(
                    box(title = "Average Amount per Donor by Zip Code", plotOutput(outputId='p_map_avg_donor')),
                    box(title = "Top 10 Zip Codes: Avg Amount per Donor", plotOutput(outputId='p_avg_donor_bar'))),
               fluidRow(
                   box(title = "Average per Donor Statewide", width=12, plotOutput(outputId='state_avg_donor')))
            ),
          
                
           tabItem(
               tabName='query',
               DT::dataTableOutput('query')
           )
           

        )#Closes tabItems
    )#Closes Body
)#Closes Page
)#Closes UI
    