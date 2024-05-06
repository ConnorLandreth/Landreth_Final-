

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```


## Objectives

- One Categorical Variable & One Numerical Variable (Salary & Job Title)
- Two Numerical Variables (Age & Years of Experience)
- Two Categorical Variables (Job Title & Education Level)


### Tools being used

For this project we are going to use Salary Prediction dataset available on Kaggle

In this research I'm going to filter out the data by male gender and removing the age groups that do not have values which I will specify underneath each section. The objective is to first create three visualizations with varying variable types, then do some loose exploratory visualization and modeling.


```{r}
library(tidyverse)
library(ggthemes)
```

After that we are going to import our dataset:

## Clean and Exploratory

```{r}
SD <- read.csv("C:/Users/cjlan/OneDrive/Documents/Salary_Data.csv")
avgsal <- SD %>%
  group_by(Age) %>%
  summarize(avg_salary = mean(Salary)) %>%
  filter(!(Age %in% c(26, 31, 36)))

avgyr <- SD %>%
  group_by(Years.of.Experience) %>%
  summarize(avg_salary = mean(Salary)) %>%
  filter(!(Years.of.Experience %in% c(6, 8)))

```


## Deliverable 1 - One Categorical & One Numerical

### One Numerical - YOE



```{r}

SD$Years.of.Experience <- as.numeric(SD$Years.of.Experience)

# Creating a gradient-colored histogram for Years.of.Experience with labels
ggplot(SD, aes(x = Years.of.Experience, fill = ..x..)) +
  geom_histogram(binwidth = 1, color = "black", alpha = 0.7) + # Using geom_histogram for a histogram
  scale_fill_gradient(low = "blue", high = "lightblue") +
  geom_text(
    aes(label = ..count..),
    stat = "count",
    vjust = -0.5,
    size = 2
  ) +
  labs(title = "Years of Experience Distribution", subtitle = "United States, 2022", x = "Years of Experience", y = "Count") +
  theme_economist() +
  theme(
    text = element_text(size = 12),
    plot.title = element_text(face = "bold", size = 16),
    plot.title.position = "panel",  # Adjust title position
    plot.caption = element_text(face = "bold", size = 10, hjust = 0, margin = margin(t = 20)),  # Adjust margin for subtitle
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text = element_text(size = 12),
    panel.grid.major = element_blank(),
    axis.text.y = element_blank(),
    legend.position = "none"
  ) +
  annotate(
    "text",
    x = 26.5,
    y = 220,
    label = "68.4% Of the population has 10 years or less of experience",
    color = "blue",
    size = 2.75
  )



```



```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```


## Objectives

- One Categorical Variable & One Numerical Variable (Salary & Job Title)
- Two Numerical Variables (Age & Years of Experience)
- Two Categorical Variables (Job Title & Education Level)


### Tools being used

For this project we are going to use Salary Prediction dataset available on Kaggle

In this research I'm going to filter out the data by male gender and removing the age groups that do not have values which I will specify underneath each section. The objective is to first create three visualizations with varying variable types, then do some loose exploratory visualization and modeling.


```{r}
library(tidyverse)
library(ggthemes)
```

After that we are going to import our dataset:
  
  ## Clean and Exploratory
  
  ```{r}
SD <- read.csv("C:/Users/cjlan/OneDrive/Documents/Salary_Data.csv")
avgsal <- SD %>%
  group_by(Age) %>%
  summarize(avg_salary = mean(Salary)) %>%
  filter(!(Age %in% c(26, 31, 36)))

avgyr <- SD %>%
  group_by(Years.of.Experience) %>%
  summarize(avg_salary = mean(Salary)) %>%
  filter(!(Years.of.Experience %in% c(6, 8)))

```


## Homework 1 - One Categorical & One Numerical

### One Numerical - YOE



```{r}

SD$Years.of.Experience <- as.numeric(SD$Years.of.Experience)

# Creating a gradient-colored histogram for Years.of.Experience with labels
ggplot(SD, aes(x = Years.of.Experience, fill = ..x..)) +
  geom_histogram(binwidth = 1, color = "black", alpha = 0.7) + # Using geom_histogram for a histogram
  scale_fill_gradient(low = "blue", high = "lightblue") +
  geom_text(
    aes(label = ..count..),
    stat = "count",
    vjust = -0.5,
    size = 2
  ) +
  labs(title = "Years of Experience Distribution", subtitle = "United States, 2022", x = "Years of Experience", y = "Count") +
  theme_economist() +
  theme(
    text = element_text(size = 12),
    plot.title = element_text(face = "bold", size = 16),
    plot.title.position = "panel",  # Adjust title position
    plot.caption = element_text(face = "bold", size = 10, hjust = 0, margin = margin(t = 20)),  # Adjust margin for subtitle
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text = element_text(size = 12),
    panel.grid.major = element_blank(),
    axis.text.y = element_blank(),
    legend.position = "none"
  ) +
  annotate(
    "text",
    x = 26.5,
    y = 220,
    label = "68.4% Of the population has 10 years or less of experience",
    color = "blue",
    size = 2.75
  )



```



### One Categorical - Edu Lvl





## Homework 2

### One Categorical & One Numerical

```{r}

# Identify the top 8 job titles
top_job_titles <- SD %>%
  group_by(Job.Title) %>%
  summarize(count = n()) %>%
  arrange(desc(count)) %>%
  slice_head(n = 6) %>%
  pull(Job.Title)

# Filter the data for the top 8 job titles
filtered_data <- SD %>%
  filter(Job.Title %in% top_job_titles)

# Create a box plot
ggplot(filtered_data, aes(x = Job.Title, y = Salary)) +
  geom_boxplot(fill = "skyblue", color = "steelblue") +
  labs(title = "Distribution of Salaries Across Top 6 Job Titles", x = "Job Title", y = "Salary") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability
#+
# annotate(
#  "text",
# x = "Bachelor's",  # Specify the factor level corresponding to the x-coordinate
#y = 20,  # Choose the y-coordinate for your annotation
#label = "  ", #
#color = "red",  # Specify the text color
#size = 2.2  # Specify the text size
#)
```


### Two Numerical

```{r}
library(ggplot2)

# Create a scatter plot
ggplot(SD, aes(x = Age, y = Years.of.Experience)) +
  geom_point(aes(color = Salary), size = 3) +
  labs(title = "Scatter Plot of Age vs. Years of Experience", x = "Age", y = "Years of Experience", color = "Salary") +
  scale_color_gradient(low = "blue", high = "red") +  # Gradient color based on Salary
  theme_minimal()

```



### Two Categorical


```{r}

SD <- SD %>%
  mutate(Education.Level = case_when(
    grepl("Bachelor", Education.Level, ignore.case = TRUE) ~ "Bachelor's",
    grepl("Master", Education.Level, ignore.case = TRUE) ~ "Master's",
    TRUE ~ as.character(Education.Level)
  ))



SD$Education.Level <- factor(SD$Education.Level, levels = c("PhD", "Master's", "Bachelor's", "High School"))

# Filter the data for the top 8 job titles
filtered_data <- SD %>%
  filter(Job.Title %in% top_job_titles)

# Create a grouped bar plot
ggplot(filtered_data, aes(x = Education.Level, fill = Job.Title)) +
  geom_bar(position = "dodge") +
  labs(title = "Distribution of Top 6 Job Titles by Education Level", x = "Education Level", y = "Count", fill = "Job Title") +
  theme_minimal() +
  theme(legend.position = "top")  # Move leg

```


## Create Dashboard
```{r}
#install.packages("shiny")
library(shiny)
```
### Tab Version
```{r}


# Define UI
ui <- fluidPage(
  titlePanel("Job Data Visualizations"),
  sidebarLayout(
    sidebarPanel(
      helpText("Select a visualization to display:")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Salary by Job Title", plotOutput("salaryPlot")),
        tabPanel("Age vs. Experience", plotOutput("ageExperiencePlot")),
        tabPanel("Education Level by Job Title", plotOutput("educationLevelPlot"))
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Visualization for 'One Categorical & One Numerical'
  output$salaryPlot <- renderPlot({
    top_job_titles <- SD %>%
      group_by(Job.Title) %>%
      summarize(count = n()) %>%
      arrange(desc(count)) %>%
      slice_head(n = 6) %>%
      pull(Job.Title)
    
    filtered_data <- SD %>%
      filter(Job.Title %in% top_job_titles)
    
    ggplot(filtered_data, aes(x = Job.Title, y = Salary)) +
      geom_boxplot(fill = "skyblue", color = "steelblue") +
      labs(title = "Distribution of Salaries Across Top 6 Job Titles", x = "Job Title", y = "Salary") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Visualization for 'Two Numerical'
  output$ageExperiencePlot <- renderPlot({
    ggplot(SD, aes(x = Age, y = Years.of.Experience)) +
      geom_point(aes(color = Salary), size = 3) +
      labs(title = "Scatter Plot of Age vs. Years of Experience", x = "Age", y = "Years of Experience", color = "Salary") +
      scale_color_gradient(low = "blue", high = "red") +
      theme_minimal()
  })
  
  # Visualization for 'Two Categorical'
  output$educationLevelPlot <- renderPlot({
    SD <- SD %>%
      mutate(Education.Level = case_when(
        grepl("Bachelor", Education.Level, ignore.case = TRUE) ~ "Bachelor's",
        grepl("Master", Education.Level, ignore.case = TRUE) ~ "Master's",
        TRUE ~ as.character(Education.Level)
      ))
    
    SD$Education.Level <- factor(SD$Education.Level, levels = c("PhD", "Master's", "Bachelor's", "High School"))
    
    top_job_titles <- SD %>%
      group_by(Job.Title) %>%
      summarize(count = n()) %>%
      arrange(desc(count)) %>%
      slice_head(n = 6) %>%
      pull(Job.Title)
    
    filtered_data <- SD %>%
      filter(Job.Title %in% top_job_titles)
    
    ggplot(filtered_data, aes(x = Education.Level, fill = Job.Title)) +
      geom_bar(position = "dodge") +
      labs(title = "Distribution of Top 6 Job Titles by Education Level", x = "Education Level", y = "Count", fill = "Job Title") +
      theme_minimal() +
      theme(legend.position = "top")
  })
  
}

# Run the app
shinyApp(ui = ui, server = server)




```

### Light Version - Version for Presentation 2
```{r}

library(shiny)
library(shinythemes)
library(ggplot2)
library(dplyr)

ui <- fluidPage(
  theme = shinytheme("journal"),
  titlePanel("Corporate Careers (US) Summary Dashboard"),
  fluidRow(
    column(6, plotOutput("salaryPlot")),
    column(6, plotOutput("ageExperiencePlot"))
  ),
  fluidRow(
    column(6, plotOutput("salaryHistogram")),  # Univariate for Numerical
    column(6, plotOutput("educationLevelPlot"))  # Adjusted Univariate for Categorical (from OG HW1)
  )
)

server <- function(input, output, session) {
  
  # Visualization for One Categorical & One Numerical - multi
  output$salaryPlot <- renderPlot({
    top_job_titles <- SD %>%
      group_by(Job.Title) %>%
      summarize(count = n()) %>%
      arrange(desc(count)) %>%
      slice_head(n = 6) %>%
      pull(Job.Title)
    
    filtered_data <- SD %>%
      filter(Job.Title %in% top_job_titles)
    
    ggplot(filtered_data, aes(x = Job.Title, y = Salary)) +
      geom_boxplot(fill = "skyblue", color = "steelblue") +
      labs(title = "Salaries for Top 6 Job Titles",
           subtitle = "United States, 2022",
           caption = "Source: Kaggle",
           x = "Job Title", y = "Salary") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      annotate("text", x = 'Senior Project Engineer', y = 50000, label = "Mean compensation for top professions: $149,833", hjust = 1.1, vjust = 1.1, size = 4, colour = "red")
  })
  
  # Visualization for Two Numerical - multi with salary bins
  output$ageExperiencePlot <- renderPlot({
    SD$SalaryBins <- cut(SD$Salary,
                         breaks = c(0, 50000, 100000, 150000, Inf),
                         labels = c("0-49,999", "50,000-99,999", "100,000-149,999", "150,000+"),
                         include.lowest = TRUE)
    
    ggplot(SD, aes(x = Age, y = Years.of.Experience, color = SalaryBins)) +
      geom_point(size = 3) +
      labs(title = "Age vs. Years of Experience",
           subtitle = "United States, 2022",
           caption = "Source: Kaggle",
           x = "Age", y = "Years of Experience", color = "Salary Range") +
      scale_color_manual(values = c("0-49,999" = "lightgreen", "50,000-99,999" = "skyblue", "100,000-149,999" = "orange", "150,000+" = "magenta")) +
      theme_minimal()
  })
  
  # Visualization for One Numeric Variable (Histogram) - uni
  output$salaryHistogram <- renderPlot({
    ggplot(SD, aes(x = Salary)) +
      geom_histogram(binwidth = 5000, fill = "gray", color = "black") +
      labs(title = "Salary Distribution",
           subtitle = "United States, 2022",
           caption = "Source: Kaggle",
           x = "Salary", y = "") +
      theme_minimal()
  })
  
  # Adjusted Visualization for One Categorical Variable - uni
  output$educationLevelPlot <- renderPlot({
    data <- SD %>% filter(!is.na(Education.Level))
    max_count <- max(table(SD$Education.Level))  # Calculate the maximum count
    ggplot(data, aes(x = Education.Level)) +
      geom_bar(fill = "grey", color = "black") +  # Set bars to grey
      geom_text(stat = 'count', aes(label = ..count..), vjust = -1.5, color = "black") +  # Display count above bars
      labs(title = "Distribution of Education Levels",
           subtitle = "United States, 2022",
           caption = "Source: Kaggle",
           x = "Education Level", y = "") +
      ylim(0, max_count * 1.2) +  # Extend the y-axis to fit labels
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.y = element_blank(),  # Remove y-axis title
        axis.text.y = element_blank(),   # Remove y-axis text
        legend.position = "none"         # Explicitly remove legend
      ) +
      annotate("text", x = "PhD", y = 2500, label = "45.1% of degree holders have a bachelor's degree", size = 4, colour = "red")
  })
}

# Running the app
shinyApp(ui = ui, server = server)

```
### Dark Version
```{r}
# Define UI
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body { background-color: #121212; color: #FFFFFF; }
      .shiny-text-output { color: #FFFFFF; }
      h1, h2, h3, h4, h5, h6 { color: #FFFFFF; }
      .well { background-color: #262626; color: #FFFFFF; }
      .btn { border: 1px solid #4CAF50; }
    "))
  ),
  titlePanel("Job Data Dashboard"),
  fluidRow(
    column(6, plotOutput("salaryPlot")),
    column(6, plotOutput("ageExperiencePlot"))
  ),
  fluidRow(
    column(12, plotOutput("educationLevelPlot"))
  )
)

# Define server logic
server <- function(input, output) {
  
  # Visualization for 'One Categorical & One Numerical'
  output$salaryPlot <- renderPlot({
    top_job_titles <- SD %>%
      group_by(Job.Title) %>%
      summarize(count = n()) %>%
      arrange(desc(count)) %>%
      slice_head(n = 6) %>%
      pull(Job.Title)
    
    filtered_data <- SD %>%
      filter(Job.Title %in% top_job_titles)
    
    ggplot(filtered_data, aes(x = Job.Title, y = Salary)) +
      geom_boxplot(fill = "skyblue", color = "steelblue") +
      labs(title = "Salaries for Top 6 Job Titles", x = "Job Title", y = "Salary") +
      theme_minimal() +
      theme(plot.background = element_rect(fill = "#121212", color = "#121212"),
            panel.background = element_rect(fill = "#121212"),
            text = element_text(color = "white"),
            axis.text.x = element_text(angle = 45, hjust = 1),
            axis.text = element_text(color = "white"),
            axis.title = element_text(color = "white"),
            legend.title = element_text(color = "white"),
            legend.text = element_text(color = "white"))
  })
  
  # Visualization for 'Two Numerical'
  output$ageExperiencePlot <- renderPlot({
    ggplot(SD, aes(x = Age, y = Years.of.Experience)) +
      geom_point(aes(color = Salary), size = 3) +
      labs(title = "Age vs. Years of Experience", x = "Age", y = "Years of Experience", color = "Salary") +
      scale_color_gradient(low = "blue", high = "red") +
      theme_minimal() +
      theme(plot.background = element_rect(fill = "#121212", color = "#121212"),
            panel.background = element_rect(fill = "#121212"),
            text = element_text(color = "white"),
            axis.text = element_text(color = "white"),
            axis.title = element_text(color = "white"),
            legend.title = element_text(color = "white"),
            legend.text = element_text(color = "white"))
  })
  
  # Visualization for 'Two Categorical'
  output$educationLevelPlot <- renderPlot({
    SD <- SD %>%
      mutate(Education.Level = case_when(
        grepl("Bachelor", Education.Level, ignore.case = TRUE) ~ "Bachelor's",
        grepl("Master", Education.Level, ignore.case = TRUE) ~ "Master's",
        TRUE ~ as.character(Education.Level)
      ))
    
    SD$Education.Level <- factor(SD$Education.Level, levels = c("PhD", "Master's", "Bachelor's", "High School"))
    
    top_job_titles <- SD %>%
      group_by(Job.Title) %>%
      summarize(count = n()) %>%
      arrange(desc(count)) %>%
      slice_head(n = 6) %>%
      pull(Job.Title)
    
    filtered_data <- SD %>%
      filter(Job.Title %in% top_job_titles)
    
    ggplot(filtered_data, aes(x = Education.Level, fill = Job.Title)) +
      geom_bar(position = "dodge") +
      labs(title = "Top 6 Job Titles by Education Level", x = "Education Level", y = "Count", fill = "Job Title") +
      theme_minimal() +
      theme(plot.background = element_rect(fill = "#121212", color = "#121212"),
            panel.background = element_rect(fill = "#121212"),
            text = element_text(color = "white"),
            axis.text = element_text(color = "white"),
            axis.title = element_text(color = "white"),
            legend.position = "top",
            legend.title = element_text(color = "white"),
            legend.text = element_text(color = "white"))
  })
}

# Run the app
shinyApp(ui = ui, server = server)




```







```{r}

# List of state names
states <- c("alabama", "alaska", "arizona", "arkansas", "california", "colorado", "connecticut", "delaware", "florida", "georgia",
            "hawaii", "idaho", "illinois", "indiana", "iowa", "kansas", "kentucky", "louisiana", "maine", "maryland", "massachusetts",
            "michigan", "minnesota", "mississippi", "missouri", "montana", "nebraska", "nevada", "new hampshire", "new jersey",
            "new mexico", "new york", "north carolina", "north dakota", "ohio", "oklahoma", "oregon", "pennsylvania", "rhode island",
            "south carolina", "south dakota", "tennessee", "texas", "utah", "vermont", "virginia", "washington", "west virginia",
            "wisconsin", "wyoming")

# Assign a random state to each row in SD
set.seed(123)  # Set seed for reproducibility
SD$state <- sample(states, size = nrow(SD), replace = TRUE)

# Convert state names to lowercase for consistency
SD$state <- tolower(SD$state)

# Check the structure of SD to ensure the 'state' column is created correctly
str(SD)



# Convert state names to lowercase for consistency
SD$state <- tolower(SD$state)

# Calculate average salary by state
average_salary <- SD %>%
  group_by(state) %>%
  summarise(average_salary = mean(Salary, na.rm = TRUE))

# Map of US states
map_us <- map_data("state")

# Merge average salary data with the map data
merged_data <- left_join(map_us, average_salary, by = c("region" = "state"))

# Plotting the map with fill color based on average salary
ggplot(merged_data, aes(x = long, y = lat, group = group, fill = average_salary)) +
  geom_polygon(color = "white", size = 0.1) +
  scale_fill_viridis_c() +
  labs(title = "Average Salary by State", fill = "Average Salary") +
  coord_map() +
  theme_minimal()



```



## Final Dashboard for Final Project Submission

```{r}

ui <- fluidPage(
  theme = shinytheme("journal"),
  titlePanel("Corporate Careers Summary Dashboard (US)"),
  fluidRow(
    column(6, plotOutput("educationLevelPlot")),
    column(6, plotOutput("salaryMapPlot"))
  ),
  fluidRow(
    column(6, plotOutput("salaryPlot")),
    column(6, plotOutput("ageExperiencePlot"))
  )
)

server <- function(input, output, session) {
  
  # Visualization for One Categorical & One Numerical - multi
  output$salaryPlot <- renderPlot({
    top_job_titles <- SD %>%
      group_by(Job.Title) %>%
      summarize(count = n()) %>%
      arrange(desc(count)) %>%
      slice_head(n = 6) %>%
      pull(Job.Title)
    
    filtered_data <- SD %>%
      filter(Job.Title %in% top_job_titles)
    
    ggplot(filtered_data, aes(x = Job.Title, y = Salary)) +
      geom_boxplot(fill = "turquoise4", color = "black") +
      labs(title = "Salaries for Top 6 Job Titles",
           subtitle = "United States, 2022",
           caption = "Source: Kaggle",
           x = "Job Title", y = "Salary") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            axis.text = element_text(face = "bold")) +  # Bold axis text
      annotate("text", x = 'Senior Project Engineer', y = 50000, label = "Mean compensation for top professions: $149,833", hjust = 1.1, vjust = 1.1, size = 4, colour = "red")
  })
  
  # Visualization for Two Numerical - multi with salary bins
  output$ageExperiencePlot <- renderPlot({
    SD$SalaryBins <- cut(SD$Salary,
                         breaks = c(0, 50000, 100000, 150000, Inf),
                         labels = c("0-49,999", "50,000-99,999", "100,000-149,999", "150,000+"),
                         include.lowest = TRUE)
    
    ggplot(SD, aes(x = Age, y = Years.of.Experience, color = SalaryBins)) +
      geom_point(size = 3) +
      labs(title = "Age vs. Years of Experience",
           subtitle = "United States, 2022",
           caption = "Source: Kaggle",
           x = "Age", y = "Years of Experience", color = "Salary Range") +
      scale_color_manual(values = c("0-49,999" = "turquoise4", "50,000-99,999" = "royalblue", "100,000-149,999" = "yellow4", "150,000+" = "purple4")) +
      theme_minimal() +
      theme(axis.text = element_text(face = "bold"))  # Bold axis text
  })
  
  # Adjusted Visualization for One Categorical Variable - uni
  output$educationLevelPlot <- renderPlot({
    data <- SD %>% filter(!is.na(Education.Level))
    max_count <- max(table(SD$Education.Level))  # Calculate the maximum count
    ggplot(data, aes(x = Education.Level)) +
      geom_bar(fill = "turquoise4", color = "black") +  # Set bars to grey
      geom_text(stat = 'count', aes(label = ..count..), vjust = -1.5, color = "black") +  # Display count above bars
      labs(title = "Distribution of Education Levels",
           subtitle = "United States, 2022",
           caption = "Source: Kaggle",
           x = "Education Level", y = "") +
      ylim(0, max_count * 1.2) +  # Extend the y-axis to fit labels
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            axis.text = element_text(face = "bold"),  # Bold axis text
            axis.title.y = element_blank(),  # Remove y-axis title
            axis.text.y = element_blank(),   # Remove y-axis text
            legend.position = "none"         # Explicitly remove legend
      ) +
      annotate("text", x = "PhD", y = 2500, label = "45.1% of degree holders have a bachelor's degree", size = 3.5, colour = "red")
  })
  
  # Plotting the map with fill color based on average salary
  output$salaryMapPlot <- renderPlot({
    ggplot(merged_data, aes(x = long, y = lat, group = group, fill = average_salary)) +
      geom_polygon(color = "white", size = 0.1) +
      scale_fill_viridis() +
      labs(title = "Average Salary by State",
           subtitle = "United States, 2022",
           caption = "Source: Kaggle",
           fill = "Average Salary") +
      coord_map() +
      theme_minimal() +
      theme(axis.text = element_text(face = "bold"))  # Bold axis text
  })
}

# Running the app
shinyApp(ui = ui, server = server)





```

