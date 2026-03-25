# ============================================================================
# E-COMMERCE WHAT-IF ANALYSIS DASHBOARD
# ============================================================================
# This script performs data cleaning, exploratory analysis, and creates an
# interactive Shiny dashboard for scenario planning and revenue impact analysis
# ============================================================================

# --- SECTION 1: LOAD REQUIRED LIBRARIES ---
library(dplyr) # For data manipulation and piping
library(ggplot2) # For creating visualizations
library(shiny) # For interactive web dashboard
library(bslib) # For Bootstrap theming in Shiny
library(scales) # For formatting labels (currency, percentages)
library(DT) # For interactive data tables

# ============================================================================
# PART 1: DATA LOADING & INITIAL EXPLORATION
# ============================================================================

# Load the CSV file into memory
data <- read.csv("db.csv")

# Print data structure (column names, types, and sample values)
print(str(data))

# Print summary statistics for all columns
print(summary(data))

# Display first 6 rows of data
print(head(data))

# Display last 6 rows of data
print(tail(data))

# Print dimensions (number of rows and columns)
print(dim(data))

# Print all column names
print(names(data))

# ============================================================================
# PART 2: DATA CLEANING & STANDARDIZATION
# ============================================================================

# Standardize Gender values: convert "Men" to "M"
data$Gender[data$Gender == "Men"] <- "M"

# Standardize Gender values: convert "Women" to "W"
data$Gender[data$Gender == "Women"] <- "W"

# Convert all Category values to lowercase for consistent matching
data$Category <- tolower(data$Category)

# Standardize Quantity: convert text "One" to numeric 1
data$Qty[data$Qty == "One"] <- 1

# Standardize Quantity: convert text "Two" to numeric 2
data$Qty[data$Qty == "Two"] <- 2

# Standardize Quantity: convert string "1" to numeric 1
data$Qty[data$Qty == "1"] <- 1

# Standardize Quantity: convert string "2" to numeric 2
data$Qty[data$Qty == "2"] <- 2

# Convert entire Qty column to numeric type
data$Qty <- as.numeric(data$Qty)

# Print updated data structure to verify changes
print(str(data))

# Print unique Gender values to confirm standardization
print(unique(data$Gender))

# ============================================================================
# PART 3: STATIC VISUALIZATIONS (Pre-Dashboard Analysis)
# ============================================================================

# --- VISUALIZATION 1: Revenue per Gender (Bar Chart) ---
# Calculate total revenue grouped by Gender
gender_sums <- tapply(data$Amount, data$Gender, sum)

# Create bar plot showing revenue by gender
barplot(
    gender_sums,
    col = "skyblue", # Light blue bar color
    main = "Revenue per Gender", # Chart title
    xlab = "Gender", # X-axis label
    ylab = "Amount" # Y-axis label
)

# --- VISUALIZATION 2: Top 10 States (Pie Chart) ---
# Calculate total revenue grouped by State, removing NA values
state_sums <- tapply(data$Amount, data$ship.state, sum, na.rm = TRUE)

# Sort states by revenue in descending order and keep top 10
top_10_states <- head(sort(state_sums, decreasing = TRUE), 10)

# Create pie chart with 10 rainbow colors
pie(
    top_10_states,
    col = rainbow(10), # Use 10 distinct rainbow colors
    main = "Top 10 States by Revenue" # Chart title
)

# --- VISUALIZATION 3: Revenue by Category (Bar Chart) ---
# Calculate total revenue grouped by Category
category_sums <- tapply(data$Amount, data$Category, sum)

# Create bar plot showing revenue by product category
barplot(
    category_sums,
    col = "skyblue", # Light blue bar color
    main = "Revenue by Category", # Chart title
    xlab = "Category", # X-axis label
    ylab = "Amount" # Y-axis label
)

# ============================================================================
# PART 4: ADVANCED VISUALIZATION - WATERFALL CHART
# ============================================================================

# Group data by Channel and sum revenues, then sort by descending revenue
channel_rev <- data %>%
    group_by(Channel) %>%
    summarise(Revenue = sum(Amount, na.rm = TRUE)) %>%
    arrange(desc(Revenue))

# Create cumulative sum column (end point of each bar in waterfall)
channel_rev$end <- cumsum(channel_rev$Revenue)

# Create starting point for each bar (previous bar's end point)
channel_rev$start <- c(0, head(channel_rev$end, -1))

# Convert Channel to factor with order matching the sorted revenue
channel_rev$Channel <- factor(
    channel_rev$Channel,
    levels = channel_rev$Channel
)

# Create a "Total" row for the waterfall chart
total_row <- data.frame(
    Channel = "Total", # Row label
    Revenue = sum(channel_rev$Revenue), # Sum of all channel revenues
    end = sum(channel_rev$Revenue), # End point equals total
    start = 0 # Start from zero
)

# Combine channel data and total row
waterfall_df <- rbind(channel_rev, total_row)

# Ensure Channel factor includes the "Total" level
waterfall_df$Channel <- factor(
    waterfall_df$Channel,
    levels = waterfall_df$Channel
)

# Add type column: "total" for the total row, "channel" for individual channels
waterfall_df$type <- ifelse(
    waterfall_df$Channel == "Total",
    "total",
    "channel"
)

# Create waterfall chart using ggplot2
ggplot(waterfall_df, aes(x = Channel)) +
    # Draw rectangles (bars) from start to end point
    geom_rect(aes(
        xmin = as.numeric(Channel) - 0.4, # Left edge of bar
        xmax = as.numeric(Channel) + 0.4, # Right edge of bar
        ymin = start, # Bottom of bar
        ymax = end, # Top of bar
        fill = type # Color by type (channel vs total)
    )) +
    # Add revenue labels on top of bars
    geom_text(
        aes(
            y = end,
            label = paste0(round(Revenue / 1e5, 1), "L") # Display in Lakhs (100,000)
        ),
        vjust = -0.5, # Vertical adjustment above bar
        size = 3.2 # Text size
    ) +
    # Define colors: blue for channels, green for total
    scale_fill_manual(values = c(
        "channel" = "#4A90D9",
        "total"   = "#2ECC71"
    )) +
    # Add chart titles and labels
    labs(
        title = "Revenue Waterfall by Channel",
        x = "Channel",
        y = "Revenue"
    ) +
    # Apply minimal theme
    theme_minimal() +
    # Customize appearance
    theme(
        legend.position = "none", # Hide legend
        axis.text.x = element_text(angle = 45, hjust = 1), # Rotate x-axis labels
        plot.title = element_text(face = "bold", hjust = 0.5) # Bold, centered title
    )

# ============================================================================
# PART 5: WHAT-IF SCENARIO ANALYSIS
# ============================================================================

# Calculate baseline total revenue (before any scenarios)
baseline_total <- sum(data$Amount)

# --- SCENARIO A: 10% Increase in Amazon Channel Sales ---
# Create new data with Amazon sales multiplied by 1.10
data_amazon <- data %>%
    mutate(
        Scenario_Amount = if_else(
            Channel == "Amazon", # If channel is Amazon
            Amount * 1.10, # Increase by 10%
            Amount # Otherwise keep original
        )
    )

# Calculate revenue impact (difference from baseline)
impact_a <- sum(data_amazon$Scenario_Amount) - baseline_total

# --- SCENARIO B: 15% Increase in Maharashtra State Sales ---
# Create new data with Maharashtra sales multiplied by 1.15
data_mh <- data %>%
    mutate(
        Scenario_Amount = if_else(
            ship.state == "MAHARASHTRA", # If state is Maharashtra
            Amount * 1.15, # Increase by 15%
            Amount # Otherwise keep original
        )
    )

# Calculate revenue impact
impact_b <- sum(data_mh$Scenario_Amount) - baseline_total

# --- SCENARIO C: 15% Price Discount + 35% Volume Increase for Kurtas ---
# Math: (Price * 0.85) * (Quantity * 1.35) = Amount * 0.85 * 1.35 = Amount * 1.1475
data_kurta <- data %>%
    mutate(
        Scenario_Amount = if_else(
            Category == "kurta", # If category is kurta
            Amount * 1.1475, # Apply combined discount and volume increase
            Amount # Otherwise keep original
        )
    )

# Calculate revenue impact
impact_c <- sum(data_kurta$Scenario_Amount) - baseline_total

# --- PRINT SCENARIO RESULTS ---
# Display baseline revenue
cat("Baseline Revenue:", baseline_total, "\n")

# Display impact of Scenario A
cat("Impact A (Amazon +10%):", impact_a, "\n")

# Display impact of Scenario B
cat("Impact B (MH +15%):", impact_b, "\n")

# Display impact of Scenario C
cat("Impact C (Kurta Promo):", impact_c, "\n")

# ============================================================================
# PART 6: INTERACTIVE SHINY DASHBOARD
# ============================================================================

# --- SUBSECTION 6.1: DATA PREPARATION FOR DASHBOARD ---

# Load raw data again for dashboard (with stringsAsFactors = FALSE for compatibility)
raw_data <- read.csv("db.csv", stringsAsFactors = FALSE)

# Standardize Gender: "Men" -> "M"
raw_data$Gender[raw_data$Gender == "Men"] <- "M"

# Standardize Gender: "Women" -> "W"
raw_data$Gender[raw_data$Gender == "Women"] <- "W"

# Convert categories to lowercase for consistent matching
raw_data$Category <- tolower(raw_data$Category)

# Standardize Quantity: "One" -> 1
raw_data$Qty[raw_data$Qty == "One"] <- 1

# Standardize Quantity: "Two" -> 2
raw_data$Qty[raw_data$Qty == "Two"] <- 2

# Convert Quantity column to numeric
raw_data$Qty <- as.numeric(raw_data$Qty)

# Convert Amount column to numeric
raw_data$Amount <- as.numeric(raw_data$Amount)

# Convert Age column to integer
raw_data$Age <- as.integer(raw_data$Age)

# Create age groups from Age column using cut function
raw_data$Age.Group <- cut(
    raw_data$Age,
    breaks = c(0, 18, 25, 35, 45, 55, 65, Inf), # Age breakpoints
    labels = c("0-18", "19-25", "26-35", "36-45", "46-55", "56-65", "65+"),
    right = TRUE # Include right boundary in intervals
)

# Extract unique channels and sort alphabetically for dropdown
all_channels <- sort(unique(raw_data$Channel))

# Extract unique states and sort alphabetically for dropdown
all_states <- sort(unique(raw_data$ship.state))

# Extract unique categories and sort alphabetically for dropdown
all_categories <- sort(unique(raw_data$Category))

# Extract age group levels
all_age_groups <- levels(raw_data$Age.Group)

# Calculate baseline total revenue across all data
baseline_total <- sum(raw_data$Amount, na.rm = TRUE)

# --- SUBSECTION 6.2: HELPER FUNCTIONS FOR FORMATTING ---

# Create vectorized function to format numbers as Indian Rupees in Lakhs (100,000)
fmt_lakh <- Vectorize(function(x) {
    # Check if value is NULL, NA, or empty
    if (is.null(x) || is.na(x) || length(x) == 0) {
        return("₹0.00 L")
    }

    # Convert to numeric, divide by 100,000, format with commas, add rupee symbol
    paste0("₹", format(round(as.numeric(x) / 1e5, 2), big.mark = ","), " L")
})

# Create vectorized function to format numbers as Indian Rupees
fmt_inr <- Vectorize(function(x) {
    # Check if value is NULL, NA, or empty
    if (is.null(x) || is.na(x) || length(x) == 0) {
        return("₹0")
    }

    # Convert to numeric, round to nearest integer, format with commas
    paste0("₹", format(round(as.numeric(x)), big.mark = ","))
})

# ============================================================================
# PART 7: SHINY UI LAYOUT
# ============================================================================

# Create page with sidebar layout and define title, theme
ui <- page_sidebar(
    title = "Clothes Store Dashboard",


    # Define Bootstrap theme with custom colors
    theme = bs_theme(
        version = 5, # Bootstrap version 5
        bootswatch = "flatly", # Flatly theme preset
        primary = "#4A90D9", # Primary blue color
        "navbar-bg" = "#2C3E50" # Dark navbar background
    ),

    # --- SIDEBAR PANEL ---
    sidebar = sidebar(
        width = 320, # Sidebar width in pixels
        open = "desktop", # Open by default on desktop
        title = "Scenario Controls",

        # Create accordion (collapsible sections) for scenario inputs
        accordion(
            open = TRUE, # First section open by default

            # --- ACCORDION PANEL 1: Channel Boost ---
            accordion_panel(
                "Channel Boost",
                icon = icon("shop"), # Shopping icon

                # Dropdown to select specific channel or "All"
                selectInput(
                    "channel_select",
                    "Select Channel",
                    choices = c("All", all_channels),
                    selected = "All"
                ),

                # Slider to adjust revenue change percentage (-50% to +100%)
                sliderInput(
                    "channel_pct",
                    "Revenue Change (%)",
                    min = -50,
                    max = 100,
                    value = 0,
                    step = 1,
                    post = "%"
                )
            ),

            # --- ACCORDION PANEL 2: State Boost ---
            accordion_panel(
                "State Boost",
                icon = icon("map-location-dot"), # Map icon

                # Dropdown to select specific state or "All"
                selectInput(
                    "state_select",
                    "Select State",
                    choices = c("All", all_states),
                    selected = "All"
                ),

                # Slider to adjust revenue change percentage
                sliderInput(
                    "state_pct",
                    "Revenue Change (%)",
                    min = -50,
                    max = 100,
                    value = 0,
                    step = 1,
                    post = "%"
                )
            ),

            # --- ACCORDION PANEL 3: Category Promotion ---
            accordion_panel(
                "Category Promo",
                icon = icon("tags"), # Tags icon

                # Dropdown to select specific category or "All"
                selectInput(
                    "cat_select",
                    "Select Category",
                    choices = c("All", all_categories),
                    selected = "All"
                ),

                # Slider for price discount (0% to 50%)
                sliderInput(
                    "cat_discount",
                    "Price Discount (%)",
                    min = 0,
                    max = 50,
                    value = 0,
                    step = 1,
                    post = "%"
                ),

                # Slider for volume increase (0% to 100%)
                sliderInput(
                    "cat_volume",
                    "Volume Increase (%)",
                    min = 0,
                    max = 100,
                    value = 0,
                    step = 1,
                    post = "%"
                )
            ),

            # --- ACCORDION PANEL 4: Data Filters ---
            accordion_panel(
                "Filters",
                icon = icon("filter"), # Filter icon

                # Checkboxes to select which age groups to include
                checkboxGroupInput(
                    "filter_age",
                    "Age Groups",
                    choices = all_age_groups,
                    selected = all_age_groups
                ),

                # Dropdown to filter by gender (M, W, or All)
                selectInput(
                    "filter_gender",
                    "Gender",
                    choices = c("All", "M", "W"),
                    selected = "All"
                )
            )
        ),

        # Add horizontal divider line
        hr(),

        # Reset button to restore all inputs to defaults
        actionButton(
            "reset_btn",
            "Reset to Defaults",
            class = "btn-outline-danger w-100", # Full-width red outline button
            icon = icon("rotate-left")
        )
    ),

    # --- MAIN CONTENT PANEL ---
    # Create 4-column layout for KPI boxes (key performance indicators)
    layout_columns(
        col_widths = c(3, 3, 3, 3), # 4 equal columns

        # KPI Box 1: Baseline Revenue
        value_box(
            title = "Baseline Revenue",
            value = textOutput("kpi_baseline"), # Dynamic value from server
            showcase = icon("indian-rupee-sign"),
            theme = "primary"
        ),

        # KPI Box 2: Scenario A Impact
        value_box(
            title = "Scenario A",
            value = textOutput("kpi_a"),
            showcase = icon("cart-shopping"),
            theme = "info"
        ),

        # KPI Box 3: Scenario B Impact
        value_box(
            title = "Scenario B",
            value = textOutput("kpi_b"),
            showcase = icon("map"),
            theme = "warning"
        ),

        # KPI Box 4: Scenario C Impact
        value_box(
            title = "Scenario C",
            value = textOutput("kpi_c"),
            showcase = icon("tag"),
            theme = "success"
        )
    ),

    # Create 3-column layout for combined scenario KPIs
    layout_columns(
        col_widths = c(4, 4, 4), # 3 equal columns

        # KPI Box 5: Combined scenario revenue
        value_box(
            title = "Combined Scenario Revenue",
            value = textOutput("kpi_combined"),
            showcase = icon("chart-line"),
            theme = "dark"
        ),

        # KPI Box 6: Total uplift amount
        value_box(
            title = "Total Uplift",
            value = textOutput("kpi_uplift"),
            showcase = icon("arrow-trend-up"),
            theme = "success"
        ),

        # KPI Box 7: Total uplift percentage
        value_box(
            title = "Uplift %",
            value = textOutput("kpi_uplift_pct"),
            showcase = icon("percent"),
            theme = "info"
        )
    ),

    # Create tabbed card with 6 different analysis views
    navset_card_tab(
        title = "Analysis Views",

        # Tab 1: Revenue by Gender
        nav_panel("Revenue by Gender", plotOutput("plot_gender", height = "450px")),

        # Tab 2: Top 10 States
        nav_panel("Top 10 States", plotOutput("plot_states", height = "450px")),

        # Tab 3: Revenue by Category
        nav_panel("Revenue by Category", plotOutput("plot_category", height = "450px")),

        # Tab 4: Channel Waterfall Chart
        nav_panel("Channel Waterfall", plotOutput("plot_waterfall", height = "450px")),

        # Tab 5: Age Group Distribution
        nav_panel("Age Group Distribution", plotOutput("plot_age", height = "450px")),

        # Tab 6: Detailed Scenario Table
        nav_panel("Scenario Table", DTOutput("scenario_table"))
    )
)

# ============================================================================
# PART 8: SHINY SERVER LOGIC
# ============================================================================

# Define server function that processes user inputs and generates outputs
server <- function(input, output, session) {
    # --- SUBSECTION 8.1: RESET BUTTON LOGIC ---
    # Observe reset button clicks and restore all inputs to defaults
    observeEvent(input$reset_btn, {
        # Reset channel percentage to 0%
        updateSliderInput(session, "channel_pct", value = 0)

        # Reset state percentage to 0%
        updateSliderInput(session, "state_pct", value = 0)

        # Reset category discount to 0%
        updateSliderInput(session, "cat_discount", value = 0)

        # Reset category volume to 0%
        updateSliderInput(session, "cat_volume", value = 0)

        # Reset channel selection to "All"
        updateSelectInput(session, "channel_select", selected = "All")

        # Reset state selection to "All"
        updateSelectInput(session, "state_select", selected = "All")

        # Reset category selection to "All"
        updateSelectInput(session, "cat_select", selected = "All")

        # Reset age group selection to all groups
        updateCheckboxGroupInput(session, "filter_age", selected = all_age_groups)

        # Reset gender filter to "All"
        updateSelectInput(session, "filter_gender", selected = "All")
    })

    # --- SUBSECTION 8.2: REACTIVE FILTERED DATA ---
    # Create reactive dataset that updates based on age and gender filters
    filtered_data <- reactive({
        # Start with raw data
        d <- raw_data %>%
            # Filter to include only selected age groups
            filter(Age.Group %in% input$filter_age)

        # If gender filter is not "All", apply gender filter
        if (input$filter_gender != "All") {
            d <- d %>%
                filter(Gender == input$filter_gender)
        }

        # Return filtered dataset
        d
    })

    # --- SUBSECTION 8.3: SCENARIO CALCULATIONS ---
    # Calculate scenario impacts reactively as inputs change
    scenario_vals <- reactive({
        # Get filtered data
        d <- filtered_data()

        # Calculate baseline revenue for filtered data
        base <- sum(d$Amount, na.rm = TRUE)

        # Calculate multiplier for channel scenario (e.g., 1.10 for +10%)
        ch_mult <- 1 + input$channel_pct / 100

        # Calculate multiplier for state scenario
        st_mult <- 1 + input$state_pct / 100

        # Calculate multiplier for category scenario (discount then volume increase)
        cat_mult <- (1 - input$cat_discount / 100) * (1 + input$cat_volume / 100)

        # Create boolean vector for channel matching (TRUE if all or if channel matches)
        ch_match <- if (input$channel_select == "All") {
            rep(TRUE, nrow(d)) # Select all rows
        } else {
            d$Channel == input$channel_select # Select matching channel
        }

        # Create boolean vector for state matching
        st_match <- if (input$state_select == "All") {
            rep(TRUE, nrow(d))
        } else {
            d$ship.state == input$state_select
        }

        # Create boolean vector for category matching
        cat_match <- if (input$cat_select == "All") {
            rep(TRUE, nrow(d))
        } else {
            d$Category == input$cat_select
        }

        # --- SCENARIO A: Channel Revenue Boost ---
        # Apply channel multiplier only to matching rows
        d_a <- d %>%
            mutate(Scen = if_else(ch_match, Amount * ch_mult, Amount))

        # Calculate impact as difference from baseline
        impact_a <- sum(d_a$Scen, na.rm = TRUE) - base

        # --- SCENARIO B: State Revenue Boost ---
        # Apply state multiplier only to matching rows
        d_b <- d %>%
            mutate(Scen = if_else(st_match, Amount * st_mult, Amount))

        # Calculate impact
        impact_b <- sum(d_b$Scen, na.rm = TRUE) - base

        # --- SCENARIO C: Category Promotion ---
        # Apply category multiplier only to matching rows
        d_c <- d %>%
            mutate(Scen = if_else(cat_match, Amount * cat_mult, Amount))

        # Calculate impact
        impact_c <- sum(d_c$Scen, na.rm = TRUE) - base

        # --- COMBINED: All Three Scenarios Together ---
        # Start with original amounts
        d_all <- d %>%
            mutate(Scen = Amount) %>%
            # Apply channel multiplier where appropriate
            mutate(Scen = if_else(ch_match, Scen * ch_mult, Scen)) %>%
            # Apply state multiplier where appropriate
            mutate(Scen = if_else(st_match, Scen * st_mult, Scen)) %>%
            # Apply category multiplier where appropriate
            mutate(Scen = if_else(cat_match, Scen * cat_mult, Scen))

        # Calculate combined scenario total
        combined <- sum(d_all$Scen, na.rm = TRUE)

        # Return list of all calculated values
        list(
            base = base, # Baseline revenue
            impact_a = impact_a, # Scenario A impact
            impact_b = impact_b, # Scenario B impact
            impact_c = impact_c, # Scenario C impact
            combined = combined, # Combined scenario revenue
            uplift = combined - base, # Total uplift amount
            uplift_pct = if (base > 0) (combined - base) / base * 100 else 0, # Uplift percentage
            d_all = d_all # Modified data for tables
        )
    })

    # --- SUBSECTION 8.4: KPI OUTPUTS ---

    # Output 1: Baseline Revenue KPI
    output$kpi_baseline <- renderText({
        sv <- scenario_vals()
        fmt_lakh(sv$base)
    })

    # Output 2: Scenario A Impact KPI
    output$kpi_a <- renderText({
        sv <- scenario_vals()
        # Add "+" prefix for positive values
        sign_prefix <- if (sv$impact_a >= 0) "+" else ""
        paste0(sign_prefix, fmt_lakh(sv$impact_a))
    })

    # Output 3: Scenario B Impact KPI
    output$kpi_b <- renderText({
        sv <- scenario_vals()
        sign_prefix <- if (sv$impact_b >= 0) "+" else ""
        paste0(sign_prefix, fmt_lakh(sv$impact_b))
    })

    # Output 4: Scenario C Impact KPI
    output$kpi_c <- renderText({
        sv <- scenario_vals()
        sign_prefix <- if (sv$impact_c >= 0) "+" else ""
        paste0(sign_prefix, fmt_lakh(sv$impact_c))
    })

    # Output 5: Combined Scenario Revenue KPI
    output$kpi_combined <- renderText({
        sv <- scenario_vals()
        fmt_lakh(sv$combined)
    })

    # Output 6: Total Uplift Amount KPI
    output$kpi_uplift <- renderText({
        sv <- scenario_vals()
        sign_prefix <- if (sv$uplift >= 0) "+" else ""
        paste0(sign_prefix, fmt_lakh(sv$uplift))
    })

    # Output 7: Uplift Percentage KPI
    output$kpi_uplift_pct <- renderText({
        sv <- scenario_vals()
        sign_prefix <- if (sv$uplift_pct >= 0) "+" else ""
        paste0(sign_prefix, round(sv$uplift_pct, 2), "%")
    })

    # --- SUBSECTION 8.5: VISUALIZATION OUTPUTS ---

    # --- PLOT 1: Revenue by Gender (Bar Chart) ---
    output$plot_gender <- renderPlot({
        # Get filtered data
        d <- filtered_data()

        # Group by gender and sum revenues
        gender_sums <- d %>%
            group_by(Gender) %>%
            summarise(Revenue = sum(Amount, na.rm = TRUE), .groups = "drop") %>%
            # Convert gender codes to labels
            mutate(Gender = if_else(Gender == "M", "Male", "Female"))

        # Create bar chart
        ggplot(gender_sums, aes(x = Gender, y = Revenue, fill = Gender)) +
            # Draw bars with white border
            geom_col(width = 0.6, color = "white") +
            # Add revenue labels on top of bars
            geom_text(aes(label = fmt_lakh(Revenue)),
                vjust = -0.5, # Position above bar
                size = 4,
                fontface = "bold"
            ) +
            # Set colors for male (blue) and female (red)
            scale_fill_manual(values = c("Male" = "#3498DB", "Female" = "#E74C3C")) +
            # Format y-axis with comma separators and expand for labels
            scale_y_continuous(labels = label_comma(), expand = expansion(mult = c(0, 0.15))) +
            # Add titles
            labs(title = "Revenue per Gender", x = "Gender", y = "Revenue (INR)") +
            # Apply minimal theme
            theme_minimal(base_size = 14) +
            # Customize appearance
            theme(
                legend.position = "none", # Hide legend
                plot.title = element_text(face = "bold", hjust = 0.5), # Bold centered title
                panel.grid.major.x = element_blank() # Remove x-axis grid lines
            )
    })

    # --- PLOT 2: Top 10 States (Pie/Donut Chart) ---
    output$plot_states <- renderPlot({
        # Get filtered data
        d <- filtered_data()

        # Group by state, sum revenues, sort descending, keep top 10
        state_sums <- d %>%
            group_by(ship.state) %>%
            summarise(Revenue = sum(Amount, na.rm = TRUE), .groups = "drop") %>%
            arrange(desc(Revenue)) %>%
            head(10)

        # Create pie chart using polar coordinates
        ggplot(state_sums, aes(x = "", y = Revenue, fill = ship.state)) +
            # Draw stacked bar
            geom_col(width = 1, color = "white", linewidth = 1.2) +
            # Convert to polar coordinates to make pie
            coord_polar("y", start = 0) +
            # Use Set3 color palette (10 distinct colors)
            scale_fill_brewer(palette = "Set3") +
            # Add title
            labs(title = "Top 10 States by Revenue", fill = "State") +
            # Apply minimal theme
            theme_minimal(base_size = 12) +
            # Customize appearance
            theme(
                plot.title = element_text(face = "bold", hjust = 0.5),
                axis.text = element_blank(), # Hide axis text
                axis.title = element_blank(), # Hide axis titles
                panel.grid = element_blank(), # Hide grid
                legend.position = "right" # Position legend on right
            )
    })

    # --- PLOT 3: Revenue by Category (Bar Chart) ---
    output$plot_category <- renderPlot({
        # Get filtered data
        d <- filtered_data()

        # Group by category and sum revenues
        cat_rev <- d %>%
            group_by(Category) %>%
            summarise(Revenue = sum(Amount, na.rm = TRUE), .groups = "drop") %>%
            # Sort by revenue descending
            arrange(desc(Revenue))

        # Convert category to factor with custom level order
        cat_rev$Category <- factor(cat_rev$Category, levels = cat_rev$Category)

        # Create bar chart
        ggplot(cat_rev, aes(x = Category, y = Revenue, fill = Category)) +
            # Draw bars with white border
            geom_col(width = 0.7, color = "white") +
            # Add revenue labels on top
            geom_text(aes(label = fmt_lakh(Revenue)),
                vjust = -0.5,
                size = 3.5,
                fontface = "bold"
            ) +
            # Use Set2 color palette
            scale_fill_brewer(palette = "Set2") +
            # Format y-axis
            scale_y_continuous(labels = label_comma(), expand = expansion(mult = c(0, 0.12))) +
            # Add titles
            labs(title = "Revenue by Category", x = "Category", y = "Revenue (INR)") +
            # Apply minimal theme
            theme_minimal(base_size = 14) +
            # Customize appearance
            theme(
                legend.position = "none", # Hide legend
                plot.title = element_text(face = "bold", hjust = 0.5),
                panel.grid.major.x = element_blank(), # Remove x-axis grid
                axis.text.x = element_text(angle = 30, hjust = 1, size = 11) # Rotate labels
            )
    })

    # --- PLOT 4: Channel Waterfall Chart ---
    output$plot_waterfall <- renderPlot({
        # Get filtered data
        d <- filtered_data()

        # Group by channel and sum revenues
        ch_rev <- d %>%
            group_by(Channel) %>%
            summarise(Revenue = sum(Amount, na.rm = TRUE), .groups = "drop") %>%
            # Sort by revenue descending
            arrange(desc(Revenue))

        # Create cumulative sum for waterfall (end point of each bar)
        ch_rev$end <- cumsum(ch_rev$Revenue)

        # Create starting point (previous bar's end)
        ch_rev$start <- c(0, head(ch_rev$end, -1))

        # Store channel levels for proper ordering
        ch_lvls <- ch_rev$Channel

        # Convert to factor with correct order
        ch_rev$Channel <- factor(ch_rev$Channel, levels = ch_lvls)

        # Create total row
        total_row <- data.frame(
            Channel = factor("Total", levels = c(ch_lvls, "Total")),
            Revenue = sum(ch_rev$Revenue),
            end = sum(ch_rev$Revenue),
            start = 0
        )

        # Update channel factor to include total
        ch_rev$Channel <- factor(ch_rev$Channel, levels = c(ch_lvls, "Total"))

        # Combine channel and total data
        wf <- rbind(ch_rev, total_row)

        # Add type column (channel vs total)
        wf$type <- ifelse(wf$Channel == "Total", "total", "channel")

        # Create waterfall chart
        ggplot(wf, aes(x = Channel)) +
            # Draw rectangles
            geom_rect(aes(
                xmin = as.numeric(Channel) - 0.4,
                xmax = as.numeric(Channel) + 0.4,
                ymin = start,
                ymax = end,
                fill = type
            ), color = "white", linewidth = 1) +
            # Add value labels
            geom_text(aes(y = end, label = fmt_lakh(Revenue)),
                vjust = -0.5,
                size = 3.5,
                fontface = "bold"
            ) +
            # Set colors
            scale_fill_manual(values = c("channel" = "#4A90D9", "total" = "#2ECC71")) +
            # Format y-axis
            scale_y_continuous(labels = label_comma()) +
            # Add titles
            labs(title = "Revenue Waterfall by Channel", x = "Channel", y = "Revenue") +
            # Apply minimal theme
            theme_minimal(base_size = 14) +
            # Customize appearance
            theme(
                legend.position = "none",
                axis.text.x = element_text(angle = 45, hjust = 1, size = 11),
                plot.title = element_text(face = "bold", hjust = 0.5),
                panel.grid.major.x = element_blank()
            )
    })

    # --- PLOT 5: Age Group Distribution (Bar Chart) ---
    output$plot_age <- renderPlot({
        # Get filtered data
        d <- filtered_data()

        # Count records by age group
        age_counts <- as.data.frame(table(d$Age.Group))

        # Rename columns for clarity
        colnames(age_counts) <- c("Age_Group", "Count")

        # Create bar chart
        ggplot(age_counts, aes(x = Age_Group, y = Count, fill = Age_Group)) +
            # Draw bars with white border
            geom_col(color = "white", width = 0.75) +
            # Add count labels on top
            geom_text(aes(label = Count), vjust = -0.5, size = 4, fontface = "bold") +
            # Assign custom colors to each age group
            scale_fill_manual(values = c(
                "0-18" = "#FF6B6B", "19-25" = "#FFA07A", "26-35" = "#FFD93D",
                "36-45" = "#6BCB77", "46-55" = "#4D96FF", "56-65" = "#9B59B6",
                "65+" = "#E056A0"
            )) +
            # Format y-axis with expanded space for labels
            scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
            # Add titles
            labs(
                title = "Customer Distribution by Age Group",
                subtitle = "Number of orders per age bracket",
                x = "Age Group",
                y = "Number of Orders"
            ) +
            # Apply minimal theme
            theme_minimal(base_size = 14) +
            # Customize appearance
            theme(
                legend.position = "none", # Hide legend
                plot.title = element_text(face = "bold", hjust = 0.5), # Bold centered title
                plot.subtitle = element_text(color = "grey50", hjust = 0.5), # Gray subtitle
                panel.grid.major.x = element_blank(), # Remove x-axis grid
                axis.text.x = element_text(face = "bold", size = 12) # Bold x-labels
            )
    })

    # --- TABLE: Scenario Details Summary ---
    output$scenario_table <- renderDT({
        # Get all scenario values
        sv <- scenario_vals()

        # Create data frame with scenario details
        tbl <- data.frame(
            # Scenario names with user inputs
            Scenario = c(
                paste0("A: ", input$channel_select, " ", sprintf("%+d%%", input$channel_pct)),
                paste0("B: ", input$state_select, " ", sprintf("%+d%%", input$state_pct)),
                paste0(
                    "C: ", input$cat_select, " (-", input$cat_discount, "% price, +",
                    input$cat_volume, "% vol)"
                ),
                "Combined (All Three)"
            ),

            # Baseline revenue (same for all rows)
            `Baseline Revenue` = rep(fmt_inr(sv$base), 4),

            # Scenario revenue after changes
            `Scenario Revenue` = c(
                fmt_inr(sv$base + sv$impact_a),
                fmt_inr(sv$base + sv$impact_b),
                fmt_inr(sv$base + sv$impact_c),
                fmt_inr(sv$combined)
            ),

            # Impact amount for each scenario
            `Impact` = c(
                fmt_inr(sv$impact_a),
                fmt_inr(sv$impact_b),
                fmt_inr(sv$impact_c),
                fmt_inr(sv$uplift)
            ),

            # Uplift percentage for each scenario
            `Uplift %` = paste0(
                round(c(sv$impact_a, sv$impact_b, sv$impact_c, sv$uplift) / sv$base * 100, 2), "%"
            ),
            check.names = FALSE # Don't modify column names
        )

        # Create interactive data table
        datatable(
            tbl,
            options = list(
                dom = "t", # Show only table (no search/pagination controls)
                pageLength = 10 # Show 10 rows per page
            ),
            rownames = FALSE, # Don't show row numbers
            class = "table table-striped table-hover" # Bootstrap table styling
        )
    })
}

# ============================================================================
# PART 9: LAUNCH DASHBOARD
# ============================================================================

# Run the Shiny app with UI and server
shinyApp(ui, server)
