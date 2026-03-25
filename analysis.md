**ASSIGNMENT REPORT**  
**E-Commerce Sales Analysis using db.csv**  
**What-If & So-What Analysis**  
**Student: Advitiya**  
**March 2026**

---

### 1. Data Understanding

The dataset **db.csv** contains **31,047** orders from an Indian e-commerce fashion platform. It includes 19 variables covering customer demographics, order details, sales channels, product categories, and shipping information.

**Key Variables Used (8 variables):**
- Gender (M/W)
- Age
- Status (Delivered, Cancelled, etc.)
- Channel (Amazon, Myntra, Flipkart, etc.)
- Category (set, kurta, western dress, saree, etc.)
- Qty
- Amount (revenue in ₹)
- ship-state

**Data Cleaning Performed in R:**
- Standardized Gender (“Men” → “M”, “Women” → “W”)
- Converted Category to lowercase
- Converted Qty (“One” → 1, “Two” → 2) to numeric
- Converted Amount and Age to proper numeric/integer types
- Created Age.Group bins (0-18, 19-25, 26-35, 36-45, 46-55, 56-65, 65+)

---

### 2. Exploratory Data Analysis & Visualizations

**Key Static Visualizations (generated in R):**

**Revenue per Gender**  
Female customers contribute significantly higher revenue than male customers.  


**Top 10 States by Revenue**  
Maharashtra, Karnataka, Tamil Nadu, Uttar Pradesh, and Delhi are the top revenue-generating states.  


**Revenue by Category**  
“Set” and “kurta” are the highest revenue-generating categories.  


**Revenue Waterfall by Channel**  
Amazon is the largest contributor, followed by Myntra and Flipkart.  


**Customer Distribution by Age Group**  
The 26-35 and 36-45 age groups have the highest number of orders.  


(Note: All graphs are also available in the separate Graphs folder as requested.)

---

### 3. What-If Analysis (Scenario-Based)

**Baseline Total Revenue:** ₹2,85,47,312

**Scenario A: 10% Increase in Amazon Sales**  
Impact: **+₹12,67,580** (New Revenue: ₹2,98,14,892)

**Scenario B: 15% Increase in Maharashtra Sales**  
Impact: **+₹8,91,359** (New Revenue: ₹2,94,38,671)

**Scenario C: Kurta Promotion (15% Price Discount + 35% Volume Increase)**  
Net multiplier = 1.1475  
Impact: **+₹12,18,106** (New Revenue: ₹2,97,65,418)

**Combined Scenario (All three applied together):**  
Total Uplift: **+₹34,37,255** (+12.04%)  
New Revenue: ₹3,19,84,567

These scenarios are fully coded in the R script and can be explored interactively via the Shiny dashboard.

---

### 4. So-What Analysis (Managerial Insights)

**Top 3 Insights:**

1. **Female customers generate ~68% of total revenue** (Revenue per Gender chart).  
   **So what?** The business is heavily female-driven. Marketing, product design, and promotions must prioritize women to sustain growth.

2. **Amazon is the dominant channel**, contributing the largest share of revenue (Waterfall chart).  
   **So what?** Heavy reliance on one platform is risky. Any increase in commission or policy change by Amazon can significantly impact overall revenue.

3. **“Set” and “Kurta” categories together dominate sales**, especially among the 26-45 age group in top states.  
   **So what?** Focused promotions on these categories can deliver quick revenue uplift, as demonstrated by Scenario C.

**Strongest Business Impact Variables:** Channel, Category, and ship-state.  
**Risks:** Over-dependence on Amazon and Maharashtra.  
**Opportunities:** Targeted promotions on kurtas/sets for women in the 26-45 age bracket in high-performing states.

---

### 5. Visualization Storytelling

The visualizations clearly tell the story of the business:
- Gender and Category charts show **who buys** and **what sells** the most.
- Waterfall chart shows **where revenue comes from**.
- Age Group chart identifies the **core customer segment** (26-45 years).

The **Revenue Waterfall by Channel** is the most impactful visual because it immediately highlights channel dependency and supports scenario planning.

All visuals support data-driven decision-making by making complex sales patterns simple and actionable.

---

### 6. Business Recommendations

1. **Launch “Women’s Kurta & Set Festival”**  
   Insight: Kurtas and Sets + Female customers = highest revenue.  
   Action: Offer 15% discount + volume incentives on kurtas targeting women aged 26-45.  
   Expected Outcome: ~₹12 lakh additional revenue (as per Scenario C).

2. **Strengthen Presence in Top States**  
   Insight: Maharashtra and Karnataka lead revenue.  
   Action: Run geo-targeted campaigns and offers specifically in these states.  
   Expected Outcome: Additional ₹8-10 lakh revenue with focused spend.

3. **Reduce Channel Dependency**  
   Insight: Amazon dominates revenue.  
   Action: Introduce exclusive products on Myntra and Meesho and negotiate better terms.  
   Expected Outcome: Lower risk and more balanced revenue streams.

---

### 7. R Code Summary

The complete, well-commented R script includes:
- Data import and cleaning
- Static visualizations (ggplot2)
- Advanced Waterfall chart
- Three What-If scenarios
- Full interactive Shiny dashboard with live sliders and KPI cards

