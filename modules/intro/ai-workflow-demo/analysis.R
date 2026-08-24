# Read a small data file and report sales totals by product.

sales <- read.csv("data/monthly_sales.csv", stringsAsFactors = FALSE)

# Remove incomplete observations before calculating summaries.
sales_complete <- sales[complete.cases(sales), ]

total_sales <- aggregate(sales ~ product, data = sales_complete, sum)
average_sales <- aggregate(sales ~ product, data = sales_complete, mean)

names(total_sales)[2] <- "total_sales"
names(average_sales)[2] <- "average_sales"
summary_table <- merge(total_sales, average_sales, by = "product")

cat("Monthly sales summary\n")
print(summary_table, row.names = FALSE)
