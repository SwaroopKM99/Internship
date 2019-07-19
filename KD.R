
auto = read.csv("C:/Users/swaroop.mondal/Documents/R-Studio/Indra_data.csv", header = T, sep = ",", stringsAsFactors = T)

auto$ROI_bin = cut(auto$ROI, c(.10,.12,.14,.16,.18,.22))
auto$ROI_bin = as.factor(auto$ROI_bin)
auto$FINAL_POS_F = as.numeric(auto$FINAL_POS_F)
auto$Loan_Amt = as.numeric(auto$Loan_Amt)

View(auto)
str(auto)
summary(auto)

#############################################################################################

#model = auto %>%
#        group_by(Product_Flag_for_Meeting, ROI_bin, Borrower_occupation ) %>%
#        summarise(count = n(), POS_in_cr = sum((FINAL_POS_F/10^7))) %>%
#        mutate(Total = sum(POS_in_cr)) %>%
#        mutate( POS_percent = (POS_in_cr/ Total)*100 ) %>%
#        arrange(Product_Flag_for_Meeting)
#model

#View(model)

#############################################################################################


Cat = LabelEncoder.fit(auto$ROI_bin)
auto$ROI_bin_unique = transform(Cat, auto$ROI_bin)


# (0.10,0.12]	             1
# (0.12,0.14]              2
# (0.14,0.16]              3
# (0.16,0.18]              4
# (0.18,0.22]              5

report_temp = sqldf("SELECT
               Product, 
               ROI,
               Occupation,
               CASES,
               CUST,
               POS_in_cr,
               (POS_in_cr/SUM(POS_in_cr))*100 AS POS_Percent
               
                  FROM ( SELECT 
               Product_Flag_for_Meeting AS 'Product', 
               ROI_bin AS 'ROI',
               Borrower_occupation AS 'Occupation',
               COUNT(APPLICATION_ID) AS 'CASES',
               COUNT(DISTINCT(G_id)) AS 'CUST',
               SUM(FINAL_POS_F/10000000) AS 'POS_in_cr'
               FROM auto GROUP BY Product_Flag_for_Meeting, ROI_bin, Borrower_occupation) t
               GROUP BY Product, ROI, Occupation
            ")
report_temp

View(report_temp)


options(scipen=999)  # to remove the exponential
report_final = sqldf("SELECT
               Product, 
               ROI,
               Occupation,
               CASES,
               CUST,
               POS_in_cr,
               (POS_in_cr/(Total/10000000))*100 AS POS_Percent,
               (Amount_Financed/CUST) AS 'ATS(Lac)'

                  FROM ( SELECT 
               Product_Flag_for_Meeting AS 'Product', 
               ROI_bin_unique AS 'ROI',
               Borrower_occupation AS 'Occupation',
               COUNT(APPLICATION_ID) AS 'CASES',
               COUNT(DISTINCT(G_id)) AS 'CUST',
               SUM(Loan_Amt)/100000 AS 'Amount_Financed',
               Total_FINAL_POS AS Total,
               SUM(FINAL_POS_F/10000000) AS 'POS_in_cr'
               FROM auto GROUP BY Product_Flag_for_Meeting, ROI_bin_unique, Borrower_occupation) t
               GROUP BY Product, ROI, Occupation
            ")
report_final

View(report_final)


write.csv(report_final, file = "PQR_Automation.csv", row.names = FALSE)

