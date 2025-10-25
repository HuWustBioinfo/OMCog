#1.Baseline Characteristics Table
library(tableone)
myvars <- c("Age","Age_quartile","Gender","Residence","Education_level3","Smoking","Drinking",
            "Exercise","Hypertension","Hyperlipidemia","Diabetes","Cerebrovascular",
            "ABSI100","ZGABSI100","WWI","CI","RFM","BRI","WHTR","WC","BMI","WHR","MCI",
            "Global","Memory","Language","Attention","Executive","Year","MMSE_score") # All variables
catvars <- c("Age_quartile","Gender","Residence","Education_level3","Smoking","Drinking",
             "Exercise","Hypertension","Hyperlipidemia","Diabetes","Cerebrovascular")# Factor variables
tab2 <- CreateTableOne(vars = myvars, data =data, factorVars = catvars, strata = "CI")
tab2 <- CreateTableOne(vars = myvars, data =data, factorVars = catvars)
print(tab2)
# Convert the TableOne object to a data frame
tab2_df <- as.data.frame(print(tab2, quote = FALSE, noSpaces = TRUE))
# Add row names as a new column in the data frame
tab2_df <- cbind(Variable = rownames(tab2_df), tab2_df)
print(tab2_df)

