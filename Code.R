library(openxlsx)
library(magrittr)
library(data.table)

# import the data (put correct path of data)
 customer_invoices <- read.csv('customer_invoices_2 (1).csv',sep = ",", header= TRUE, stringsAsFactors = FALSE)

# change into datatable
setDT(customer_invoices)

#convert dates format from char to dates
customer_invoices[, c("status_datetime", "invoice_date", "payment_due_date") := lapply(.SD, function(x) as.Date(x, format = "%d/%m/%Y %H:%M")),
.SDcols = c("status_datetime", "invoice_date", "payment_due_date")]

# import client customer map data
 Client_Customer_map <- read.csv('client_customer_map_2.csv',sep = ",", header= TRUE, stringsAsFactors = FALSE)

# change into datatable
setDT(Client_Customer_map)


#modify column names
setnames(Client_Customer_map, old = c("customer_id", "client_customer_id"), new = c("customer_id_small", "customer_id"))

#sort columns wrt customer id
setkey(Client_Customer_map, customer_id)

Dictionary <- read.xlsx("PATH/Dictionary.xlsx")

# Step1: part a 
Double_Entered_Event <- customer_invoices[, .(gardenia_invoice_id, invoice_status)] %>%
  .[invoice_status == "entered", .N, by = gardenia_invoice_id, ] %>%
  .[N > 1]

#Step1:part b
###### Step1: part b
Cust_inv_date_check <- customer_invoices[
  !gardenia_invoice_id %in% Double_Entered_Event$gardenia_invoice_id,
  .(gardenia_invoice_id, invoice_version, status_datetime, invoice_status)
  ]

Cust_inv_date_check <- Cust_inv_date_check[
  order(gardenia_invoice_id, invoice_version, status_datetime)
  ]

Cust_inv_date_check <- Cust_inv_date_check[
  , status_datetime := as.numeric(status_datetime)
  ]

Cust_inv_date_check <- Cust_inv_date_check[
  , c("invoice_version_check", "status_datetime_check") := lapply(.SD, function(x) c(NA, diff(x))),
  by = .(gardenia_invoice_id), .SDcols = c("invoice_version", "status_datetime")
  ]

Cust_inv_date_check <- Cust_inv_date_check[!is.na(invoice_version_check),
                                           comp_invoice_version_dete := ifelse(invoice_version_check > 0 & status_datetime_check >= 0, "P", "F")
                                           ]

# Step1: part c
customer_invoices_clean<-customer_invoices[!gardenia_invoice_id %in% Double_Entered_Event$gardenia_invoice_id,]

#step1: part d
Sanity_check_dates_1st_row<-customer_invoices_clean[1,.(status_datetime,invoice_date,payment_due_date)]
Sanity_check_dates_last_row<-customer_invoices_clean[.N,.(status_datetime,invoice_date,payment_due_date)]
Sanity_check_dates_min_status_datetime<-customer_invoices_clean[which.min(status_datetime),.(status_datetime,invoice_date,payment_due_date)]
Sanity_check_dates_min_invoice_date<-customer_invoices_clean[which.min(invoice_date),.(status_datetime,invoice_date,payment_due_date)]
Sanity_check_dates_min_payment_due_date<-customer_invoices_clean[which.min(payment_due_date),.(status_datetime,invoice_date,payment_due_date)]

Sanity_check_dates_max_status_datetime<-customer_invoices_clean[which.max(status_datetime),.(status_datetime,invoice_date,payment_due_date)]
Sanity_check_dates_max_invoice_date<-customer_invoices_clean[which.max(invoice_date),.(status_datetime,invoice_date,payment_due_date)]
Sanity_check_dates_max_payment_due_date<-customer_invoices_clean[which.max(payment_due_date),.(status_datetime,invoice_date,payment_due_date)]

###### Step 2: part a and b
###### The function dcast converts the data from long to wide format. In the modified data, invoice_status (entered, paid, adjusted)
###### will be column names and status_datetime will be present under respective columns (entered, paid, adjusted). Since, there are
###### more than one date in the adjusted column per invoice, the maximum date will be taken as adjustment reflects
###### the invoice adjustmet (return of product) by customer

customer_invoices_normalized <- dcast(customer_invoices_clean, gardenia_invoice_id + customer_id + related_document + invoice_currency + invoice_date
                                      + payment_due_date + client_region + input_source + input_user + input_doc_type + client_accounting_currency + payment_terms
                                      ~ invoice_status,
                                      value.var = "status_datetime", fun.aggregate = max
                                      
) %>%
  .[, c("entered", "paid") := lapply(.SD, function(x) as.Date(x)),
    .SDcols = c("entered", "paid")
    ] %>%
  setkey(., gardenia_invoice_id)

###### Since there are more than one entries in Payment method and invoice amount per invoice_status, these fields were separted
###### from customer_invoices_normalized and will be merged using tmp varialbe below and by taking minimum amount from
###### the invoice as required
tmp <- customer_invoices_clean[, .(gardenia_invoice_id, payment_method, invoice_amount)][
  , lapply(.SD, function(x) min(x, na.rm = TRUE)),
  by = gardenia_invoice_id, .SDcols = c("payment_method", "invoice_amount")
  ] %>%
  setkey(., gardenia_invoice_id)

##### part c & d. Firtst merge Payment method and invoice amount and then calculating the part c & d features
customer_invoices_normalized_2 <- tmp[customer_invoices_normalized][, Is_Late := ifelse(paid > payment_due_date, 1, 0)][
  , Early_late_days := paid - payment_due_date
  ] %>% setkey(., customer_id)

customer_invoices_normalized_2 <- Client_Customer_map[customer_invoices_normalized_2]
setkey(customer_invoices_normalized_2, customer_id, entered)


