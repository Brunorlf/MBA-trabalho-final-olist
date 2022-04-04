library(readr)
library(dplyr)
df_orders <- read_csv("datasets/olist_orders_dataset.csv")
df_customers <- read_csv("datasets/olist_customers_dataset.csv")
df_payments <- read_csv("datasets/olist_order_payments_dataset.csv")

# Criar dataframe com regiões

df_regioes <- data.frame(uf = c("AC", "AL", "AP", "AM", "BA", "CE", "DF", "ES", "GO", "MA", "MT", "MS", "MG", "PA", "PB", "PR", "PE", "PI", "RJ", "RN", "RS", "RO", "RR", "SC", "SP", "SE", "TO"),
                         estado = c("Acre", "Alagoas", "Amapá", "Amazonas", "Bahia", "Ceará", "Distrito Federal", "Espírito Santo", "Goiás", "Maranhão", "Mato Grosso", "Mato Grosso do Sul", "Minas Gerais", "Pará", "Paraíba", "Paraná", "Pernambuco", "Piauí", "Rio de Janeiro", "Rio Grande do Norte", "Rio Grande do Sul", "Rondônia", "Roraima", "Santa Catarina", "São Paulo", "Sergipe", "Tocantins"),
                         regiao = c("Norte", "Nordeste", "Norte", "Norte", "Nordeste", "Nordeste", "Centro Oeste", "Sudeste", "Centro Oeste", "Nordeste", "Centro Oeste", "Centro Oeste", "Sudeste", "Norte", "Nordeste", "Sul", "Nordeste", "Nordeste", "Sudeste", "Nordeste", "Sul", "Norte", "Norte", "Sul", "Sudeste", "Nordeste", "Norte")
                         )

# Excluir colunas que não serão utilizadas nos dataframes
df_customers <- subset(df_customers, select = c(customer_id, customer_state))
colnames(df_customers)[2] <- "uf"

df_orders <- subset(df_orders, order_status[]=="delivered")
df_orders <- subset(df_orders, select = c(order_id, customer_id, order_delivered_customer_date, order_estimated_delivery_date))

df_payments <- subset(df_payments, select = c(order_id, payment_value))

df_regioes <- subset(df_regioes, select = c(uf, regiao))

# Inner join pedidos, valor e consumidores

df_merge <- merge(x = df_orders, y = df_customers, by = "customer_id", all.x = TRUE) 
df_merge <- merge(x = df_merge, y = df_payments, by = "order_id", all.x = TRUE)
df_merge <- merge(x = df_merge, y = df_regioes, by = "uf", all.x = TRUE)
df_merge <- subset(df_merge, select = c(order_delivered_customer_date, order_estimated_delivery_date, payment_value, uf, regiao))

df_atrasado = df_merge %>% 
    select(order_delivered_customer_date, order_estimated_delivery_date, payment_value, uf, regiao) %>%
    mutate(diferenca_dias = as.integer(difftime(order_delivered_customer_date, order_estimated_delivery_date, units="days"))) %>%
    filter(diferenca_dias > 0)

df_ok = df_merge %>% 
    select(order_delivered_customer_date, order_estimated_delivery_date, payment_value, uf, regiao) %>%
    mutate(diferenca_dias = as.integer(difftime(order_delivered_customer_date, order_estimated_delivery_date, units="days"))) %>%
    filter(diferenca_dias < 1)

summary(df_atrasado)
summary(df_ok)

boxplot(df_atrasado$diferenca_dias)
boxplot(df_ok$diferenca_dias)

hist(df_atrasado$diferenca_dias)
# Regiões que mais tiveram atraso



