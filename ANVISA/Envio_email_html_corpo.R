if(!require('pacman')) install.packages('pacman')
pacman::p_load(tidyverse, rio, blastula, here, glue)

 
## Credenciais de envio de e-mail
create_smtp_creds_key(
  id = "gmail_creds",
  provider = "gmail",
  user = "cmusso86@gmail.com",
  overwrite = TRUE
)


email <- render_email("ANVISA/Relatorio_G1_html.Rmd")

emails_df <- import("ANVISA/lista_emails.xlsx")

for (i in 1:nrow(emails_df)) {  
email |> 
    smtp_send(
  to = emails_df$email[i],
  from = "cmusso86@gmail.com",
  subject = "Análise de Notícias - Palavras Relacionadas à Saúde",
  credentials = creds_key("gmail_creds")
)}
