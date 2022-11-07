setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

df = read_csv('./articles.csv')
psych::describe(df)

