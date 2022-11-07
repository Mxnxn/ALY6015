setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

df = read_csv('./adult.csv')

df['income'] = as.numeric(as.factor(df$income))

psych::describe(df)
summary(df)

barplot(prop.table(table(df$income)) * 100,ylim=c(0,100),main='Rel Freq of Income Class',names.arg=c('>50','<=50'))

just_US = df %>% filter(`native-country` == 'United-States')

library('gmodels')
CrossTable(df$income,df$gender)
CrossTable(just_US$income,just_US['native-country'])

