setwd("~/data/test")
# подгрузим сет всех статей национальных журналов 
national <- read_csv("national_articles.csv", 
                     col_types = cols(...1 = col_skip()))
national <- as.data.table(national)

national[Source.title == "Moscow University Physics Bulletin", Sourceid := 28721]
national[Source.title == "Moscow University Physics Bulletin", subject := "Physics and Astronomy"] # там не было айди мы восстановили
# почему-то в датасете статей нет журнала 28721 Moscow University Physics Bulletin (English Translation of Vestnik Moskovskogo Universiteta, Fizika)


# подгрузим сет статей авторов по физике
phy_articles <- read_csv("articles.csv", 
                         col_types = cols(...1 = col_skip()))
phy_articles <- as.data.table(phy_articles)

# подгрузим сет всех журналов

journals_tidy <- read_csv("journals_tidy.csv", 
                          col_types = cols(...1 = col_skip()))
gc()

# оставим только журналы по физике и астрономии в первом списке
national[subject == "Physics and Astronomy",] -> national

# обрежем ряд переменных чтобы потом удобнее было убрать дубли

national[, 2:36] -> national

# уберем дубли которые возникли изза того у журнала несколько категорий
unique(national) -> national_unique
#unique(national, by = c("EID", "Sourceid")) -> national_unique

# фильтр на годы 
national_unique[Year %in% (2014:2019)] -> national_unique

# посчитаем количество авторов в статьях

national_unique[, num_author := (str_count(Authors, ",") + 1)]
table(national_unique$num_author)
hist(national_unique$num_author)

# перекодируем количество авторов в категории

national_unique[num_author >= 21, collab_dummy := "21+"]
national_unique[num_author >= 6 & num_author <= 20, collab_dummy := "6-20"]
national_unique[num_author >= 2 & num_author <= 5, collab_dummy := "2-5"]
national_unique[num_author == 1, collab_dummy := "1"]
table(national_unique$collab_dummy)

# сделаем дамми по странам для статей и авторов

as.data.table(phy_articles$`Country/Region`) -> countries
countries %>%
  separate_rows(V1, sep = "\\|\\ ") -> countries
countries %>%
  mutate(V1 = str_squish(countries$V1)) -> countries 
countries <- as.data.table(countries)
unique(countries) -> countries
countries %>% filter(V1 != "-") -> countries
setnames(countries, "V1", "country")

journals_tidy <- as.data.table(journals_tidy)
# Armenia Azerbaijan    Belarus    Estonia    Georgia Kazakhstan     Latvia  Lithuania 
# Moldova     Russia    Ukraine Uzbekistan 

countries[country %in% c("Armenia",
                         "Azerbaijan",
                         "Belarus",
                         "Estonia",
                         "Georgia",
                         "Kazakhstan",
                         "Latvia",
                         "Lithuania",
                         "Moldova",
                         "Russian Federation",
                         "Ukraine",
                         "Uzbekistan"), national_dummy := 1]
countries[is.na(national_dummy), national_dummy := 0]

national_unique[str_detect(Authors.with.affiliations, "Armenia"), Armenia := 1]
national_unique[is.na(Armenia), Armenia := 0]
table(national_unique$Armenia)

national_unique[str_detect(Authors.with.affiliations, "Azerbaijan"), Azerbaijan := 1]
national_unique[is.na(Azerbaijan), Azerbaijan := 0]
table(national_unique$Azerbaijan)

national_unique[str_detect(Authors.with.affiliations, "Belarus"), Belarus := 1]
national_unique[is.na(Belarus), Belarus := 0]
table(national_unique$Belarus)

national_unique[str_detect(Authors.with.affiliations, "Estonia"), Estonia := 1]
national_unique[is.na(Estonia), Estonia := 0]
table(national_unique$Estonia)

national_unique[str_detect(Authors.with.affiliations, "Georgia"), Georgia := 1]
national_unique[is.na(Georgia), Georgia := 0]
table(national_unique$Georgia)

national_unique[str_detect(Authors.with.affiliations, "Kazakhstan"), Kazakhstan := 1]
national_unique[is.na(Kazakhstan), Kazakhstan := 0]
table(national_unique$Kazakhstan)

national_unique[str_detect(Authors.with.affiliations, "Latvia"), Latvia := 1]
national_unique[is.na(Latvia), Latvia := 0]
table(national_unique$Latvia)

national_unique[str_detect(Authors.with.affiliations, "Lithuania"), Lithuania := 1]
national_unique[is.na(Lithuania), Lithuania := 0]
table(national_unique$Lithuania)

national_unique[str_detect(Authors.with.affiliations, "Moldova"), Moldova := 1]
national_unique[is.na(Moldova), Moldova := 0]
table(national_unique$Moldova)

national_unique[str_detect(Authors.with.affiliations, "Russian Federation"), Russia := 1]
national_unique[is.na(Russia), Russia := 0]
table(national_unique$Russia)

national_unique[str_detect(Authors.with.affiliations, "Ukraine"), Ukraine := 1]
national_unique[is.na(Ukraine), Ukraine := 0]
table(national_unique$Ukraine)

national_unique[str_detect(Authors.with.affiliations, "Uzbekistan"), Uzbekistan := 1]
national_unique[is.na(Uzbekistan), Uzbekistan := 0]
table(national_unique$Uzbekistan)

national_unique[, sum_countries := Armenia + Azerbaijan + Belarus + Estonia + Georgia + Kazakhstan + Latvia + Lithuania + Moldova + Russia + Ukraine + Uzbekistan]
table(national_unique$sum_countries)

# разобьем все статьи на авторов
# убираем 17 наблюдений с отсутстсующими айди

national_unique %>%
  separate_rows(author_id, sep = ";") %>% 
  mutate(author_id = str_squish(author_id)) %>%
  filter(str_length(author_id) > 1) %>%
  filter(author_id != "[No author id available]" & author_id != "[Отсутствует идентификатор автора]") -> df_national


# перекодируем количество авторов в категории для всех публикаций по физике
phy_articles <- as.data.table(phy_articles)
phy_articles[`Number of Authors` >= 21, collab_dummy := "21+"]
phy_articles[`Number of Authors` >= 6 & `Number of Authors` <= 20, collab_dummy := "6-20"]
phy_articles[`Number of Authors` >= 2 & `Number of Authors` <= 5, collab_dummy := "2-5"]
phy_articles[`Number of Authors` == 1, collab_dummy := "1"]
table(phy_articles[sample_dummy == 0]$collab_dummy)

phy_articles[EID %in% national_unique$EID, sample_dummy := 1]
phy_articles[is.na(sample_dummy), sample_dummy := 0]
table(phy_articles$sample_dummy)

national_unique[EID %in% phy_articles$EID, sample_dummy := 1]
national_unique[is.na(sample_dummy), sample_dummy := 0]

# расхождение в 80 работ

# уберем из списка все статьи которые в нашей выборке и пересчитаем статистику

phy_articles <- as.data.table(phy_articles)

phy_articles[str_detect(`Country/Region`, "Armenia"), Armenia := 1]
phy_articles[is.na(Armenia), Armenia := 0]
table(phy_articles$Armenia, phy_articles$sample_dummy)

phy_articles[str_detect(`Country/Region`, "Azerbaijan"), Azerbaijan := 1]
phy_articles[is.na(Azerbaijan), Azerbaijan := 0]
table(phy_articles$Azerbaijan, phy_articles$sample_dummy)

phy_articles[str_detect(`Country/Region`, "Belarus"), Belarus := 1]
phy_articles[is.na(Belarus), Belarus := 0]
table(phy_articles$Belarus, phy_articles$sample_dummy)

phy_articles[str_detect(`Country/Region`, "Estonia"), Estonia := 1]
phy_articles[is.na(Estonia), Estonia := 0]
table(phy_articles$Estonia, phy_articles$sample_dummy)

phy_articles[str_detect(`Country/Region`, "Georgia"), Georgia := 1]
phy_articles[is.na(Georgia), Georgia := 0]
table(phy_articles$Georgia, phy_articles$sample_dummy)

phy_articles[str_detect(`Country/Region`, "Kazakhstan"), Kazakhstan := 1]
phy_articles[is.na(Kazakhstan), Kazakhstan := 0]
table(phy_articles$Kazakhstan, phy_articles$sample_dummy)

phy_articles[str_detect(`Country/Region`, "Latvia"), Latvia := 1]
phy_articles[is.na(Latvia), Latvia := 0]
table(phy_articles$Latvia, phy_articles$sample_dummy)

phy_articles[str_detect(`Country/Region`, "Lithuania"), Lithuania := 1]
phy_articles[is.na(Lithuania), Lithuania := 0]
table(phy_articles$Lithuania, phy_articles$sample_dummy)

phy_articles[str_detect(`Country/Region`, "Moldova"), Moldova := 1]
phy_articles[is.na(Moldova), Moldova := 0]
table(phy_articles$Moldova, phy_articles$sample_dummy)

phy_articles[str_detect(`Country/Region`, "Russian Federation"), Russia := 1]
phy_articles[is.na(Russia), Russia := 0]
table(phy_articles$Russia, phy_articles$sample_dummy)

phy_articles[str_detect(`Country/Region`, "Ukraine"), Ukraine := 1]
phy_articles[is.na(Ukraine), Ukraine := 0]
table(phy_articles$Ukraine, phy_articles$sample_dummy)

phy_articles[str_detect(`Country/Region`, "Uzbekistan"), Uzbekistan := 1]
phy_articles[is.na(Uzbekistan), Uzbekistan := 0]
table(phy_articles$Uzbekistan, phy_articles$sample_dummy)

phy_articles[, sum_countries := Armenia + Azerbaijan + Belarus + Estonia + Georgia + Kazakhstan + Latvia + Lithuania + Moldova + Russia + Ukraine + Uzbekistan]

# разобьем на авторов

phy_articles %>%
  separate_rows(`Scopus Author Ids`, sep = "\\|\\ ") %>% 
  mutate(author_id = str_squish(`Scopus Author Ids`)) %>%
  filter(str_length(`Scopus Author Ids`) > 1) %>%
  filter(author_id != "[No author id available]" & author_id != "[Отсутствует идентификатор автора]") -> df_all

df_national %>%
  group_by(author_id) %>%
  summarise(n()) -> authors_national

df_all %>%
  group_by(author_id) %>%
  summarise(n()) -> authors_all

authors_all <- as.data.table(authors_all)
authors_national <- as.data.table(authors_national)
authors_national[author_id %in% authors_all$author_id, sample_dummy := 1]
authors_national[is.na(sample_dummy), sample_dummy := 0]
authors_national %>%
  group_by(sample_dummy) %>%
  summarise(sum(`n()`))

authors_all[author_id %in% authors_national$author_id, sample_dummy := 1]
authors_all[is.na(sample_dummy), sample_dummy := 0]
setnames(authors_all, "n()", "articles_all")
setnames(authors_national, "n()", "articles_national")

authors_all <- left_join(authors_all, authors_national, by = "author_id")

authors_all <- as.data.table(authors_all)

authors_all %>%
  filter(sample_dummy.x == 1) %>%
  mutate(mean_percent = (articles_national/articles_all)*100) -> authors_all

hist(authors_all[mean_percent <= 100]$mean_percent)
plot(authors_all[mean_percent <= 100]$articles_all, authors_all[mean_percent <= 100]$articles_national)

df_all %>%
  filter(author_id %in% authors_all$author_id) -> df_all_filtered
rm(df_all)

super_journals <- c("29719", "28677", "4000151822",
                    "29229", "21100201772", "29709", "11500153511", "19700182758", 
                    "21100470165", "21100327726", "29150", "29047")

df_all_filtered %>%
  group_by(author_id, `Source ID`) %>%
  summarise(num_articles = n()) -> test

test <- as.data.table(test)  
test[`Source ID` %in% super_journals, super_dummy := 1]
test[is.na(super_dummy), super_dummy := 0]

test %>%
  select(author_id, super_dummy) -> test

test %>%
  group_by(author_id) %>%
  summarise(sum_super = sum(super_dummy)) ->test2

table(test2$sum_super)
test2 <- as.data.table(test2)

test2[sum_super >= 1]$author_id -> super_people

df_national %>%
  filter(author_id %in% super_people) -> test3

test3 %>%
  select(EID, collab_dummy) %>%
  distinct(EID, collab_dummy) -> test3

test3%>%
  group_by(collab_dummy) %>%
  summarise(n()) -> test3


# посчитаем и визуализируем долю разных коллабов по странам

national_unique %>%
  select(collab_dummy, Armenia, Azerbaijan, Belarus, Estonia, Georgia, Kazakhstan, Latvia, Lithuania, Moldova, Russia, Ukraine, Uzbekistan) -> national_collab
writexl::write_xlsx(national_collab, "nat_collab.xlsx")

phy_articles %>%
  filter(sample_dummy == 0) %>%
  select(collab_dummy, Armenia, Azerbaijan, Belarus, Estonia, Georgia, Kazakhstan, Latvia, Lithuania, Moldova, Russia, Ukraine, Uzbekistan) -> other_collab
writexl::write_xlsx(other_collab, "other_collab.xlsx")

# разделим авторов на страны в оригинальном датасете

national_unique[, num_authors2 := str_count(Authors.with.affiliations, ";") + 1]
national_unique[, num_authors3 := str_count(author_id, ";")]
national_unique[, diff := num_authors3 - num_authors2]
table(national_unique$diff)

national_unique %>%
  filter(diff == 0) -> national

national <- as.data.table(national)
national[, author_id2 := str_replace_all(author_id, ";", ",")]

source("https://raw.githubusercontent.com/mrdwab/SOfun/master/R/list_reduction.R")

national %>%
  select(author_id, Authors.with.affiliations) -> df

df <- as.data.frame(df)

## Split the list
install.packages("splitstackshape")
library(splitstackshape)
library(data.table)
cSplit(as.data.table(df, keep.rownames=TRUE), c("author_id", "Authors.with.affiliations"), ";", "long")[, C := str_c(author_id,Authors.with.affiliations)][, lapply(.SD, toString), "rn"]

df$author_id <- str_sub(df$author_id, end = -2)

df %>%
  tidyr::separate_rows(author_id, Authors.with.affiliations, sep = ";") %>%
  mutate(C = if_else(str_c(author_id,Authors.with.affiliations), C, NA)) -> df2

df %>%
  tidyr::separate_rows(author_id, Authors.with.affiliations, sep = ";") -> df

# кодируем национальность
df <- as.data.table(df)

df[str_detect(Authors.with.affiliations, "Armenia"), Armenia := 1]
df[is.na(Armenia), Armenia := 0]

df[str_detect(Authors.with.affiliations, "Azerbaijan"), Azerbaijan := 1]
df[is.na(Azerbaijan), Azerbaijan := 0]

df[str_detect(Authors.with.affiliations, "Belarus"), Belarus := 1]
df[is.na(Belarus), Belarus := 0]

df[str_detect(Authors.with.affiliations, "Estonia"), Estonia := 1]
df[is.na(Estonia), Estonia := 0]

df[str_detect(Authors.with.affiliations, "Georgia"), Georgia := 1]
df[is.na(Georgia), Georgia := 0]

df[str_detect(Authors.with.affiliations, "Kazakhstan"), Kazakhstan := 1]
df[is.na(Kazakhstan), Kazakhstan := 0]

df[str_detect(Authors.with.affiliations, "Latvia"), Latvia := 1]
df[is.na(Latvia), Latvia := 0]

df[str_detect(Authors.with.affiliations, "Lithuania"), Lithuania := 1]
df[is.na(Lithuania), Lithuania := 0]

df[str_detect(Authors.with.affiliations, "Moldova"), Moldova := 1]
df[is.na(Moldova), Moldova := 0]

df[str_detect(Authors.with.affiliations, "Russian Federation"), Russia := 1]
df[str_detect(Authors.with.affiliations, "Russia"), Russia := 1]
df[is.na(Russia), Russia := 0]

df[str_detect(Authors.with.affiliations, "Ukraine"), Ukraine := 1]
df[is.na(Ukraine), Ukraine := 0]

df[str_detect(Authors.with.affiliations, "Uzbekistan"), Uzbekistan := 1]
df[is.na(Uzbekistan), Uzbekistan := 0]

df[, sum_countries := Armenia + Azerbaijan + Belarus + Estonia + Georgia + Kazakhstan + Latvia + Lithuania + Moldova + Russia + Ukraine + Uzbekistan]
df[sum_countries == 0, not_Soviet := 1]
df[is.na(not_Soviet), not_Soviet := 0]


df %>%
  group_by(author_id) %>%
  summarise(Armenia_num = sum(Armenia),
            Azerbaijan_num = sum(Azerbaijan),
            Belarus_num = sum(Belarus),
            Estonia_num = sum(Estonia),
            Georgia_num = sum(Georgia),
            Kazakhstan_num = sum(Kazakhstan),
            Latvia_num = sum(Latvia),
            Lithuania_num = sum(Lithuania),
            Moldova_num = sum(Moldova),
            Russia_num = sum(Russia),
            Ukraine_num = sum(Ukraine),
            Uzbekistan_num = sum(Uzbekistan),
            Other_num = sum(not_Soviet)) -> df1

# создадим колонку со страной
df1 <- as.data.table(df1)

df1$country_person <- names(df1)[2:14][max.col(df1[,2:14])]

df1 %>%
  select(author_id, country_person) -> affiliations

left_join(df_national, affiliations, by = "author_id") -> df_national

df_national %>%
  filter(!is.na(country_person)) -> df_national1

df_national1 %>%
  group_by(author_id, country_person, Year, Sourceid) %>%
  summarise(n()) -> test

df_national1 %>%
  group_by(author_id, country_person, Year) %>%
  summarise(num_journals = n_distinct(Sourceid)) -> test

writexl::write_xlsx(test, "test.xlsx")

left_join(df_all_filtered, affiliations, by = "author_id") -> df_all1

df_all1 %>%
  filter(author_id %in% df_national1$author_id) %>%
  filter(!(`Source ID` %in% national$Sourceid)) %>%
  group_by(country_person, Year, `Source ID`) %>%
  summarise(n()) -> test
  
  
df_all1 %>%
  filter(author_id %in% df_national1$author_id) %>%
  filter(!(`Source ID` %in% national$Sourceid)) %>%
  group_by(author_id, country_person, Year, ) %>%
  summarise(num_journals = n_distinct(`Source ID`)) -> test

# соберем данные для скеттер плота по авторам

df_national1 %>%
  group_by(author_id, country_person) %>%
  summarise(num_national = n_distinct(EID)) -> articles1

df_all1 %>%
  filter(author_id %in% df_national1$author_id) %>%
  filter(!(`Source ID` %in% national$Sourceid)) %>%
  group_by(author_id, country_person) %>%
  summarise(num_other = n_distinct(EID)) -> articles2

articles_scatter <- left_join(articles1, articles2, by = c("author_id", "country_person"))
articles_scatter <- as.data.table(articles_scatter)

articles_scatter[is.na(num_other), num_other := 0]
writexl::write_xlsx(articles_scatter, "test.xlsx")

# посчитаем импакт в сравнении

citescore_data %>%
  select(year, `Scopus Source ID`, `CiteScore 2020`, SJR, `Scopus ASJC Code (Sub-subject Area)`) -> journal_stats

journal_stats <- as.data.table(journal_stats)
setnames(journal_stats, "Scopus Source ID", "Sourceid")
setnames(journal_stats, "Scopus ASJC Code (Sub-subject Area)", "subject_code")
setnames(journal_stats, "year", "Year")

left_join(df_national1, journals_tidy[subject_code == "3101", c("Sourceid", "subject_code")], by = "Sourceid") -> df_national1

left_join(df_national1, journal_stats, by = c("Year", "Sourceid", "subject_code")) -> df_national2

df_national2 %>%
  group_by(author_id, country_person) %>%
  summarise(mean_citescore = mean(`CiteScore 2020`, na.rm = T),
            mean_sjr = mean(SJR, na.rm = T),
            mean_cit = mean(Cited.by, na.rm = T)) ->test

df_all1 %>%
  filter(author_id %in% df_national1$author_id) %>%
  filter(!(Sourceid %in% national$Sourceid)) %>%
  group_by(author_id, country_person) %>%
  summarise(mean_citescore = mean(`CiteScore (publication year)`, na.rm = T),
            mean_sjr = mean(`SJR (publication year)`, na.rm = T),
            mean_cit = mean(Citations, na.rm = T)) -> test

df_all1$`CiteScore (publication year)` <- str_replace(df_all1$`CiteScore (publication year)`, "-", "")
df_all1$`SJR (publication year)` <- str_replace(df_all1$`SJR (publication year)`, "-", "")
df_all1$`SNIP (publication year)` <- str_replace(df_all1$`SNIP (publication year)`, "-", "")

df_all1$`CiteScore (publication year)` <- as.numeric(df_all1$`CiteScore (publication year)`)
df_all1$`SJR (publication year)` <- as.numeric(df_all1$`SJR (publication year)`)
df_all1$`SNIP (publication year)` <- as.numeric(df_all1$`SNIP (publication year)`)

df_all1 <-as.data.table(df_all1)
df_all1[Sourceid %in% super_journals, super_dummy := 1]
df_all1[is.na(super_dummy), super_dummy := 0]

df_all1 %>%
  filter(author_id %in% df_national1$author_id) %>%
  filter(!(Sourceid %in% national$Sourceid)) %>%
  group_by(country_person, Year) %>%
  summarise(n_super = sum(super_dummy)) -> test

# посчитаем импакт журналов по данным журналов национальных

citescore_data %>%
  select(year, `Scopus Source ID`, `CiteScore 2020`, SNIP, SJR, `Scopus ASJC Code (Sub-subject Area)`) -> journal_stats
journal_stats <- as.data.table(journal_stats)
setnames(journal_stats, "Scopus Source ID", "Sourceid")
setnames(journal_stats, "Scopus ASJC Code (Sub-subject Area)", "subject_code")
setnames(journal_stats, "year", "Year")

journals_tidy %>%
  filter(subject == "Physics and Astronomy") %>%
  select(Sourceid, Country, translated_version) -> phy_journals_tidy

phy_journals_tidy <- as.data.table(phy_journals_tidy)
phy_journals_tidy[is.na(translated_version), translated_version := "no"]
n_distinct(phy_journals_tidy$Sourceid)

journal_stats %>%
  filter(Sourceid %in% phy_journals_tidy$Sourceid) %>%
  filter(Year %in% 2014:2019) -> journal_stats_phy
n_distinct(journal_stats_phy$Sourceid)

left_join(journal_stats_phy, phy_journals_tidy, by = "Sourceid") -> impact_national

writexl::write_xlsx(impact_national, "test.xlsx")

# посчитаем импакт журналов по международным данным - по данным тех журналов где еще публиковались люди

df_all1 %>%
  filter(author_id %in% df_national1$author_id) %>%
  filter(!(Sourceid %in% national$Sourceid)) %>%
  group_by(author_id, Year) %>%
  summarise(mean_citescore = mean(`CiteScore (publication year)`, na.rm = T),
            mean_sjr = mean(`SJR (publication year)`, na.rm = T),
            mean_snip = mean(`SNIP (publication year)`, na.rm = T)) -> test
n_distinct(test$author_id)

df_national1 %>%
  select(author_id, Year, Sourceid) -> test2
n_distinct(test2$author_id)

left_join(test2, test, by = c("author_id", "Year")) -> test3

phy_journals_tidy %>%
  distinct() -> phy_journals_tidy
left_join(test3, phy_journals_tidy, by = "Sourceid") -> impact_authors

writexl::write_xlsx(impact_authors, "test.xlsx")

# добавим пост советское дамми к анализу импакта
df_national1 <- as.data.table(df_national1)
df_national1[country_person == "Other_num", postsoviet := "other authors"]
df_national1[is.na(postsoviet), postsoviet := "postsoviet authors"] 

df_all1 <- as.data.table(df_all1)
df_all1[country_person == "Other_num", postsoviet := "other authors"]
df_all1[is.na(postsoviet), postsoviet := "postsoviet authors"] 

df_national1 %>%
  select(author_id, Year, Sourceid, postsoviet, Cited.by) -> test

left_join(test, phy_journals_tidy, by = "Sourceid") -> test2
left_join(journal_stats_phy, test2, by = c("Sourceid", "Year")) -> impact_national2
writexl::write_xlsx(impact_national2, "test.xlsx")

df_all1 %>%
  filter(author_id %in% df_national1$author_id) %>%
  filter(!(Sourceid %in% national$Sourceid)) %>%
  group_by(author_id, postsoviet) %>%
  summarise(mean_citescore = mean(`CiteScore (publication year)`, na.rm = T),
            mean_sjr = mean(`SJR (publication year)`, na.rm = T),
            mean_snip = mean(`SNIP (publication year)`, na.rm = T)) -> test

df_national1 %>%
  select(author_id, postsoviet, Sourceid) -> test2
n_distinct(test2$author_id)

left_join(test2, test, by = "author_id") -> test3

phy_journals_tidy %>%
  distinct() -> phy_journals_tidy
left_join(test3, phy_journals_tidy, by = "Sourceid") -> impact_authors2

writexl::write_xlsx(impact_authors2, "test.xlsx")



# 29 января расчёты для красивой статистики в статье

# сделаем статистику по набору национальных журналов
# во-первых, сделаем датасет где будет список из журналов и присущие им в разные года квартили и другая статистика

journals_tidy[subject == "Physics and Astronomy"] -> journals_tidy_phy

# оставим в датасете только нужные переменные:
# Sourceid, Country, Source.title, Publisher, In WOS, language, 
# translated_version, category, subjectcode, subject)

journals_tidy_phy %>%
  select(Sourceid, Source.title, Country, Publisher, `In WoS`, 
         language, translated_version, category, subject_code, subject) -> journals_tidy_phy

# проверим что у нас нужное количество уникальных журналов - 83

distinct(journals_tidy_phy, across(Sourceid)) # 83

# посчитаем количество стран по журналам по физике и астрономии

journals_tidy_phy2 %>%
  select(Sourceid, Country) %>%
  distinct() %>%
  group_by(Country) %>%
  summarise(n())

# посчитаем среди этих журналов количество переводных и непереводных

journals_tidy_phy %>% 
  select(Sourceid, translated_version) %>%
  unique() %>%
  group_by(translated_version) %>%
  summarise(n())

# посчитаем среди этих журналов количество журналов у которых активных статус в WoS

journals_tidy_phy %>% 
  select(Sourceid, `In WoS`) %>%
  unique() %>%
  group_by(`In WoS`) %>%
  summarise(n())

# 20 нет и 63 есть

# посчитаем у какого количества журналов более одной категории

journals_tidy_phy %>% 
  select(Sourceid, category, Country) %>%
  group_by(Sourceid, Country) %>%
  summarise(n_cat = n()) %>%
  filter(n_cat > 1)

# 16 журналов с более чем 1 категорией, 13 из них рф, 3 - украина

# у журналов есть разные категории и у категорий есть разный квартиль в разные годы
# нам нужно найти лучший квартиль в год на журнал

# сначала присоединим к тайди данным по журналам нужное количество лет из базы по статистике журналов

library(readxl)
journal_stats <- read_excel("data/test/citescore_data.xlsx")

# оставим только те переменные которые нам нужны и освободим память

# year, Scopus Source ID, Title, Scholarly Output, CiteScore 2020, SNIP, SJR
# Scopus ASJC Code (Sub-subject Area), Quartile, Top 10% (CiteScore Percentile)

journal_stats <- as.data.table(journal_stats)
setnames(journal_stats, "Scopus Source ID", "Sourceid")
setnames(journal_stats, "Scholarly Output", "Yearly_publications")
setnames(journal_stats, "CiteScore 2020", "CiteScore")
setnames(journal_stats, "Scopus ASJC Code (Sub-subject Area)", "subject_code")
setnames(journal_stats, "Top 10% (CiteScore Percentile)", "percentile")

journal_stats %>%
  select(year, Sourceid, Title, Yearly_publications, CiteScore, SNIP, SJR,
         subject_code, Quartile, percentile) -> journal_stats

left_join(journals_tidy_phy, journal_stats, by = c("Sourceid", "subject_code")) -> journals_tidy_phy

# кажется где-то образовались na - данных по этим журналам нет в journal stats?
journals_tidy_phy[is.na(year)]

# это происходит потому что в оригинале журнал статс у них стоит код 3100 а не 3101, заменим его для этого списка журналов

#Sourceid
#1: 12000154541
#2:       12893
#3:       14371
#4:       14497
#5:      145506
#6: 19700173016
#7: 19700173018
#8: 19700173019
#9: 19700174657
#10: 21100305004
#11: 21100410100
#12: 21100466428
#13: 21100875598
#14: 21100912212
#15: 21100920795
#16: 21100942105
#17:       27502
#18:       28517
#19:       28721
#20:       37960
#21:  3900148203

journals_tidy_phy[(Sourceid %in% c("12000154541",
                                  "12893",
                                  "14371",
                                  "14497",
                                  "145506",
                                  "19700173016",
                                  "19700173018",
                                  "19700173019",
                                  "19700174657",
                                  "21100305004",
                                  "21100410100",
                                  "21100466428",
                                  "21100875598",
                                  "21100912212",
                                  "21100920795",
                                  "21100942105",
                                  "27502",
                                  "28517",
                                  "28721",
                                  "37960",
                                  "3900148203")) & subject_code == 3101, subject_code := 3100]

# оставим только те наблюдения где в определенный год наивысший квартиль

# создадим квази переменную айди
journals_tidy_phy$quazi_id <- str_c(journals_tidy_phy$Sourceid, "_", journals_tidy_phy$year)

journals_tidy_phy[, .SD[which.min(abs(Quartile))], by = quazi_id] -> journals_tidy_phy2

# проверим что у нас осталось по одному наблюдению в формате журнал - год

journals_tidy_phy2 <- as.data.table(journals_tidy_phy2)

journals_tidy_phy2 %>%
  group_by(Sourceid, year) %>%
  summarise(n_cat = n()) %>%
  filter(n_cat > 1)

# все верно, больше не осталось дублей
# сохраним датасет для визуализации в табло

# перекодируем na в транслейтед в нет

journals_tidy_phy2[is.na(translated_version), translated_version := "no"]

# еще раз проверим что у нас нужное количество журналов в итоге осталось

distinct(journals_tidy_phy2, across(Sourceid)) -> journals1 #83 журнала

writexl::write_xlsx(journals_tidy_phy2, "stats_national_phy.xlsx")
gc()

# Теперь рассчитаем количество публикаций в этих журналах по годам

# работаем с датасетом national_unique (преобразование в начале)

distinct(national_unique, across(Sourceid)) # 82 журнала
national[Source.title == "Moscow University Physics Bulletin", Sourceid := 28721]
national[Source.title == "Moscow University Physics Bulletin", subject := "Physics and Astronomy"] # там не было айди мы восстановили
# почему-то в датасете статей нет журнала 28721 Moscow University Physics Bulletin (English Translation of Vestnik Moskovskogo Universiteta, Fizika)

# итого у нас 50477 статей
table(national_unique$Year) #  распределение по годам
national_unique[is.na(translated_version), translated_version := "no"]
table(national_unique$translated_version, national_unique$Year)
table(national_unique$translated_version)

# подсчитаем количество статей по журналам и отметим топ-10 журналов
setnames(national_unique, "Year", "year")

left_join(national_unique, unique(journals_tidy_phy2[, c("Sourceid", "Source.title", "Country", "translated_version")]), by = "Sourceid") -> national_with_stats
national_with_stats <- as.data.table(national_with_stats)

national_with_stats %>%
  group_by(Sourceid, Source.title.x, Source.title.y, Country) %>%
  summarise(n_pubs = n()) %>%
  mutate(percent_pubs = round((n_pubs/50477)*100,2)) -> test

table(national_with_stats$num_author)
# выгрузим таблицу в табло и попробуем посчитать доли разных типов статей по годам
writexl::write_xlsx(national_with_stats[, c("Sourceid", "Country", "year", "translated_version", "Language.of.Original.Document", "collab_dummy")], "national_with_stats.xlsx")

# переходим к анализу на уровне автора 
# нам надо прикрутить к статьям в этом датасете айди и страну происхождения авторов

# разобьем все статьи на авторов
# убираем 17 наблюдений с отсутстсующими айди

national_with_stats %>%
  separate_rows(author_id, sep = ";") %>% 
  mutate(author_id = str_squish(author_id)) %>%
  filter(str_length(author_id) > 1) %>%
  filter(author_id != "[No author id available]" & author_id != "[Отсутствует идентификатор автора]") -> national_authors

national_authors <- as.data.table(national_authors)

# сколько у нас уникальных авторов получается
distinct(national_authors, across(author_id)) #74036

# попробуем другую разбивку на авторов

national_with_stats[, num_authors2 := str_count(Authors.with.affiliations, ";") + 1]
national_with_stats[, num_authors3 := str_count(author_id, ";")]
national_with_stats[, diff := num_authors3 - num_authors2]
table(national_with_stats$diff)

national_with_stats %>%
  filter(diff == 0) -> national_authors

national_with_stats2 <- as.data.table(national_with_stats2)
national_with_stats2[, author_id2 := str_replace_all(author_id, ";", ",")]

source("https://raw.githubusercontent.com/mrdwab/SOfun/master/R/list_reduction.R")

national_with_stats2 %>%
  select(author_id, Authors.with.affiliations) -> df

df$author_id <- str_sub(df$author_id, end = -2)

df %>%
  tidyr::separate_rows(author_id, Authors.with.affiliations, sep = ";") -> df

# кодируем национальность
df <- as.data.table(df)

df[str_detect(Authors.with.affiliations, "Armenia"), Armenia := 1]
df[is.na(Armenia), Armenia := 0]

df[str_detect(Authors.with.affiliations, "Azerbaijan"), Azerbaijan := 1]
df[is.na(Azerbaijan), Azerbaijan := 0]

df[str_detect(Authors.with.affiliations, "Belarus"), Belarus := 1]
df[is.na(Belarus), Belarus := 0]

df[str_detect(Authors.with.affiliations, "Estonia"), Estonia := 1]
df[is.na(Estonia), Estonia := 0]

df[str_detect(Authors.with.affiliations, "Georgia"), Georgia := 1]
df[is.na(Georgia), Georgia := 0]

df[str_detect(Authors.with.affiliations, "Kazakhstan"), Kazakhstan := 1]
df[is.na(Kazakhstan), Kazakhstan := 0]

df[str_detect(Authors.with.affiliations, "Latvia"), Latvia := 1]
df[is.na(Latvia), Latvia := 0]

df[str_detect(Authors.with.affiliations, "Lithuania"), Lithuania := 1]
df[is.na(Lithuania), Lithuania := 0]

df[str_detect(Authors.with.affiliations, "Moldova"), Moldova := 1]
df[is.na(Moldova), Moldova := 0]

df[str_detect(Authors.with.affiliations, "Russian Federation"), Russia := 1]
df[str_detect(Authors.with.affiliations, "Russia"), Russia := 1]
df[is.na(Russia), Russia := 0]

df[str_detect(Authors.with.affiliations, "Ukraine"), Ukraine := 1]
df[is.na(Ukraine), Ukraine := 0]

df[str_detect(Authors.with.affiliations, "Uzbekistan"), Uzbekistan := 1]
df[is.na(Uzbekistan), Uzbekistan := 0]

df[, sum_countries := Armenia + Azerbaijan + Belarus + Estonia + Georgia + Kazakhstan + Latvia + Lithuania + Moldova + Russia + Ukraine + Uzbekistan]
df[sum_countries == 0, not_Soviet := 1]
df[is.na(not_Soviet), not_Soviet := 0]

# докодируем еще 17 стран у которых не менее 200 наблюдений

df[str_detect(Authors.with.affiliations, "Brazil"), Brazil := 1]
df[is.na(Brazil), Brazil := 0]

df[str_detect(Authors.with.affiliations, "Bulgaria"), Bulgaria := 1]
df[is.na(Bulgaria), Bulgaria := 0]

df[str_detect(Authors.with.affiliations, "China"), China := 1]
df[is.na(China), China := 0]

df[str_detect(Authors.with.affiliations, "Czech Republic"), Czech_Republic := 1]
df[is.na(Czech_Republic), Czech_Republic := 0]

df[str_detect(Authors.with.affiliations, "Finland"), Finland := 1]
df[is.na(Finland), Finland := 0]

df[str_detect(Authors.with.affiliations, "France"), France := 1]
df[is.na(France), France := 0]

df[str_detect(Authors.with.affiliations, "Germany"), Germany := 1]
df[is.na(Germany), Germany := 0]

df[str_detect(Authors.with.affiliations, "South Korea"), South_Korea := 1]
df[is.na(South_Korea), South_Korea := 0]

df[str_detect(Authors.with.affiliations, "India"), India := 1]
df[is.na(India), India := 0]

df[str_detect(Authors.with.affiliations, "Italy"), Italy := 1]
df[is.na(Italy), Italy := 0]

df[str_detect(Authors.with.affiliations, "Japan"), Japan := 1]
df[is.na(Japan), Japan := 0]

df[str_detect(Authors.with.affiliations, "Poland"), Poland := 1]
df[is.na(Poland), Poland := 0]

df[str_detect(Authors.with.affiliations, "Slovakia"), Slovakia := 1]
df[is.na(Slovakia), Slovakia := 0]

df[str_detect(Authors.with.affiliations, "Spain"), Spain := 1]
df[is.na(Spain), Spain := 0]

df[str_detect(Authors.with.affiliations, "Switzerland"), Switzerland := 1]
df[is.na(Switzerland), Switzerland := 0]

df[str_detect(Authors.with.affiliations, "Turkey"), Turkey := 1]
df[is.na(Turkey), Turkey := 0]

df[str_detect(Authors.with.affiliations, "United Kingdom"), UK := 1]
df[is.na(UK), UK := 0]

df[str_detect(Authors.with.affiliations, "United States"), USA := 1]
df[is.na(USA), USA := 0]

df[, sum_countries2 := Brazil + Bulgaria + China + Czech_Republic + Finland + France + Germany + South_Korea + India + Italy + Japan + Poland + Slovakia + Spain + Switzerland + Turkey + UK + USA]
table(df$sum_countries2)

df[(sum_countries >= 1) & (sum_countries2 >= 1), multi_affil := 1]
df[is.na(multi_affil), multi_affil := 0]
table(df$multi_affil)

df %>%
  group_by(author_id, multi_affil) %>%
  summarise(Armenia_num = sum(Armenia),
            Azerbaijan_num = sum(Azerbaijan),
            Belarus_num = sum(Belarus),
            Estonia_num = sum(Estonia),
            Georgia_num = sum(Georgia),
            Kazakhstan_num = sum(Kazakhstan),
            Latvia_num = sum(Latvia),
            Lithuania_num = sum(Lithuania),
            Moldova_num = sum(Moldova),
            Russia_num = sum(Russia),
            Ukraine_num = sum(Ukraine),
            Uzbekistan_num = sum(Uzbekistan),
            Other_num = sum(not_Soviet),
            Brazil_num = sum(Brazil),
            Bulgaria_num = sum(Bulgaria),
            China_num = sum(China),
            Czech_num = sum(Czech_Republic),
            Finland_num = sum(Finland),
            France_num = sum(France),
            Germany_num = sum(Germany),
            Korea_num = sum(South_Korea),
            India_num = sum(India),
            Italy_num = sum(Italy),
            Japan_num = sum(Japan),
            Poland_num = sum(Poland),
            Slovakia_num = sum(Slovakia),
            Spain_num = sum(Spain),
            Swiss_num = sum(Switzerland),
            Turkey_num = sum(Turkey),
            UK_num = sum(UK),
            USA_num = sum(USA)) -> df1

# создались мультиакки тк не у всех сразу отметился статус 2+ аффил, уберем это до 73866

df1 <- as.data.table(df1)
df1[, .SD[which.max(abs(multi_affil))], by = author_id] -> df1
df1[, sum_countries2 := Brazil_num + Bulgaria_num + China_num + Czech_num + Finland_num + France_num + Germany_num + Korea_num + India_num + Italy_num + Japan_num + Poland_num + Slovakia_num + Spain_num + Swiss_num + Turkey_num + UK_num + USA_num]
df1[sum_countries2 >=1, Other_num := 0]
df1[, !"sum_countries2"] -> df1

df1$country_person <- names(df1)[3:33][max.col(df1[,3:33])]
table(df1$country_person)

# отрежем нужные символы справа
df1$country_person <- str_sub(df1$country_person, end = -5)

df1 %>%
  select(author_id, multi_affil, country_person) -> affiliations

affiliations <- as.data.table(affiliations)
rm(df1)
# получили файл аффилиаций

# приаттачим его к публикациям национальных авторов

left_join(national_authors[, c("author_id", "Sourceid", "Country", "year", "translated_version", "EID", "num_author")], affiliations, by = "author_id") -> national_authors
national_authors <- as.data.table(national_authors)
national_authors %>%
  filter(!is.na(country_person)) %>%
  select(!multi_affil) %>%
  select(!author_id) -> national_authors

# суммируем страны на уровне страны автора

national_authors %>%
  group_by(Sourceid, Country, year, translated_version, EID, num_author, country_person) %>%
  mutate(n_people = n()) %>%
  distinct() -> national_authors3

national_authors3 %>%
  pivot_wider(names_from = country_person, values_from = n_people) -> national_authors2

# теперь заменим все цифры на 1 а na на 0

national_authors2 %>%
  mutate(across(c(France,
                  Ukraine,
                  India,
                  Other,
                  Russia,
                  Germany,
                  Poland,
                  Korea,
                  Kazakhstan,
                  Uzbekistan,
                  Belarus,
                  Japan,
                  Slovakia,
                  Latvia,
                  UK,
                  Turkey,
                  USA,
                  China,
                  Finland,
                  Italy, 
                  Georgia,
                  Spain,
                  Azerbaijan,
                  Moldova,
                  Czech,
                  Armenia,
                  Swiss,
                  Brazil,
                  Bulgaria,
                  Estonia,
                  Lithuania), ~ replace(., !is.na(.), 1))) -> national_authors2

national_authors2 %>%
  mutate(across(c(France,
                  Ukraine,
                  India,
                  Other,
                  Russia,
                  Germany,
                  Poland,
                  Korea,
                  Kazakhstan,
                  Uzbekistan,
                  Belarus,
                  Japan,
                  Slovakia,
                  Latvia,
                  UK,
                  Turkey,
                  USA,
                  China,
                  Finland,
                  Italy, 
                  Georgia,
                  Spain,
                  Azerbaijan,
                  Moldova,
                  Czech,
                  Armenia,
                  Swiss,
                  Brazil,
                  Bulgaria,
                  Estonia,
                  Lithuania), ~ replace(., is.na(.), 0))) -> national_authors2

national_authors2 <- as.data.table(national_authors2)
national_authors2[, !"EID"] -> test
test <- as.data.table(test)
test[, !"num_author"] -> test
test[, !"year"] -> test
test[, c("Sourceid", "Country", "translated_version", "collab_dummy",
         "France",
         "Ukraine",
         "India",
         "Other",
         "Russia",
         "Germany",
         "Poland",
         "Korea",
         "Kazakhstan",
         "Uzbekistan",
         "Belarus",
         "Japan",
         "Slovakia",
         "Latvia",
         "UK",
         "Turkey",
         "USA",
         "China",
         "Finland",
         "Italy", 
         "Georgia",
         "Spain",
         "Azerbaijan",
         "Moldova",
         "Czech",
         "Armenia",
         "Swiss",
         "Brazil",
         "Bulgaria",
         "Estonia", "Lithuania")] -> test2

test2 %>%
  filter(collab_dummy != "1") %>%
  group_by(Sourceid) %>%
  mutate(n_pubs = n()) -> test2

test2 %>%
  filter(collab_dummy != "1") %>%
  select(!collab_dummy) %>%
  group_by(Sourceid, Country, translated_version, n_pubs) %>%
  summarise_all(.funs = sum,na.rm=T) -> test2

test2 %>%
  pivot_longer(!c("Sourceid", "Country", "translated_version", "n_pubs"), names_to = "Country_author", values_to = "Count") %>%
  mutate(share_country = (Count/n_pubs)*100) %>%
  select("Sourceid", "Country", "translated_version", "Country_author", "share_country") -> test2

writexl::write_xlsx(test2, "national_authors2.xlsx")
rm(test)
rm(test2)

gc()

# Анализируем общую статистику по экстра публикациям

# Сначала уберем оттуда все публикации, которые у нас в списке

phy_articles <- as.data.table(phy_articles)

phy_articles[!(EID %in% national_with_stats$EID),] -> phy_articles
phy_articles[`Publication type` %in% c("Article", "Review"), ] -> phy_articles
gc()

phy_articles[`Source ID` %in% journals_tidy_phy2$Sourceid, sample_dummy := 1]
phy_articles[is.na(sample_dummy), sample_dummy := 0]

phy_articles[`Source ID` %in% journals_tidy$Sourceid, national_dummy := 1]
phy_articles[is.na(national_dummy), national_dummy := 0]

phy_articles %>%
  group_by(`Source ID`, `Scopus Source title`, sample_dummy, national_dummy) %>%
  summarise(n_pubs = n()) %>%
  mutate(share_pubs = (n_pubs/329245)*100) -> test
rm(test)

# посмотрим доли публикаций в других типах журналов из нашего списка

table(phy_articles$national_dummy)

# посмотрим типы публикаций

phy_articles[`Number of Authors` >= 21, collab_dummy := "21+"]
phy_articles[`Number of Authors` >= 6 & `Number of Authors` <= 20, collab_dummy := "6-20"]
phy_articles[`Number of Authors` >= 2 & `Number of Authors` <= 5, collab_dummy := "2-5"]
phy_articles[`Number of Authors` == 1, collab_dummy := "1"]

table(phy_articles$collab_dummy)
table(national_with_stats$collab_dummy)

# посчитаем долю работ по тому же научному направлению

phy_articles[str_detect("3100", phy_articles$`All Science Journal Classification (ASJC) code`), physics_dummy :=1]
phy_articles[str_detect("3101", phy_articles$`All Science Journal Classification (ASJC) code`), physics_dummy :=1]
phy_articles[str_detect("3102", phy_articles$`All Science Journal Classification (ASJC) code`), physics_dummy :=1]
phy_articles[str_detect("3103", phy_articles$`All Science Journal Classification (ASJC) code`), physics_dummy :=1]
phy_articles[str_detect("3104", phy_articles$`All Science Journal Classification (ASJC) code`), physics_dummy :=1]
phy_articles[str_detect("3105", phy_articles$`All Science Journal Classification (ASJC) code`), physics_dummy :=1]
phy_articles[str_detect("3106", phy_articles$`All Science Journal Classification (ASJC) code`), physics_dummy :=1]
phy_articles[str_detect("3107", phy_articles$`All Science Journal Classification (ASJC) code`), physics_dummy :=1]
phy_articles[str_detect("3108", phy_articles$`All Science Journal Classification (ASJC) code`), physics_dummy :=1]
phy_articles[str_detect("3109", phy_articles$`All Science Journal Classification (ASJC) code`), physics_dummy :=1]
phy_articles[str_detect("3110", phy_articles$`All Science Journal Classification (ASJC) code`), physics_dummy :=1]
phy_articles[is.na(physics_dummy), physics_dummy := 0]
table(phy_articles$physics_dummy)

# прикрутим данные citescore к нашим физическим публикациям
citescore <- readxl::read_xlsx("citescore_data.xlsx")
citescore <- as.data.table(citescore)

setnames(citescore, "Scopus Source ID", "Sourceid")
setnames(phy_articles, "Source ID", "Sourceid")
setnames(citescore, "year", "Year")
setnames(citescore, "Year", "year")
setnames(citescore, "Top 10% (CiteScore Percentile)", "percentile")

# оставим только максимальный квартиль в год по каждому журналу

citescore$quaziid <- str_c(citescore$Sourceid, citescore$Year)

citescore[, .SD[which.max(abs(Quartile))], by = quaziid] -> citescore

left_join(phy_articles, citescore[, c("Sourceid", "Year", "Quartile", "percentile")], by = c("Sourceid", "Year")) -> phy_articles

phy_articles <-as.data.table(phy_articles)
table(phy_articles$Quartile)

# посчитаем по каждому автору количество публикаций в национальных журналах
gc()

national_with_stats %>%
  separate_rows(author_id, sep = ";") %>% 
  mutate(author_id = str_squish(author_id)) %>%
  filter(str_length(author_id) > 1) %>%
  filter(author_id != "[No author id available]" & author_id != "[Отсутствует идентификатор автора]") -> national_authors

national_authors <- as.data.table(national_authors)

national_authors[author_id %in% affiliations$author_id,] -> national_authors
distinct(national_authors, across(author_id)) 
distinct(national_authors, across(Sourceid)) 

left_join(national_authors, affiliations, by = "author_id") -> national_authors

left_join(national_authors, citescore[, c("Sourceid", "year", "Quartile", "percentile")], by = c("Sourceid", "year"))-> national_authors

national_authors %>%
  group_by(author_id, Quartile) %>%
  summarise(n_pubs_q_nat = n()) -> test

test %>%
  pivot_wider(names_from = Quartile, values_from = n_pubs_q_nat) -> test

test %>%
  select(1,2,3,4,6) -> test

setnames(test, "1", "Q1_n")
setnames(test, "2", "Q2_n")
setnames(test, "3", "Q3_n")
setnames(test, "4", "Q4_n")

left_join(authors_stats, test, by = "author_id") -> authors_stats
gc()

# разделим все публикации в других журналах на авторов
phy_articles %>%
  separate_rows(`Scopus Author Ids`, sep = "\\|") %>% 
  mutate(author_id = str_squish(`Scopus Author Ids`)) %>%
  filter(str_length(author_id) > 1) %>%
  filter(author_id %in% affiliations$author_id) -> phy_authors

phy_authors <- as.data.table(phy_authors)

distinct(phy_authors, across(author_id)) 

phy_authors %>%
  group_by(author_id) %>%
  summarise(n_pubs_other = n()) -> test

left_join(authors_stats, test, by = "author_id") -> authors_stats
authors_stats <- as.data.table(authors_stats)

authors_stats[is.na(n_pubs_other), n_pubs_other := 0]

phy_authors %>%
  group_by(author_id, Quartile) %>%
  summarise(n()) -> test

phy_authors %>%
  group_by(author_id, Quartile) %>%
  summarise(n_pubs_q_other = n()) -> test

test %>%
  pivot_wider(names_from = Quartile, values_from = n_pubs_q_other) -> test

test %>%
  select(1,2,3,4,6) -> test

setnames(test, "1", "Q1_other")
setnames(test, "2", "Q2_other")
setnames(test, "3", "Q3_other")
setnames(test, "4", "Q4_other")

left_join(authors_stats, test, by = "author_id") -> authors_stats

authors_stats %>%
  mutate(across(c(Q2_n,
                  Q1_n,
                  Q3_n,
                  Q4_n,
                  Q1_other,
                  Q2_other,
                  Q3_other,
                  Q4_other), ~ replace(., is.na(.), 0))) -> authors_stats

authors_stats %>%
  mutate(percent_Q1_n = (Q1_n/n_pubs_nat)*100,
         percent_Q2_n = (Q2_n/n_pubs_nat)*100,
         percent_Q3_n = (Q3_n/n_pubs_nat)*100,
         percent_Q4_n = (Q4_n/n_pubs_nat)*100,
         percent_Q1_other = (Q1_other/n_pubs_other)*100,
         percent_Q2_other = (Q2_other/n_pubs_other)*100,
         percent_Q3_other = (Q3_other/n_pubs_other)*100,
         percent_Q4_other = (Q4_other/n_pubs_other)*100) -> authors_stats

phy_authors[Sourceid %in% super_journals, super_dummy := 1]
phy_authors[is.na(super_dummy), super_dummy := 0]

phy_authors %>%
  group_by(author_id, super_dummy) %>%
  summarise(n_super = n()) -> test

test %>%
  pivot_wider(names_from = super_dummy, values_from = n_super) -> test
setnames(test, "0", "not_super")
setnames(test, "1", "super")

test %>%
  select(1,3) -> test

left_join(authors_stats, test) -> authors_stats
authors_stats[is.na(super), super := 0]

authors_stats %>%
  mutate(percent_super = (super/n_pubs_other)*100) -> authors_stats

left_join(authors_stats, affiliations) -> authors_stats

writexl::write_xlsx(authors_stats, "authors_stats.xlsx")

authors_stats %>%
  filter(n_pubs_other == 0)

authors_stats %>%
  filter(percent_super > 0)

phy_articles[Sourceid %in% super_journals, super_dummy := 1]
phy_articles[is.na(super_dummy), super_dummy := 0]
table(phy_articles$super_dummy, phy_articles$collab_dummy)

# добавим к статьям в национальных журналах статистику авторов

left_join(national_authors, authors_stats, by = "author_id") -> final

final <- as.data.table(final)

final %>%
  select(Sourceid, Source.title.x, translated_version, Country, percent_Q1_other, percent_Q2_other, percent_Q3_other, percent_Q4_other, percent_super) %>%
  group_by(Sourceid, Source.title.x, translated_version, Country) %>%
  mutate(mean_q1 = mean(percent_Q1_other, na.rm = T),
         mean_q2 = mean(percent_Q2_other, na.rm = T),
         mean_q3 = mean(percent_Q3_other, na.rm = T),
         mean_q4 = mean(percent_Q4_other, na.rm = T),
         mean_top = mean(percent_super, na.rm = T)) %>%
  select(Sourceid, Source.title.x, translated_version, Country, mean_q1, mean_q2, mean_q3, mean_q4, mean_top) %>%
  distinct() -> final_test

writexl::write_xlsx(final_test, "final_test.xlsx")

# 11 февраля досчитываем статистики для 8 версии статьи

table(affiliations$country_person, affiliations$multi_affil)

# количество уникальных журналов

phy_articles %>%
  group_by(`Scopus Source title`) %>%
  summarise(n_pubs = n()) %>%
  arrange(desc(n_pubs))

# 23 февраля досчитываем статистики для 9 версии статьи

# квартили по другим журналам по годам 
phy_articles %>%
  group_by(Year, Quartile) %>%
  summarise(n_journals = n_distinct(`Scopus Source title`)) %>%
  filter(!is.na(Quartile)) -> phy_quartile

writexl::write_xlsx(phy_quartile, "phy_quartile.xlsx")

# Доля публикаций авторов в других журналах

# Разделим публикации в других журналах на авторов

phy_articles %>%
  separate_rows(`Scopus Author Ids`, sep = "\\|") %>% 
  mutate(author_id = str_squish(`Scopus Author Ids`)) %>%
  filter(str_length(author_id) > 1) %>%
  filter(author_id %in% affiliations$author_id) -> phy_authors

phy_authors <- as.data.table(phy_authors)

distinct(phy_authors, across(author_id)) 

journals_tidy %>%
  filter(!(subject == "Physics and Astronomy")) %>%
  filter(!(Sourceid %in% journals_tidy_phy2$Sourceid)) -> journals_not_phy

phy_articles[Sourceid %in% journals_not_phy$Sourceid, national_dummy := 1]
phy_articles[is.na(national_dummy), national_dummy := 0]
table(phy_articles$national_dummy)

# собираем статистику для скеттерплота

national_authors %>%
  group_by(author_id) %>%
  mutate(num_national = n()) %>%
  ungroup() %>%
  select(author_id, Sourceid, Country, num_national) %>%
  filter(author_id %in% affiliations$author_id) %>%
  unique() -> authors_tidy

phy_authors %>%
  group_by(author_id) %>%
  summarise(n_other = n()) -> test

left_join(authors_tidy, test, by = "author_id") -> authors_tidy
authors_tidy <-as.data.table(authors_tidy)
authors_tidy[is.na(n_other), n_other := 0]

authors_tidy %>%
  select(author_id, Country, num_national, n_other) %>%
  group_by(author_id, Country, n_other) %>%
  summarise(n_national = sum(num_national)) %>%
  unique() -> test
  
writexl::write_xlsx(test, "scatterplot.xlsx")

# список топ журналов по индексу Nature

nature_journals <- c('11500153511',
                     '25143',
                     '19881',
                     '21677',
                     '23915',
                     '22687',
                     '27030',
                     '26750',
                     '29093',
                     '29183',
                     '18434',
                     '5300152732',
                     '146172',
                     '6100153018',
                     '22781',
                     '19700200838',
                     '15537',
                     '18525',
                     '22566',
                     '14599',
                     '21100242814',
                     '21537',
                     '27545',
                     '22126',
                     '22214',
                     '25870',
                     '26959',
                     '27962',
                     '20798',
                     '25264',
                     '17592',
                     '18555',
                     '15870',
                     '21272',
                     '17400154823',
                     '21100855886',
                     '28520',
                     '16764',
                     '22680',
                     '21100779404',
                     '18606',
                     '17382',
                     '19700188421',
                     '17853',
                     '21206',
                     '16115',
                     '19014',
                     '4000148205',
                     '18100156701',
                     '21100198409',
                     '19700182758',
                     '18990',
                     '17600155041',
                     '21315',
                     '17854',
                     '15819',
                     '21100778827',
                     '5200152704',
                     '17436',
                     '5700165152',
                     '4000151822',
                     '12394',
                     '17978',
                     '26396',
                     '21100874237',
                     '21100874236',
                     '21100779241',
                     '29150',
                     '21100201772',
                     '12977',
                     '4000151808',
                     '21121',
                     '130030',
                     '23571',
                     '21100457028',
                     '19700174677',
                     '19900192103',
                     '17435',
                     '5800173382',
                     '19600166212',
                     '16594',
                     '18795')

phy_articles$nature_dummy <- NA
phy_articles[Sourceid %in% nature_journals, nature_dummy := 1]
phy_articles[is.na(nature_dummy), nature_dummy := 0]
table(phy_articles$nature_dummy)

phy_articles$super_dummy <- NA
phy_articles[Sourceid %in% super_journals, super_dummy := 1]
phy_articles[is.na(super_dummy), super_dummy := 0]
table(phy_articles$super_dummy)

phy_authors$nature_dummy <- NA
phy_authors[Sourceid %in% nature_journals, nature_dummy := 1]
phy_authors[is.na(nature_dummy), nature_dummy := 0]
table(phy_authors$nature_dummy)

phy_authors$super_dummy <- NA
phy_authors[Sourceid %in% super_journals, super_dummy := 1]
phy_authors[is.na(super_dummy), super_dummy := 0]
table(phy_authors$super_dummy)

# посчитаем количество статей в q1 и q2 

phy_authors %>%
  filter(Quartile == 1) %>%
  group_by(author_id) %>%
  summarise(n_q1 = n()) -> test

left_join(authors_tidy, test, by = "author_id") -> authors_tidy

authors_tidy[is.na(n_q1), n_q1 := 0]

phy_authors %>%
  filter(Quartile == 2) %>%
  group_by(author_id) %>%
  summarise(n_q2 = n()) -> test

left_join(authors_tidy, test, by = "author_id") -> authors_tidy

authors_tidy[is.na(n_q2), n_q2 := 0]

# посчитаем количество статей в национальных журналах по другим темам

phy_authors %>%
  filter(national_dummy == 1) %>%
  group_by(author_id) %>%
  summarise(n_nat_other = n()) -> test

left_join(authors_tidy, test, by = "author_id") -> authors_tidy

authors_tidy[is.na(n_nat_other), n_nat_other := 0]


# посчитаем количество статей в журналах с citescore percentile 90+

phy_authors$percentile <- as.numeric(phy_authors$`CiteScore percentile (publication year) *`)

phy_authors %>%
  filter(percentile <= 10) %>%
  group_by(author_id) %>%
  summarise(n_percentile90 = n()) -> test

left_join(authors_tidy, test, by = "author_id") -> authors_tidy

authors_tidy[is.na(n_percentile90), n_percentile90 := 0]

# посчитаем количество статей в журналах индекса нейчер

phy_authors %>%
  filter(nature_dummy == 1) %>%
  group_by(author_id) %>%
  summarise(n_nature = n()) -> test

left_join(authors_tidy, test, by = "author_id") -> authors_tidy

authors_tidy[is.na(n_nature), n_nature := 0]

# посчитаем количество статей в журналах топ 10 скимаго

phy_authors %>%
  filter(super_dummy == 1) %>%
  group_by(author_id) %>%
  summarise(n_super = n()) -> test

left_join(authors_tidy, test, by = "author_id") -> authors_tidy

authors_tidy[is.na(n_super), n_super := 0]

# переводим всё в дамми переменные

authors_tidy <- as.data.table(authors_tidy)

authors_tidy[n_q1 >=1, dummy_q1 := 1]
authors_tidy[is.na(dummy_q1), dummy_q1 := 0]


authors_tidy[n_q2 >=1, dummy_q2 := 1]
authors_tidy[is.na(dummy_q2), dummy_q2 := 0]

authors_tidy[n_other >=1, dummy_other := 1]
authors_tidy[is.na(dummy_other), dummy_other := 0]

authors_tidy[n_nat_other >=1, dummy_nat_other := 1]
authors_tidy[is.na(dummy_nat_other), dummy_nat_other := 0]


authors_tidy[n_nature >=1, dummy_nature := 1]
authors_tidy[is.na(dummy_nature), dummy_nature := 0]

authors_tidy[n_super >=1, dummy_super := 1]
authors_tidy[is.na(dummy_super), dummy_super := 0]

authors_tidy[n_percentile90 >=1, dummy_percentile90 := 1]
authors_tidy[is.na(dummy_percentile90), dummy_percentile90 := 0]

authors_tidy %>%
  select(author_id, Sourceid, Country,
         dummy_q1, dummy_q2, dummy_other, dummy_nat_other, dummy_super, dummy_percentile90, dummy_nature) -> authors_tidy

# на уровне журнала суммируем

authors_tidy %>%
  group_by(Sourceid, Country) %>%
  mutate(n_authors = n()) -> authors_tidy

authors_tidy %>%
  group_by(Sourceid, Country, n_authors) %>%
  summarise(n_q1 = sum(dummy_q1),
            n_q2 = sum(dummy_q2),
            n_other = sum(dummy_other),
            nat_other = sum(dummy_nat_other),
            n_super = sum(dummy_super),
            n_nature = sum(dummy_nature),
            n_percentile90 = sum(dummy_percentile90)) %>%
  unique() %>%
  mutate(percent_q1 = 100*(n_q1/n_authors),
         percent_q2 = 100*(n_q2/n_authors),
         percent_other = 100*(n_other/n_authors),
         percent_nat_other = 100*(nat_other/n_authors),
         percent_super = 100*(n_super/n_authors),
         percent_nature = 100*(n_nature/n_authors),
         percent_percentile90 = 100*(n_percentile90/n_authors)) %>%
  select(Sourceid, Country, percent_q1, percent_q2, percent_other, percent_nat_other, percent_super, percent_nature, percent_percentile90) -> test

journals_tidy_phy2 %>%
  select(Sourceid, translated_version) %>%
  unique() -> test2
left_join(test, test2, by = "Sourceid") -> test

writexl::write_xlsx(test, "statistics_shares.xlsx")


