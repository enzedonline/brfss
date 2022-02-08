brfss2013 <-brfss2013 %>%
    mutate(weightkg = as.numeric(as.character(weight2))) %>%
    mutate(weightkg = case_when(
        weightkg >= 50 & weightkg < 1000 ~ round(weightkg / 2.2),
        weightkg >= 9000 & weightkg < 9999 ~ weightkg - 9000,
        TRUE ~ as.numeric(NA)
        )
    )  %>%
    mutate(heightcm = case_when(
        height3 >= 50 & height3 < 1000 ~ round(((height3 %/% 100)*12 + (height3 %% 100)) * 2.54),
        height3 >= 9000 & height3 < 9999 ~ height3 - 9000,
        TRUE ~ as.numeric(NA)
        )
    )  %>%
    mutate(bmi = case_when(
        !is.na(heightcm) & !is.na(weightkg) ~ round(weightkg / (heightcm/100)**2, 1),
        TRUE ~ as.numeric(NA)
        )
    ) %>%
    mutate(weightStatus = case_when(
        bmi < 18.5 ~ 2,
        bmi >= 18.5 & bmi < 25 ~ 3,
        bmi >= 25 & bmi < 30 ~ 4,
        bmi > 30 ~ 5,
        TRUE ~ 1
        )
    ) %>%
    mutate(weightStatus = factor(
        weightStatus, 
        labels=c('Unknown', 'Underweight', 'Healthy', 'Overweight', 'Obese')
        )
    )

brfss2013 %>%
    select(heightcm, weightkg, bmi, weightStatus) %>%
    sample_n(20)

weightStatusByRace <- brfss2013 %>%
    select(weightStatus, X_race) %>%
    filter(as.integer(weightStatus) > 1 & !is.na(X_race)) %>%
    group_by(X_race, weightStatus) %>% 
    summarise(n = n(), .groups = 'drop_last') %>%
    mutate(Percent = round(100 * n/sum(n)), 1) %>%
    mutate(X_race = as.character(X_race)) %>%
    mutate(X_race = str_wrap(X_race, 20)) %>%
    arrange(X_race, desc(Percent))

weightStatusByRace %>% 
    select(X_race, weightStatus, Percent) %>%
    pivot_wider(
        names_from = weightStatus, 
        names_sort = T, 
        values_from = Percent
    ) %>%
    rename(Race = X_race) %>%
    kbl() %>% 
    kable_styling(bootstrap_options = c("condensed"))

font_add_google("Roboto", "Roboto")

p <- weightStatusByRace %>% 
    ggplot(aes(x=Race, y=Percent, fill=weightStatus)) +
    geom_col(position="dodge") +
    theme_excel_new(base_family = 'Roboto') +
    labs(fill = "Weight Status") +
    ggtitle("Weight Status as Percentage Make Up of Each Recorded Ethnic Group")
    
ggplotly(p) %>% 
    config(displayModeBar = F) %>%
    layout(xaxis=list(fixedrange=TRUE)) %>% 
    layout(yaxis=list(fixedrange=TRUE))

diabetes_race <- brfss2013 %>%
    select(diabete3, X_race) %>%
    mutate(hasDiabetes = as.integer(diabete3) %in% 1:2) %>%
    filter(!is.na(X_race)) %>%
    group_by(X_race, hasDiabetes) %>% 
    summarise(n = n()) %>%
    mutate(Percent = round(100 * n/sum(n)), 1) %>%
    mutate(Race = as.character(X_race)) %>%
    mutate(Race = str_wrap(Race, 20)) %>%
    filter(hasDiabetes) %>%
    arrange(desc(Percent))



p <- diabetes_race %>% ggplot(aes(x=reorder(Race, -Percent), y=Percent)) +
    geom_col(fill='orange') +
    theme_excel_new(base_family = 'Arial')
ggplotly(p)

diabetes_race <- brfss2013 %>%
    select(diabete3, X_race, weightStatus) %>%
    mutate(hasDiabetes = as.integer(diabete3) %in% 1:2) %>%
    filter(!is.na(X_race) & weightStatus=='Overweight') %>%
    group_by(X_race, hasDiabetes) %>% 
    summarise(n = n(), .groups = 'drop_last') %>%
    mutate(Percent = round(100 * n/sum(n)), 1) %>%
    mutate(X_race = as.character(X_race)) %>%
    mutate(X_race = str_wrap(X_race, 20)) %>%
    filter(hasDiabetes) %>%
    arrange(desc(Percent))

diabetes_race %>% 
    select(X_race, Percent) %>%
    kbl() %>% 
    kable_styling(bootstrap_options = c("condensed"), full_width = F)

p <- diabetes_race %>% ggplot(aes(x=reorder(X_race, -Percent), y=Percent)) +
    geom_col(fill='orange') +
    theme_excel_new(base_family = 'Roboto') +
    ggtitle("Percentage Overweight People in Each Recorded Ethnic Group that Identify as Diabetic")
ggplotly(p)

diabetes_bmi <- brfss2013 %>%
    select(diabete3, X_race, bmi) %>%
    mutate(hasDiabetes = as.integer(diabete3) %in% 1:2) %>%
    filter(!is.na(X_race) & bmi>=23) %>%
    group_by(X_race, hasDiabetes) %>% 
    summarise(n = n(), .groups = 'keep') %>%
    mutate(Percent = round(100 * n/sum(n)), 1) %>%
    mutate(Race = as.character(X_race)) %>%
    mutate(Race = str_wrap(Race, 20)) %>%
    filter(hasDiabetes) %>%
    arrange(desc(Percent))

p <- diabetes_bmi %>% ggplot(aes(x=reorder(Race, -Percent), y=Percent)) +
    geom_col(fill='orange') +
    theme_excel_new(base_family = 'Roboto') +
    ggtitle("Percentage People with BMI > 25 in Each Recorded Ethnic Group that Identify as Diabetic")
ggplotly(p)



weightStatusBySalary <- brfss2013 %>%
    select(weightStatus, income2) %>%
    filter(as.integer(weightStatus) > 1 & !is.na(income2)) %>%
    group_by(income2, weightStatus) %>% 
    summarise(n = n(), .groups = 'drop_last') %>%
    mutate(Percent = round(100 * n/sum(n), 1)) %>%
    rename(Income = income2) %>%
    arrange(Income, desc(Percent))

weightStatusBySalary %>% 
    select(Income, weightStatus, Percent) %>%
    pivot_wider(
        names_from = weightStatus, 
        names_sort = T, 
        values_from = Percent
    ) %>%
    kbl() %>% 
    kable_styling(bootstrap_options = c("condensed"))

weightStatusBySalary %>% 
    ggplot(aes(x=Income, y=Percent, group=weightStatus, color=weightStatus)) +
    geom_line(size=2) +
    theme_excel_new() +
    labs(color = "Weight Status") +
    ggtitle("Weight Status as Percentage Make Up of Each Recorded Income Group")

asthmaStatusBySalary <- brfss2013 %>%
    select(X_ltasth1, income2) %>%
    filter(!is.na(X_ltasth1) & !is.na(income2)) %>%
    group_by(income2, X_ltasth1) %>% 
    summarise(n = n(), .groups = 'drop_last') %>%
    mutate(Percent = round(100 * n/sum(n), 1)) %>%
    rename(Income = income2) %>%
    filter(X_ltasth1=='Yes') %>%
    arrange(Income, desc(Percent))

asthmaStatusBySalary %>% 
    select(Income, Percent) %>%
    kbl() %>% 
    kable_styling(bootstrap_options = c("condensed"), full_width = F)

asthmaStatusBySalary %>% 
    ggplot(aes(x=Income, y=Percent)) +
    geom_col(fill='orange') +
    theme_excel_new() +
    ggtitle("Asthma Rate Amongst Salary Groups")

rentVsOwnBySalary <- brfss2013 %>%
    select(renthom1, income2) %>%
    filter(renthom1 %in% c('Rent', 'Own') & !is.na(income2)) %>%
    group_by(income2, renthom1) %>% 
    summarise(n = n(), .groups = 'drop_last') %>%
    mutate(Percent = round(100 * n/sum(n), 1)) %>%
    rename(Income = income2) %>%
    filter(renthom1=='Rent') %>%
    arrange(Income, desc(Percent))

rentVsOwnBySalary %>% 
    select(Income, Percent) %>%
    kbl() %>% 
    kable_styling(bootstrap_options = c("condensed"), full_width = F)


rentVsOwnVsAsthmaBySalary <- brfss2013 %>%
    select(X_ltasth1, renthom1, income2) %>%
    filter(!is.na(X_ltasth1) & renthom1 %in% c('Rent', 'Own') & !is.na(income2)) %>%
    group_by(income2, renthom1, X_ltasth1) %>% 
    summarise(n = n(), .groups = 'drop_last') %>%
    mutate(Percent = round(100 * n/sum(n), 1)) %>%
    rename(Income = income2) %>%
    select(-n) %>%
    pivot_wider(names_from = renthom1, values_from = Percent) %>%
    filter(X_ltasth1 == 'Yes') %>%
    mutate(`Proportional Difference (%)` = round(100 * (Rent/Own - 1), 1))

rentVsOwnVsAsthmaBySalary %>%
    ungroup() %>%
    plot_ly(x = ~Income, y = ~Rent, type = 'bar', name = 'Asthma Rate for Renters') %>% 
    add_trace(y = ~Own, name = 'Asthma Rate for Owners') %>% 
    add_trace(
        y = ~`Proportional Difference (%)`, 
        type = 'scatter',  
        mode = 'lines+markers', 
        name = 'Proportional Difference (%)'
        ) %>% 
    layout(title = list(text = 'Asthma Rate by Own/Rent Home Status and Income Group'),
           xaxis = list(title = ""),
           yaxis = list(title = 'Percent', barmode = 'group'), 
           barmode = 'group'
    ) %>% 
    config(displayModeBar = F) %>%
    layout(xaxis=list(fixedrange=TRUE)) 
    
rentVsOwnVsAsthmaBySalary %>%
    ungroup() %>%
    plot_ly(
        x = ~Income, 
        y = ~`Proportional Difference (%)`, 
        type = 'scatter',  
        mode = 'lines+markers', 
        name = 'Proportional Difference (%)', 
        line = list(shape = "linear"), 
        inherit = FALSE
        )
    

rentVsOwnVsAsthmaBySalary %>% 
    select(Income, Rent, Own, `Proportional Difference (%)`) %>%
    kbl() %>% 
    kable_styling(bootstrap_options = c("condensed"), full_width = F)


smokingStatusBySalary <- brfss2013 %>%
    select(X_smoker3, income2) %>%
    filter(!is.na(X_smoker3) & !is.na(income2)) %>%
    group_by(income2, X_smoker3) %>% 
    summarise(n = n(), .groups = 'drop_last') %>%
    mutate(Percent = 100 - round(100 * n/sum(n), 1)) %>%
    rename(Income = income2) %>%
    filter(X_smoker3=='Never smoked') %>%
    arrange(Income, desc(Percent))

smokingStatusBySalary %>% 
    select(Income, Percent) %>%
    kbl() %>% 
    kable_styling(bootstrap_options = c("condensed"), full_width = F)

smokingStatusBySalary %>% 
    ggplot(aes(x=Income, y=Percent)) +
    geom_col(fill='orange') +
    theme_excel_new() +
    ggtitle("Asthma Rate Amongst Salary Groups")

dailyDrinksBySalary <- brfss2013 %>%
    select(X_rfdrhv4, income2) %>%
    filter(!is.na(X_rfdrhv4) & !is.na(income2)) %>%
    group_by(income2) %>% 
    summarise(n = n(), .groups = 'drop_last') %>%
    mutate(Percent = round(100 * n/sum(n), 1)) %>%
    rename(Income = income2) %>%
    arrange(Income, desc(Percent))

dailyDrinksBySalary %>% 
    select(Income, Percent) %>%
    kbl() %>% 
    kable_styling(bootstrap_options = c("condensed"), full_width = F)

dailyDrinksBySalary %>% 
    ggplot(aes(x=Income, y=Percent)) +
    geom_col(fill='orange') +
    theme_excel_new() +
    ggtitle("Daily Alcohol Consumption Amongst Salary Groups")



dfQ3 <- brfss2013 %>%
    select(padur1_, menthlth, sleptim1) %>%
    mutate(sleepLengthQuality = 
               case_when(
                   sleptim1 >= 7 & sleptim1 <= 9 ~ 2,
                   (6 <= sleptim1 & sleptim1 < 7) | (9 < sleptim1 & sleptim1 <= 10) ~ 3,
                   (1 <= sleptim1 & sleptim1 < 6) | (10 < sleptim1 & sleptim1 <= 24) ~ 4,
                   TRUE ~ 1
               )
    ) %>%
    mutate(sleepLengthQuality = factor(
        sleepLengthQuality, 
        labels=c('Unknown', 'Healthy', 'Adequate', 'Unhealthy')
        )
    ) %>%
    filter(
        sleepLengthQuality != 'Unknown',
        !is.na(padur1_) & padur1_ <= 599,
        !is.na(menthlth) & menthlth <= 30,
    ) %>%
    rename(Exercise = padur1_, MentalHealth = menthlth) %>%
    mutate(MentalHealth = 30 - MentalHealth)
    
exerciseBySleepCategory <- dfQ3 %>%
    select(Exercise, sleepLengthQuality) %>%
    group_by(sleepLengthQuality) %>% 
    summarise(Minutes = round(mean(Exercise), 1), .groups = 'drop_last')

exerciseBySleepCategory %>% 
    kbl(col.names = c('Sleep Quality', 'Exercise Minutes')) %>% 
    kable_styling(bootstrap_options = c("condensed"), full_width = F)

mhAll <- dfQ3 %>%
    select(MentalHealth, sleepLengthQuality) %>%
    group_by(sleepLengthQuality) %>% 
    summarise(n = n(), .groups = 'drop_last') %>%
    mutate(Percent = round(100 * n/sum(n), 1)) %>%
    select(-n)

mhL25 <- dfQ3 %>%
    select(MentalHealth, sleepLengthQuality) %>%
    filter(MentalHealth <= quantile(dfQ3$MentalHealth, 0.25)) %>%
    group_by(sleepLengthQuality) %>% 
    summarise(n = n(), .groups = 'drop_last') %>%
    mutate(Percent = round(100 * n/sum(n), 1)) %>%
    select(-n)

mhL10 <- dfQ3 %>%
    select(MentalHealth, sleepLengthQuality) %>%
    filter(MentalHealth <= quantile(dfQ3$MentalHealth, 0.1)) %>%
    group_by(sleepLengthQuality) %>% 
    summarise(n = n(), .groups = 'drop_last') %>%
    mutate(Percent = round(100 * n/sum(n), 1)) %>%
    select(-n)

mhChronic <- dfQ3 %>%
    select(MentalHealth, sleepLengthQuality) %>%
    filter(MentalHealth <= quantile(dfQ3$MentalHealth, 0.02)) %>%
    group_by(sleepLengthQuality) %>% 
    summarise(n = n(), .groups = 'drop_last') %>%
    mutate(Percent = round(100 * n/sum(n), 1)) %>%
    select(-n)

mhAll <- mhAll %>% 
    mutate(L25 = mhL25$Percent, L10 = mhL10$Percent, Chronic = mhChronic$Percent)

mhAll %>% 
    kbl(col.names = c('Sleep Quality', 'All', 'Lower 25%', 'Lower 10%', 'Chronic (Lower 2%)')) %>% 
    kable_styling(bootstrap_options = c("condensed"), full_width = F)

mhAll %>%
    plot_ly(x = ~sleepLengthQuality, y = ~Percent, type = 'bar', name = 'All') %>%
    add_trace(y = ~L25, name = 'Lower 25%') %>%
    add_trace(y = ~L10, name = 'Lower 10%') %>%
    add_trace(y = ~Chronic, name = 'Chronic (Lower 2%)') %>%
    layout(xaxis = list(title = 'Sleep Length Quality'), barmode = 'group')

exAll <- dfQ3 %>%
    select(Exercise, sleepLengthQuality) %>%
    group_by(sleepLengthQuality) %>% 
    summarise(n = n(), .groups = 'drop_last') %>%
    mutate(Percent = round(100 * n/sum(n), 1)) %>%
    select(-n)

exU25 <- dfQ3 %>%
    select(Exercise, sleepLengthQuality) %>%
    filter(Exercise >= quantile(dfQ3$Exercise, 0.75)) %>%
    group_by(sleepLengthQuality) %>% 
    summarise(n = n(), .groups = 'drop_last') %>%
    mutate(Percent = round(100 * n/sum(n), 1)) %>%
    select(-n)

exU10 <- dfQ3 %>%
    select(Exercise, sleepLengthQuality) %>%
    filter(Exercise >= quantile(dfQ3$Exercise, 0.9)) %>%
    group_by(sleepLengthQuality) %>% 
    summarise(n = n(), .groups = 'drop_last') %>%
    mutate(Percent = round(100 * n/sum(n), 1)) %>%
    select(-n)

exU2 <- dfQ3 %>%
    select(Exercise, sleepLengthQuality) %>%
    filter(Exercise >= quantile(dfQ3$Exercise, 0.98)) %>%
    group_by(sleepLengthQuality) %>% 
    summarise(n = n(), .groups = 'drop_last') %>%
    mutate(Percent = round(100 * n/sum(n), 1)) %>%
    select(-n)

exAll <- exAll %>% 
    mutate(U25 = exU25$Percent, U10 = exU10$Percent, U2 = exU2$Percent)

exAll %>% 
    kbl(col.names = c('Sleep Quality', 'All', 'Upper 25%', 'Upper 10%', 'Top 2%')) %>% 
    kable_styling(bootstrap_options = c("condensed"), full_width = F)

exAll %>%
    plot_ly(x = ~sleepLengthQuality, y = ~Percent, type = 'bar', name = 'All') %>%
    add_trace(y = ~U25, name = 'Upper 25%') %>%
    add_trace(y = ~U10, name = 'Upper 10%') %>%
    add_trace(y = ~U2, name = 'Top 2%') %>%
    layout(xaxis = list(title = 'Sleep Length Quality'), barmode = 'group')



h1 <- dfQ3 %>% 
    ggplot(aes(MentalHealth)) +
    geom_histogram(fill='orange', bins = 10, aes(y = (..count..)/sum(..count..))) +
    theme_excel_new() + 
    scale_y_continuous(labels=percent) +
    ggtitle('Mental Health Index')

h2 <- dfQ3 %>% 
    ggplot(aes(Exercise)) +
    geom_histogram(fill='orange', bins = 10, aes(y = (..count..)/sum(..count..))) +
    theme_excel_new() +
    scale_y_continuous(labels=percent) +
    ggtitle('Exercise Minutes / Month')

h3 <- dfQ3 %>% 
    ggplot(aes(sleepLengthQuality)) +
    geom_bar(fill='orange', aes(y = (..count..)/sum(..count..))) +
    theme_excel_new() +
    scale_y_continuous(labels=percent) +
    ggtitle('Sleep Health')

title <- ggdraw() + 
    draw_label(
        "Variable Distributions",
        fontface = 'bold',
        x = 0,
        hjust = 0
    ) +
    theme(
        plot.margin = margin(0, 0, 0, 7)
    )

plot_grid(title, plot_grid(h1, h2, h3, nrow = 1), ncol = 1, rel_heights = c(0.1, 1))

