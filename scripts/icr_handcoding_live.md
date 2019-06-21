Examine Pilot Data
================
Aaron Erlich
03/06/2019

## Inter Coder Reliability

Specify the wave over which you want to check intercoder reliability in
the `ROUND` variable.

Before, anything else, we need to do some cleaup

``` r
ROUND <- "full_pilot1"
pacman::p_load(tidyverse, tidyselect, irr, kableExtra, irrNA, lubridate, splitstackshape)

icr <- read_csv("./data_raw/Big Data y Acceso a Info en México_June 21, 2019_14.17.csv")

fs_ps1 <- readRDS("./data_raw/full_sample_post_p1.rds") %>%
  filter(round == ROUND)

#remove garbage rows
#omit all dates before June 8 
#or where there is no associated email
icr2 <- icr %>% 
  filter(!(is.na(RecipientEmail)), Progress==100) 

#cleanup folio_id
icr2 <- mutate(icr2, folio_id = ifelse(grepl("'", S1),
                                    trimws(S1),
                                    trimws(str_c("'", S1))))

#get short coder_id
icr2 <- mutate(icr2, coder_id = str_extract(RecipientEmail, "^[[:alnum:]]+"))

#omit earlier coding
icr2 <- icr2 %>%
  mutate(start_date = date(StartDate)) %>%
  filter(start_date >= date("2019-06-08") )

#get rid of dublicated by taking the one with the last start time time
icr2 <- icr2 %>% group_by(folio_id, coder_id ) %>%
  filter(StartDate==max(StartDate)) %>%
    mutate(duplicate = n()>1) %>%
  ungroup()

#check
stopifnot(sum(icr2$duplicate) ==0)

#where there are checkboxes
check_alls <- c("S7", "S8", "S11")

icr2 <- concat.split.expanded(icr2, "S7", sep = ",", 
                      mode = "binary", 
                      type = "character",
                      fill = 0, drop = FALSE)

icr2 <- concat.split.expanded(icr2, "S8", sep = ",", 
                      mode = "binary", 
                      type = "character",
                      fill = 0, drop = FALSE)


icr2 <- concat.split.expanded(icr2, "S11", sep = ",", 
                      mode = "binary", 
                      type = "character",
                      fill = 0, drop = FALSE)

#icr2 <- bind_cols(icr2, s7_cols, s8_cols, s11_cols)

#ordinal
icr2 <- mutate(icr2, R5 = factor(R5, levels = c("Poca o nada", 
                                                  "Menos de la mitad", 
                                                  "Aproximadamente la mitad",
                                                  "La mayoría",
                                                  "Toda")))

icr2 <- mutate(icr2, R7 = factor(R7, levels = c("Poca o nada", 
                                                  "Menos de la mitad", 
                                                  "Aproximadamente la mitad",
                                                  "La mayoría",
                                                  "Toda")))
```

\#make short username

``` r
#only examine folios in wave of interested
to_eval <- filter(icr2, folio_id %in% fs_ps1$FOLIO)

missing_folios <- fs_ps1$FOLIO[!(fs_ps1$FOLIO %in% to_eval$folio_id)]

binary_vars <- vars_select(names(to_eval), matches("S7_|S8_[^4]|S11_"))

yes_nos <- vars_select(names(to_eval), matches("S6_|S10_"))

all_dummy <- str_sort(c(binary_vars, yes_nos), numeric = TRUE)

to_eval<- to_eval %>% 
  mutate_at(vars(yes_nos),
            list(~dplyr::recode(., `Sí` = 1,
                                `No` = 0, 
                                .default = NA_real_)))

number_times_coded <- to_eval %>%
  group_by(folio_id) %>%
  summarise(count = n())


n_coder <- filter(number_times_coded, count > 1)
one_coder <-  filter(number_times_coded, count == 1)
```

These have only bee coded once or not at all for this wave of data entry

``` r
kable(cat(missing_folios))
```

<table>

<tbody>

<tr>

</tr>

</tbody>

</table>

``` r
kable(cat(one_coder$folio_id))
```

    ## '0000500098606 '0000900078812 '0001200365514 '0001200376812 '0001300025410 '0001400002203 '0001400015513 '0001600091108 '0002000051413 '0064100033311 '0210000078606 '0610100051615 '0673800130909 '1031500024406 '1113100023410 '1615100021912 '1820000005214 '2510100095613

<table>

<tbody>

<tr>

</tr>

</tbody>

</table>

``` r
#get only multiply coded folios
to_eval <- filter(to_eval, folio_id %in% n_coder$folio_id)
coder_ids <- unique(to_eval$coder_id)

bad_folios <- vector()

for(i in 1:length(all_dummy)) {
  data_wide <- reshape2::dcast(to_eval,  folio_id ~ coder_id , value.var= all_dummy[i], mean)
  #data_wide <- data_wide %>% select(-folio_id) %>%
  data_wide$rater_var <- apply(data_wide[, coder_ids], 1, var, na.rm = TRUE)
    data_wide$num_coders <- apply(data_wide[, coder_ids], 1, function(x)
      length(na.omit(x)))

  print( sprintf("Binary variable %s", all_dummy[i]))
  
  ratings_out <- data_wide %>%
    filter(rater_var > 0 | is.na(rater_var)) %>% sample_n(5)

#capture the number of coders, get observations with at least 2. 
#output where there is disagreement only
  output <-  ratings_out %>% kable(., escape = TRUE, digit = 2)
  print(output)

  bad_folios <- c(bad_folios, ratings_out$folio_id)

  to_stat <- select(data_wide, -folio_id, -rater_var, -num_coders)
  print(kable(iccNA(to_stat)$ICCs))
    print(sprintf("CORRELATION MATRIX"))

print(kable(cor(to_stat, use = "pairwise.complete.obs"), digit = 2))
  cat("\n\n")

  kendallW <- kendallNA(to_stat)
  print(sprintf("Kendalls W is: %s", kendallW$`Kendall's W`))
  #print(kripp.alpha(t(to_stat), method = "nominal"))
  cat("\n\n")
}
```

\[1\] “Binary variable S6\_1”

<table>

<thead>

<tr>

<th style="text-align:left;">

folio\_id

</th>

<th style="text-align:right;">

andrea

</th>

<th style="text-align:right;">

carmen

</th>

<th style="text-align:right;">

edithimg

</th>

<th style="text-align:right;">

jesicatapia

</th>

<th style="text-align:right;">

katia

</th>

<th style="text-align:right;">

sandra

</th>

<th style="text-align:right;">

rater\_var

</th>

<th style="text-align:right;">

num\_coders

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

’0001200073410

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0.25

</td>

<td style="text-align:right;">

4

</td>

</tr>

<tr>

<td style="text-align:left;">

’0678000010912

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0.33

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’1114100004905

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.50

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’0821000005107

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.33

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’1111200025211

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.33

</td>

<td style="text-align:right;">

3

</td>

</tr>

</tbody>

</table>

<table>

<thead>

<tr>

<th style="text-align:left;">

</th>

<th style="text-align:right;">

ICC

</th>

<th style="text-align:right;">

p-value

</th>

<th style="text-align:right;">

lower CI limit

</th>

<th style="text-align:right;">

upper CI limit

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

ICC(1)

</td>

<td style="text-align:right;">

0.3000415

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.2260342

</td>

<td style="text-align:right;">

0.3740388

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(k)

</td>

<td style="text-align:right;">

0.5326432

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.4370896

</td>

<td style="text-align:right;">

0.6137091

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(A,1)

</td>

<td style="text-align:right;">

0.3189910

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.2460474

</td>

<td style="text-align:right;">

0.3917447

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(A,k)

</td>

<td style="text-align:right;">

0.5546426

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.4644551

</td>

<td style="text-align:right;">

0.6313851

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(C,1)

</td>

<td style="text-align:right;">

0.3230204

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.2493785

</td>

<td style="text-align:right;">

0.3962475

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(C,k)

</td>

<td style="text-align:right;">

0.5592043

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.4690221

</td>

<td style="text-align:right;">

0.6356965

</td>

</tr>

</tbody>

</table>

\[1\] “CORRELATION MATRIX”

<table>

<thead>

<tr>

<th style="text-align:left;">

</th>

<th style="text-align:right;">

andrea

</th>

<th style="text-align:right;">

carmen

</th>

<th style="text-align:right;">

edithimg

</th>

<th style="text-align:right;">

jesicatapia

</th>

<th style="text-align:right;">

katia

</th>

<th style="text-align:right;">

sandra

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

andrea

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

0.31

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

0.65

</td>

<td style="text-align:right;">

0.70

</td>

</tr>

<tr>

<td style="text-align:left;">

carmen

</td>

<td style="text-align:right;">

0.31

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

0.39

</td>

<td style="text-align:right;">

NA

</td>

</tr>

<tr>

<td style="text-align:left;">

edithimg

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

0.3

</td>

<td style="text-align:right;">

\-0.09

</td>

<td style="text-align:right;">

0.56

</td>

</tr>

<tr>

<td style="text-align:left;">

jesicatapia

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

0.30

</td>

<td style="text-align:right;">

1.0

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

NA

</td>

</tr>

<tr>

<td style="text-align:left;">

katia

</td>

<td style="text-align:right;">

0.65

</td>

<td style="text-align:right;">

0.39

</td>

<td style="text-align:right;">

\-0.09

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

0.48

</td>

</tr>

<tr>

<td style="text-align:left;">

sandra

</td>

<td style="text-align:right;">

0.70

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

0.56

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

0.48

</td>

<td style="text-align:right;">

1.00

</td>

</tr>

</tbody>

</table>

\[1\] “Kendalls W is: 0.595486143914009”

\[1\] “Binary variable S6\_2”

<table>

<thead>

<tr>

<th style="text-align:left;">

folio\_id

</th>

<th style="text-align:right;">

andrea

</th>

<th style="text-align:right;">

carmen

</th>

<th style="text-align:right;">

edithimg

</th>

<th style="text-align:right;">

jesicatapia

</th>

<th style="text-align:right;">

katia

</th>

<th style="text-align:right;">

sandra

</th>

<th style="text-align:right;">

rater\_var

</th>

<th style="text-align:right;">

num\_coders

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

’0002100030807

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.33

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’0917500004813

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0.50

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’0002000155613

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.50

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’1411100034409

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.50

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’0001600284910

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.33

</td>

<td style="text-align:right;">

3

</td>

</tr>

</tbody>

</table>

<table>

<thead>

<tr>

<th style="text-align:left;">

</th>

<th style="text-align:right;">

ICC

</th>

<th style="text-align:right;">

p-value

</th>

<th style="text-align:right;">

lower CI limit

</th>

<th style="text-align:right;">

upper CI limit

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

ICC(1)

</td>

<td style="text-align:right;">

0.5129854

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.4477213

</td>

<td style="text-align:right;">

0.5750657

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(k)

</td>

<td style="text-align:right;">

0.7368795

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.6830831

</td>

<td style="text-align:right;">

0.7825194

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(A,1)

</td>

<td style="text-align:right;">

0.5222898

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.4579471

</td>

<td style="text-align:right;">

0.5834233

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(A,k)

</td>

<td style="text-align:right;">

0.7440407

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.6919351

</td>

<td style="text-align:right;">

0.7883071

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(C,1)

</td>

<td style="text-align:right;">

0.5250407

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.4605713

</td>

<td style="text-align:right;">

0.5861825

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(C,k)

</td>

<td style="text-align:right;">

0.7461354

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.6941973

</td>

<td style="text-align:right;">

0.7901891

</td>

</tr>

</tbody>

</table>

\[1\] “CORRELATION MATRIX”

<table>

<thead>

<tr>

<th style="text-align:left;">

</th>

<th style="text-align:right;">

andrea

</th>

<th style="text-align:right;">

carmen

</th>

<th style="text-align:right;">

edithimg

</th>

<th style="text-align:right;">

jesicatapia

</th>

<th style="text-align:right;">

katia

</th>

<th style="text-align:right;">

sandra

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

andrea

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

0.74

</td>

<td style="text-align:right;">

0.61

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

0.68

</td>

<td style="text-align:right;">

0.48

</td>

</tr>

<tr>

<td style="text-align:left;">

carmen

</td>

<td style="text-align:right;">

0.74

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

0.34

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

0.66

</td>

<td style="text-align:right;">

\-0.07

</td>

</tr>

<tr>

<td style="text-align:left;">

edithimg

</td>

<td style="text-align:right;">

0.61

</td>

<td style="text-align:right;">

0.34

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

0.65

</td>

<td style="text-align:right;">

\-0.09

</td>

<td style="text-align:right;">

0.48

</td>

</tr>

<tr>

<td style="text-align:left;">

jesicatapia

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

0.65

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

0.30

</td>

</tr>

<tr>

<td style="text-align:left;">

katia

</td>

<td style="text-align:right;">

0.68

</td>

<td style="text-align:right;">

0.66

</td>

<td style="text-align:right;">

\-0.09

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

0.38

</td>

</tr>

<tr>

<td style="text-align:left;">

sandra

</td>

<td style="text-align:right;">

0.48

</td>

<td style="text-align:right;">

\-0.07

</td>

<td style="text-align:right;">

0.48

</td>

<td style="text-align:right;">

0.30

</td>

<td style="text-align:right;">

0.38

</td>

<td style="text-align:right;">

1.00

</td>

</tr>

</tbody>

</table>

\[1\] “Kendalls W is: 0.722166194668984”

\[1\] “Binary variable S6\_3”

<table>

<thead>

<tr>

<th style="text-align:left;">

folio\_id

</th>

<th style="text-align:right;">

andrea

</th>

<th style="text-align:right;">

carmen

</th>

<th style="text-align:right;">

edithimg

</th>

<th style="text-align:right;">

jesicatapia

</th>

<th style="text-align:right;">

katia

</th>

<th style="text-align:right;">

sandra

</th>

<th style="text-align:right;">

rater\_var

</th>

<th style="text-align:right;">

num\_coders

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

’27705

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0.33

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’1111500024807

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0.33

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’0001000019203

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.50

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’1215100138911

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0.33

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’0416000007815

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.33

</td>

<td style="text-align:right;">

3

</td>

</tr>

</tbody>

</table>

<table>

<thead>

<tr>

<th style="text-align:left;">

</th>

<th style="text-align:right;">

ICC

</th>

<th style="text-align:right;">

p-value

</th>

<th style="text-align:right;">

lower CI limit

</th>

<th style="text-align:right;">

upper CI limit

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

ICC(1)

</td>

<td style="text-align:right;">

0.2316019

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.1571136

</td>

<td style="text-align:right;">

0.3072835

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(k)

</td>

<td style="text-align:right;">

0.4448679

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.3313681

</td>

<td style="text-align:right;">

0.5411589

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(A,1)

</td>

<td style="text-align:right;">

0.3003158

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.2274860

</td>

<td style="text-align:right;">

0.3733491

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(A,k)

</td>

<td style="text-align:right;">

0.5329682

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.4384756

</td>

<td style="text-align:right;">

0.6133836

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(C,1)

</td>

<td style="text-align:right;">

0.3078724

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.2339461

</td>

<td style="text-align:right;">

0.3816438

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(C,k)

</td>

<td style="text-align:right;">

0.5418455

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.4481119

</td>

<td style="text-align:right;">

0.6213500

</td>

</tr>

</tbody>

</table>

\[1\] “CORRELATION MATRIX”

<table>

<thead>

<tr>

<th style="text-align:left;">

</th>

<th style="text-align:right;">

andrea

</th>

<th style="text-align:right;">

carmen

</th>

<th style="text-align:right;">

edithimg

</th>

<th style="text-align:right;">

jesicatapia

</th>

<th style="text-align:right;">

katia

</th>

<th style="text-align:right;">

sandra

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

andrea

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

0.40

</td>

<td style="text-align:right;">

0.26

</td>

<td style="text-align:right;">

0.50

</td>

<td style="text-align:right;">

0.26

</td>

<td style="text-align:right;">

0.33

</td>

</tr>

<tr>

<td style="text-align:left;">

carmen

</td>

<td style="text-align:right;">

0.40

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

0.33

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

0.28

</td>

<td style="text-align:right;">

0.67

</td>

</tr>

<tr>

<td style="text-align:left;">

edithimg

</td>

<td style="text-align:right;">

0.26

</td>

<td style="text-align:right;">

0.33

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

0.31

</td>

<td style="text-align:right;">

0.12

</td>

<td style="text-align:right;">

0.55

</td>

</tr>

<tr>

<td style="text-align:left;">

jesicatapia

</td>

<td style="text-align:right;">

0.50

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

0.31

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

\-0.50

</td>

<td style="text-align:right;">

0.28

</td>

</tr>

<tr>

<td style="text-align:left;">

katia

</td>

<td style="text-align:right;">

0.26

</td>

<td style="text-align:right;">

0.28

</td>

<td style="text-align:right;">

0.12

</td>

<td style="text-align:right;">

\-0.50

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

0.13

</td>

</tr>

<tr>

<td style="text-align:left;">

sandra

</td>

<td style="text-align:right;">

0.33

</td>

<td style="text-align:right;">

0.67

</td>

<td style="text-align:right;">

0.55

</td>

<td style="text-align:right;">

0.28

</td>

<td style="text-align:right;">

0.13

</td>

<td style="text-align:right;">

1.00

</td>

</tr>

</tbody>

</table>

\[1\] “Kendalls W is: 0.57329651236436”

\[1\] “Binary variable S6\_4”

<table>

<thead>

<tr>

<th style="text-align:left;">

folio\_id

</th>

<th style="text-align:right;">

andrea

</th>

<th style="text-align:right;">

carmen

</th>

<th style="text-align:right;">

edithimg

</th>

<th style="text-align:right;">

jesicatapia

</th>

<th style="text-align:right;">

katia

</th>

<th style="text-align:right;">

sandra

</th>

<th style="text-align:right;">

rater\_var

</th>

<th style="text-align:right;">

num\_coders

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

’0917900001507

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0.33

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’2510100061415

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0.33

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’1111200025211

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0.33

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’1816400107212

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0.50

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’0000800101312

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0.33

</td>

<td style="text-align:right;">

3

</td>

</tr>

</tbody>

</table>

<table>

<thead>

<tr>

<th style="text-align:left;">

</th>

<th style="text-align:right;">

ICC

</th>

<th style="text-align:right;">

p-value

</th>

<th style="text-align:right;">

lower CI limit

</th>

<th style="text-align:right;">

upper CI limit

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

ICC(1)

</td>

<td style="text-align:right;">

0.2741820

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.1998635

</td>

<td style="text-align:right;">

0.3489418

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(k)

</td>

<td style="text-align:right;">

0.5010880

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.3990827

</td>

<td style="text-align:right;">

0.5876273

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(A,1)

</td>

<td style="text-align:right;">

0.2749114

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.2007253

</td>

<td style="text-align:right;">

0.3495494

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(A,k)

</td>

<td style="text-align:right;">

0.5020035

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.4004084

</td>

<td style="text-align:right;">

0.5882553

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(C,1)

</td>

<td style="text-align:right;">

0.2754019

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.2010498

</td>

<td style="text-align:right;">

0.3501647

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(C,k)

</td>

<td style="text-align:right;">

0.5026183

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.4008592

</td>

<td style="text-align:right;">

0.5889300

</td>

</tr>

</tbody>

</table>

\[1\] “CORRELATION MATRIX”

<table>

<thead>

<tr>

<th style="text-align:left;">

</th>

<th style="text-align:right;">

andrea

</th>

<th style="text-align:right;">

carmen

</th>

<th style="text-align:right;">

edithimg

</th>

<th style="text-align:right;">

jesicatapia

</th>

<th style="text-align:right;">

katia

</th>

<th style="text-align:right;">

sandra

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

andrea

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

0.60

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

0.15

</td>

<td style="text-align:right;">

\-0.07

</td>

</tr>

<tr>

<td style="text-align:left;">

carmen

</td>

<td style="text-align:right;">

0.60

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

\-0.03

</td>

<td style="text-align:right;">

\-0.05

</td>

</tr>

<tr>

<td style="text-align:left;">

edithimg

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

0.79

</td>

<td style="text-align:right;">

\-0.12

</td>

<td style="text-align:right;">

0.33

</td>

</tr>

<tr>

<td style="text-align:left;">

jesicatapia

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

0.79

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

0.38

</td>

</tr>

<tr>

<td style="text-align:left;">

katia

</td>

<td style="text-align:right;">

0.15

</td>

<td style="text-align:right;">

\-0.03

</td>

<td style="text-align:right;">

\-0.12

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

0.24

</td>

</tr>

<tr>

<td style="text-align:left;">

sandra

</td>

<td style="text-align:right;">

\-0.07

</td>

<td style="text-align:right;">

\-0.05

</td>

<td style="text-align:right;">

0.33

</td>

<td style="text-align:right;">

0.38

</td>

<td style="text-align:right;">

0.24

</td>

<td style="text-align:right;">

1.00

</td>

</tr>

</tbody>

</table>

\[1\] “Kendalls W is: 0.524347158532948”

\[1\] “Binary variable S7\_Datos agregados (base de datos)”

<table>

<thead>

<tr>

<th style="text-align:left;">

folio\_id

</th>

<th style="text-align:right;">

andrea

</th>

<th style="text-align:right;">

carmen

</th>

<th style="text-align:right;">

edithimg

</th>

<th style="text-align:right;">

jesicatapia

</th>

<th style="text-align:right;">

katia

</th>

<th style="text-align:right;">

sandra

</th>

<th style="text-align:right;">

rater\_var

</th>

<th style="text-align:right;">

num\_coders

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

’0063500110314

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0.50

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’0063700065013

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0.50

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’1857600005803

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0.33

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’1615100031912

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0.33

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’0001300041011

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.33

</td>

<td style="text-align:right;">

3

</td>

</tr>

</tbody>

</table>

<table>

<thead>

<tr>

<th style="text-align:left;">

</th>

<th style="text-align:right;">

ICC

</th>

<th style="text-align:right;">

p-value

</th>

<th style="text-align:right;">

lower CI limit

</th>

<th style="text-align:right;">

upper CI limit

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

ICC(1)

</td>

<td style="text-align:right;">

0.3004540

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.2264529

</td>

<td style="text-align:right;">

0.3744379

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(k)

</td>

<td style="text-align:right;">

0.5331319

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.4376782

</td>

<td style="text-align:right;">

0.6141130

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(A,1)

</td>

<td style="text-align:right;">

0.3105482

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.2374556

</td>

<td style="text-align:right;">

0.3835949

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(A,k)

</td>

<td style="text-align:right;">

0.5449536

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.4528226

</td>

<td style="text-align:right;">

0.6233551

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(C,1)

</td>

<td style="text-align:right;">

0.3144541

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.2406446

</td>

<td style="text-align:right;">

0.3879954

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(C,k)

</td>

<td style="text-align:right;">

0.5494582

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.4572820

</td>

<td style="text-align:right;">

0.6276417

</td>

</tr>

</tbody>

</table>

\[1\] “CORRELATION MATRIX”

<table>

<thead>

<tr>

<th style="text-align:left;">

</th>

<th style="text-align:right;">

andrea

</th>

<th style="text-align:right;">

carmen

</th>

<th style="text-align:right;">

edithimg

</th>

<th style="text-align:right;">

jesicatapia

</th>

<th style="text-align:right;">

katia

</th>

<th style="text-align:right;">

sandra

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

andrea

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

0.41

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

0.52

</td>

<td style="text-align:right;">

0.63

</td>

</tr>

<tr>

<td style="text-align:left;">

carmen

</td>

<td style="text-align:right;">

0.41

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

0.34

</td>

<td style="text-align:right;">

NA

</td>

</tr>

<tr>

<td style="text-align:left;">

edithimg

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

NA

</td>

</tr>

<tr>

<td style="text-align:left;">

jesicatapia

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

0.11

</td>

</tr>

<tr>

<td style="text-align:left;">

katia

</td>

<td style="text-align:right;">

0.52

</td>

<td style="text-align:right;">

0.34

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

0.31

</td>

</tr>

<tr>

<td style="text-align:left;">

sandra

</td>

<td style="text-align:right;">

0.63

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

0.11

</td>

<td style="text-align:right;">

0.31

</td>

<td style="text-align:right;">

1.00

</td>

</tr>

</tbody>

</table>

\[1\] “Kendalls W is: 0.564094658629249”

\[1\] “Binary variable S7\_Múltiples datos”

<table>

<thead>

<tr>

<th style="text-align:left;">

folio\_id

</th>

<th style="text-align:right;">

andrea

</th>

<th style="text-align:right;">

carmen

</th>

<th style="text-align:right;">

edithimg

</th>

<th style="text-align:right;">

jesicatapia

</th>

<th style="text-align:right;">

katia

</th>

<th style="text-align:right;">

sandra

</th>

<th style="text-align:right;">

rater\_var

</th>

<th style="text-align:right;">

num\_coders

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

’0610100166512

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0.33

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’1511100022411

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.50

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’0001100183308

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0.33

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’0001300041011

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0.33

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’0000700207914

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0.33

</td>

<td style="text-align:right;">

3

</td>

</tr>

</tbody>

</table>

<table>

<thead>

<tr>

<th style="text-align:left;">

</th>

<th style="text-align:right;">

ICC

</th>

<th style="text-align:right;">

p-value

</th>

<th style="text-align:right;">

lower CI limit

</th>

<th style="text-align:right;">

upper CI limit

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

ICC(1)

</td>

<td style="text-align:right;">

0.6487536

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.5950812

</td>

<td style="text-align:right;">

0.6982081

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(k)

</td>

<td style="text-align:right;">

0.8308164

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.7962258

</td>

<td style="text-align:right;">

0.8601623

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(A,1)

</td>

<td style="text-align:right;">

0.6537985

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.6008195

</td>

<td style="text-align:right;">

0.7025969

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(A,k)

</td>

<td style="text-align:right;">

0.8339158

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.8000608

</td>

<td style="text-align:right;">

0.8626651

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(C,1)

</td>

<td style="text-align:right;">

0.6564403

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.6035373

</td>

<td style="text-align:right;">

0.7050897

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(C,k)

</td>

<td style="text-align:right;">

0.8355289

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.8018798

</td>

<td style="text-align:right;">

0.8640699

</td>

</tr>

</tbody>

</table>

\[1\] “CORRELATION MATRIX”

<table>

<thead>

<tr>

<th style="text-align:left;">

</th>

<th style="text-align:right;">

andrea

</th>

<th style="text-align:right;">

carmen

</th>

<th style="text-align:right;">

edithimg

</th>

<th style="text-align:right;">

jesicatapia

</th>

<th style="text-align:right;">

katia

</th>

<th style="text-align:right;">

sandra

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

andrea

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

0.76

</td>

<td style="text-align:right;">

0.79

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

0.61

</td>

<td style="text-align:right;">

0.64

</td>

</tr>

<tr>

<td style="text-align:left;">

carmen

</td>

<td style="text-align:right;">

0.76

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

0.78

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

0.63

</td>

<td style="text-align:right;">

0.79

</td>

</tr>

<tr>

<td style="text-align:left;">

edithimg

</td>

<td style="text-align:right;">

0.79

</td>

<td style="text-align:right;">

0.78

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

0.50

</td>

<td style="text-align:right;">

0.71

</td>

<td style="text-align:right;">

0.68

</td>

</tr>

<tr>

<td style="text-align:left;">

jesicatapia

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

0.50

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

0.50

</td>

<td style="text-align:right;">

0.49

</td>

</tr>

<tr>

<td style="text-align:left;">

katia

</td>

<td style="text-align:right;">

0.61

</td>

<td style="text-align:right;">

0.63

</td>

<td style="text-align:right;">

0.71

</td>

<td style="text-align:right;">

0.50

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

0.72

</td>

</tr>

<tr>

<td style="text-align:left;">

sandra

</td>

<td style="text-align:right;">

0.64

</td>

<td style="text-align:right;">

0.79

</td>

<td style="text-align:right;">

0.68

</td>

<td style="text-align:right;">

0.49

</td>

<td style="text-align:right;">

0.72

</td>

<td style="text-align:right;">

1.00

</td>

</tr>

</tbody>

</table>

\[1\] “Kendalls W is: 0.789054297325443”

\[1\] “Binary variable S7\_Múltiples documentos”

<table>

<thead>

<tr>

<th style="text-align:left;">

folio\_id

</th>

<th style="text-align:right;">

andrea

</th>

<th style="text-align:right;">

carmen

</th>

<th style="text-align:right;">

edithimg

</th>

<th style="text-align:right;">

jesicatapia

</th>

<th style="text-align:right;">

katia

</th>

<th style="text-align:right;">

sandra

</th>

<th style="text-align:right;">

rater\_var

</th>

<th style="text-align:right;">

num\_coders

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

’1026500043108

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.50

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’0002000155613

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.50

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’0000900247913

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0.33

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’0000900156715

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0.33

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’26625

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0.33

</td>

<td style="text-align:right;">

3

</td>

</tr>

</tbody>

</table>

<table>

<thead>

<tr>

<th style="text-align:left;">

</th>

<th style="text-align:right;">

ICC

</th>

<th style="text-align:right;">

p-value

</th>

<th style="text-align:right;">

lower CI limit

</th>

<th style="text-align:right;">

upper CI limit

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

ICC(1)

</td>

<td style="text-align:right;">

0.6574526

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.6046897

</td>

<td style="text-align:right;">

0.7059700

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(k)

</td>

<td style="text-align:right;">

0.8361452

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.8026442

</td>

<td style="text-align:right;">

0.8645668

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(A,1)

</td>

<td style="text-align:right;">

0.6644864

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.6125170

</td>

<td style="text-align:right;">

0.7122061

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(A,k)

</td>

<td style="text-align:right;">

0.8404005

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.8078011

</td>

<td style="text-align:right;">

0.8680660

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(C,1)

</td>

<td style="text-align:right;">

0.6647753

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.6127611

</td>

<td style="text-align:right;">

0.7125139

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(C,k)

</td>

<td style="text-align:right;">

0.8405743

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.8079575

</td>

<td style="text-align:right;">

0.8682398

</td>

</tr>

</tbody>

</table>

\[1\] “CORRELATION MATRIX”

<table>

<thead>

<tr>

<th style="text-align:left;">

</th>

<th style="text-align:right;">

andrea

</th>

<th style="text-align:right;">

carmen

</th>

<th style="text-align:right;">

edithimg

</th>

<th style="text-align:right;">

jesicatapia

</th>

<th style="text-align:right;">

katia

</th>

<th style="text-align:right;">

sandra

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

andrea

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

0.75

</td>

<td style="text-align:right;">

0.78

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

0.60

</td>

<td style="text-align:right;">

0.81

</td>

</tr>

<tr>

<td style="text-align:left;">

carmen

</td>

<td style="text-align:right;">

0.75

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

0.79

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

0.58

</td>

<td style="text-align:right;">

0.42

</td>

</tr>

<tr>

<td style="text-align:left;">

edithimg

</td>

<td style="text-align:right;">

0.78

</td>

<td style="text-align:right;">

0.79

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

0.86

</td>

<td style="text-align:right;">

0.86

</td>

<td style="text-align:right;">

0.65

</td>

</tr>

<tr>

<td style="text-align:left;">

jesicatapia

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

0.86

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

0.72

</td>

</tr>

<tr>

<td style="text-align:left;">

katia

</td>

<td style="text-align:right;">

0.60

</td>

<td style="text-align:right;">

0.58

</td>

<td style="text-align:right;">

0.86

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

0.50

</td>

</tr>

<tr>

<td style="text-align:left;">

sandra

</td>

<td style="text-align:right;">

0.81

</td>

<td style="text-align:right;">

0.42

</td>

<td style="text-align:right;">

0.65

</td>

<td style="text-align:right;">

0.72

</td>

<td style="text-align:right;">

0.50

</td>

<td style="text-align:right;">

1.00

</td>

</tr>

</tbody>

</table>

\[1\] “Kendalls W is: 0.79223787601887”

\[1\] “Binary variable S7\_Un dato”

<table>

<thead>

<tr>

<th style="text-align:left;">

folio\_id

</th>

<th style="text-align:right;">

andrea

</th>

<th style="text-align:right;">

carmen

</th>

<th style="text-align:right;">

edithimg

</th>

<th style="text-align:right;">

jesicatapia

</th>

<th style="text-align:right;">

katia

</th>

<th style="text-align:right;">

sandra

</th>

<th style="text-align:right;">

rater\_var

</th>

<th style="text-align:right;">

num\_coders

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

’0063700328414

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0.50

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’1511100067610

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.33

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’0817000010910

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.50

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’0001700078810

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0.33

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’1119500000306

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0.33

</td>

<td style="text-align:right;">

3

</td>

</tr>

</tbody>

</table>

<table>

<thead>

<tr>

<th style="text-align:left;">

</th>

<th style="text-align:right;">

ICC

</th>

<th style="text-align:right;">

p-value

</th>

<th style="text-align:right;">

lower CI limit

</th>

<th style="text-align:right;">

upper CI limit

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

ICC(1)

</td>

<td style="text-align:right;">

0.5749413

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.5143664

</td>

<td style="text-align:right;">

0.6317306

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(k)

</td>

<td style="text-align:right;">

0.7824328

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.7379499

</td>

<td style="text-align:right;">

0.8201711

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(A,1)

</td>

<td style="text-align:right;">

0.5787378

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.5187100

</td>

<td style="text-align:right;">

0.6350167

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(A,k)

</td>

<td style="text-align:right;">

0.7850688

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.7412882

</td>

<td style="text-align:right;">

0.8222553

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(C,1)

</td>

<td style="text-align:right;">

0.5813768

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.5213088

</td>

<td style="text-align:right;">

0.6375966

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(C,k)

</td>

<td style="text-align:right;">

0.7868912

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.7432913

</td>

<td style="text-align:right;">

0.8238724

</td>

</tr>

</tbody>

</table>

\[1\] “CORRELATION MATRIX”

<table>

<thead>

<tr>

<th style="text-align:left;">

</th>

<th style="text-align:right;">

andrea

</th>

<th style="text-align:right;">

carmen

</th>

<th style="text-align:right;">

edithimg

</th>

<th style="text-align:right;">

jesicatapia

</th>

<th style="text-align:right;">

katia

</th>

<th style="text-align:right;">

sandra

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

andrea

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

0.81

</td>

<td style="text-align:right;">

0.56

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

0.58

</td>

<td style="text-align:right;">

0.33

</td>

</tr>

<tr>

<td style="text-align:left;">

carmen

</td>

<td style="text-align:right;">

0.81

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

0.56

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

0.57

</td>

<td style="text-align:right;">

0.52

</td>

</tr>

<tr>

<td style="text-align:left;">

edithimg

</td>

<td style="text-align:right;">

0.56

</td>

<td style="text-align:right;">

0.56

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

0.67

</td>

<td style="text-align:right;">

0.84

</td>

<td style="text-align:right;">

0.50

</td>

</tr>

<tr>

<td style="text-align:left;">

jesicatapia

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

0.67

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

0.52

</td>

</tr>

<tr>

<td style="text-align:left;">

katia

</td>

<td style="text-align:right;">

0.58

</td>

<td style="text-align:right;">

0.57

</td>

<td style="text-align:right;">

0.84

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

0.52

</td>

</tr>

<tr>

<td style="text-align:left;">

sandra

</td>

<td style="text-align:right;">

0.33

</td>

<td style="text-align:right;">

0.52

</td>

<td style="text-align:right;">

0.50

</td>

<td style="text-align:right;">

0.52

</td>

<td style="text-align:right;">

0.52

</td>

<td style="text-align:right;">

1.00

</td>

</tr>

</tbody>

</table>

\[1\] “Kendalls W is: 0.750336814578704”

\[1\] “Binary variable S7\_Un documento”

<table>

<thead>

<tr>

<th style="text-align:left;">

folio\_id

</th>

<th style="text-align:right;">

andrea

</th>

<th style="text-align:right;">

carmen

</th>

<th style="text-align:right;">

edithimg

</th>

<th style="text-align:right;">

jesicatapia

</th>

<th style="text-align:right;">

katia

</th>

<th style="text-align:right;">

sandra

</th>

<th style="text-align:right;">

rater\_var

</th>

<th style="text-align:right;">

num\_coders

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

’29725

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0.33

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’0001200073410

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0.33

</td>

<td style="text-align:right;">

4

</td>

</tr>

<tr>

<td style="text-align:left;">

’0063500015815

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0.33

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’0063700025011

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0.33

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’1412000005508

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.50

</td>

<td style="text-align:right;">

2

</td>

</tr>

</tbody>

</table>

<table>

<thead>

<tr>

<th style="text-align:left;">

</th>

<th style="text-align:right;">

ICC

</th>

<th style="text-align:right;">

p-value

</th>

<th style="text-align:right;">

lower CI limit

</th>

<th style="text-align:right;">

upper CI limit

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

ICC(1)

</td>

<td style="text-align:right;">

0.5952741

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.5364564

</td>

<td style="text-align:right;">

0.6501537

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(k)

</td>

<td style="text-align:right;">

0.7963557

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.7547195

</td>

<td style="text-align:right;">

0.8316791

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(A,1)

</td>

<td style="text-align:right;">

0.6055100

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.5475039

</td>

<td style="text-align:right;">

0.6594751

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(A,k)

</td>

<td style="text-align:right;">

0.8031874

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.7628619

</td>

<td style="text-align:right;">

0.8373752

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(C,1)

</td>

<td style="text-align:right;">

0.6045439

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.5465273

</td>

<td style="text-align:right;">

0.6585495

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(C,k)

</td>

<td style="text-align:right;">

0.8025476

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.7621509

</td>

<td style="text-align:right;">

0.8368120

</td>

</tr>

</tbody>

</table>

\[1\] “CORRELATION MATRIX”

<table>

<thead>

<tr>

<th style="text-align:left;">

</th>

<th style="text-align:right;">

andrea

</th>

<th style="text-align:right;">

carmen

</th>

<th style="text-align:right;">

edithimg

</th>

<th style="text-align:right;">

jesicatapia

</th>

<th style="text-align:right;">

katia

</th>

<th style="text-align:right;">

sandra

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

andrea

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

0.59

</td>

<td style="text-align:right;">

0.55

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

0.67

</td>

<td style="text-align:right;">

0.61

</td>

</tr>

<tr>

<td style="text-align:left;">

carmen

</td>

<td style="text-align:right;">

0.59

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

0.85

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

0.66

</td>

<td style="text-align:right;">

\-0.07

</td>

</tr>

<tr>

<td style="text-align:left;">

edithimg

</td>

<td style="text-align:right;">

0.55

</td>

<td style="text-align:right;">

0.85

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

0.65

</td>

<td style="text-align:right;">

0.69

</td>

<td style="text-align:right;">

0.44

</td>

</tr>

<tr>

<td style="text-align:left;">

jesicatapia

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

0.65

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

0.62

</td>

</tr>

<tr>

<td style="text-align:left;">

katia

</td>

<td style="text-align:right;">

0.67

</td>

<td style="text-align:right;">

0.66

</td>

<td style="text-align:right;">

0.69

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

0.58

</td>

</tr>

<tr>

<td style="text-align:left;">

sandra

</td>

<td style="text-align:right;">

0.61

</td>

<td style="text-align:right;">

\-0.07

</td>

<td style="text-align:right;">

0.44

</td>

<td style="text-align:right;">

0.62

</td>

<td style="text-align:right;">

0.58

</td>

<td style="text-align:right;">

1.00

</td>

</tr>

</tbody>

</table>

\[1\] “Kendalls W is: 0.755287751527495”

\[1\] “Binary variable S8\_¿cuál?”

<table>

<thead>

<tr>

<th style="text-align:left;">

folio\_id

</th>

<th style="text-align:right;">

andrea

</th>

<th style="text-align:right;">

carmen

</th>

<th style="text-align:right;">

edithimg

</th>

<th style="text-align:right;">

jesicatapia

</th>

<th style="text-align:right;">

katia

</th>

<th style="text-align:right;">

sandra

</th>

<th style="text-align:right;">

rater\_var

</th>

<th style="text-align:right;">

num\_coders

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

’0210000015405

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.33

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’1857600021710

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0.33

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’0000900247913

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0.33

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’0001200313314

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.50

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’1116100021513

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.33

</td>

<td style="text-align:right;">

3

</td>

</tr>

</tbody>

</table>

<table>

<thead>

<tr>

<th style="text-align:left;">

</th>

<th style="text-align:right;">

ICC

</th>

<th style="text-align:right;">

p-value

</th>

<th style="text-align:right;">

lower CI limit

</th>

<th style="text-align:right;">

upper CI limit

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

ICC(1)

</td>

<td style="text-align:right;">

0.1720856

</td>

<td style="text-align:right;">

1.6e-06

</td>

<td style="text-align:right;">

0.0980638

</td>

<td style="text-align:right;">

0.2483489

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(k)

</td>

<td style="text-align:right;">

0.3559334

</td>

<td style="text-align:right;">

1.6e-06

</td>

<td style="text-align:right;">

0.2242505

</td>

<td style="text-align:right;">

0.4676507

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(A,1)

</td>

<td style="text-align:right;">

0.1836840

</td>

<td style="text-align:right;">

3.0e-07

</td>

<td style="text-align:right;">

0.1101290

</td>

<td style="text-align:right;">

0.2593693

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(A,k)

</td>

<td style="text-align:right;">

0.3743206

</td>

<td style="text-align:right;">

3.0e-07

</td>

<td style="text-align:right;">

0.2476181

</td>

<td style="text-align:right;">

0.4821380

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(C,1)

</td>

<td style="text-align:right;">

0.1853422

</td>

<td style="text-align:right;">

3.0e-07

</td>

<td style="text-align:right;">

0.1111021

</td>

<td style="text-align:right;">

0.2615847

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(C,k)

</td>

<td style="text-align:right;">

0.3769052

</td>

<td style="text-align:right;">

3.0e-07

</td>

<td style="text-align:right;">

0.2494264

</td>

<td style="text-align:right;">

0.4850321

</td>

</tr>

</tbody>

</table>

\[1\] “CORRELATION MATRIX”

<table>

<thead>

<tr>

<th style="text-align:left;">

</th>

<th style="text-align:right;">

andrea

</th>

<th style="text-align:right;">

carmen

</th>

<th style="text-align:right;">

edithimg

</th>

<th style="text-align:right;">

jesicatapia

</th>

<th style="text-align:right;">

katia

</th>

<th style="text-align:right;">

sandra

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

andrea

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

0.40

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

0.19

</td>

<td style="text-align:right;">

NA

</td>

</tr>

<tr>

<td style="text-align:left;">

carmen

</td>

<td style="text-align:right;">

0.40

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

0.17

</td>

<td style="text-align:right;">

NA

</td>

</tr>

<tr>

<td style="text-align:left;">

edithimg

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

0.43

</td>

</tr>

<tr>

<td style="text-align:left;">

jesicatapia

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

NA

</td>

</tr>

<tr>

<td style="text-align:left;">

katia

</td>

<td style="text-align:right;">

0.19

</td>

<td style="text-align:right;">

0.17

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

NA

</td>

</tr>

<tr>

<td style="text-align:left;">

sandra

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

0.43

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

1.00

</td>

</tr>

</tbody>

</table>

\[1\] “Kendalls W is: 0.483631634650783”

\[1\] “Binary variable S8\_Actividades de la institución”

<table>

<thead>

<tr>

<th style="text-align:left;">

folio\_id

</th>

<th style="text-align:right;">

andrea

</th>

<th style="text-align:right;">

carmen

</th>

<th style="text-align:right;">

edithimg

</th>

<th style="text-align:right;">

jesicatapia

</th>

<th style="text-align:right;">

katia

</th>

<th style="text-align:right;">

sandra

</th>

<th style="text-align:right;">

rater\_var

</th>

<th style="text-align:right;">

num\_coders

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

’1026500043108

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0.50

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’0612100019614

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0.33

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’0064103230113

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0.33

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’1610100168713

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0.50

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’1215100061809

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0.33

</td>

<td style="text-align:right;">

3

</td>

</tr>

</tbody>

</table>

<table>

<thead>

<tr>

<th style="text-align:left;">

</th>

<th style="text-align:right;">

ICC

</th>

<th style="text-align:right;">

p-value

</th>

<th style="text-align:right;">

lower CI limit

</th>

<th style="text-align:right;">

upper CI limit

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

ICC(1)

</td>

<td style="text-align:right;">

0.4976562

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.4313846

</td>

<td style="text-align:right;">

0.5609217

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(k)

</td>

<td style="text-align:right;">

0.7248172

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.6685546

</td>

<td style="text-align:right;">

0.7725494

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(A,1)

</td>

<td style="text-align:right;">

0.4935171

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.4270087

</td>

<td style="text-align:right;">

0.5570598

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(A,k)

</td>

<td style="text-align:right;">

0.7215023

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.6640589

</td>

<td style="text-align:right;">

0.7700831

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(C,1)

</td>

<td style="text-align:right;">

0.5041481

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.4382552

</td>

<td style="text-align:right;">

0.5669468

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(C,k)

</td>

<td style="text-align:right;">

0.7299664

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.6747203

</td>

<td style="text-align:right;">

0.7768259

</td>

</tr>

</tbody>

</table>

\[1\] “CORRELATION MATRIX”

<table>

<thead>

<tr>

<th style="text-align:left;">

</th>

<th style="text-align:right;">

andrea

</th>

<th style="text-align:right;">

carmen

</th>

<th style="text-align:right;">

edithimg

</th>

<th style="text-align:right;">

jesicatapia

</th>

<th style="text-align:right;">

katia

</th>

<th style="text-align:right;">

sandra

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

andrea

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

0.59

</td>

<td style="text-align:right;">

0.51

</td>

<td style="text-align:right;">

0.50

</td>

<td style="text-align:right;">

0.46

</td>

<td style="text-align:right;">

0.45

</td>

</tr>

<tr>

<td style="text-align:left;">

carmen

</td>

<td style="text-align:right;">

0.59

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

0.55

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

0.48

</td>

<td style="text-align:right;">

0.67

</td>

</tr>

<tr>

<td style="text-align:left;">

edithimg

</td>

<td style="text-align:right;">

0.51

</td>

<td style="text-align:right;">

0.55

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

0.65

</td>

<td style="text-align:right;">

0.33

</td>

<td style="text-align:right;">

0.53

</td>

</tr>

<tr>

<td style="text-align:left;">

jesicatapia

</td>

<td style="text-align:right;">

0.50

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

0.65

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

0.50

</td>

<td style="text-align:right;">

0.43

</td>

</tr>

<tr>

<td style="text-align:left;">

katia

</td>

<td style="text-align:right;">

0.46

</td>

<td style="text-align:right;">

0.48

</td>

<td style="text-align:right;">

0.33

</td>

<td style="text-align:right;">

0.50

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

0.47

</td>

</tr>

<tr>

<td style="text-align:left;">

sandra

</td>

<td style="text-align:right;">

0.45

</td>

<td style="text-align:right;">

0.67

</td>

<td style="text-align:right;">

0.53

</td>

<td style="text-align:right;">

0.43

</td>

<td style="text-align:right;">

0.47

</td>

<td style="text-align:right;">

1.00

</td>

</tr>

</tbody>

</table>

\[1\] “Kendalls W is: 0.68888124551363”

\[1\] “Binary variable S8\_Contrataciones con externos”

<table>

<thead>

<tr>

<th style="text-align:left;">

folio\_id

</th>

<th style="text-align:right;">

andrea

</th>

<th style="text-align:right;">

carmen

</th>

<th style="text-align:right;">

edithimg

</th>

<th style="text-align:right;">

jesicatapia

</th>

<th style="text-align:right;">

katia

</th>

<th style="text-align:right;">

sandra

</th>

<th style="text-align:right;">

rater\_var

</th>

<th style="text-align:right;">

num\_coders

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

’0912100016013

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0.33

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’0610100092815

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.33

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’0064100319507

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0.50

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’0000900216813

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0.33

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’0912100065110

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0.33

</td>

<td style="text-align:right;">

3

</td>

</tr>

</tbody>

</table>

<table>

<thead>

<tr>

<th style="text-align:left;">

</th>

<th style="text-align:right;">

ICC

</th>

<th style="text-align:right;">

p-value

</th>

<th style="text-align:right;">

lower CI limit

</th>

<th style="text-align:right;">

upper CI limit

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

ICC(1)

</td>

<td style="text-align:right;">

0.7155751

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.6694165

</td>

<td style="text-align:right;">

0.7574446

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(k)

</td>

<td style="text-align:right;">

0.8699454

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.8433551

</td>

<td style="text-align:right;">

0.8925042

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(A,1)

</td>

<td style="text-align:right;">

0.7182068

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.6724491

</td>

<td style="text-align:right;">

0.7597073

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(A,k)

</td>

<td style="text-align:right;">

0.8714055

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.8451627

</td>

<td style="text-align:right;">

0.8936829

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(C,1)

</td>

<td style="text-align:right;">

0.7190604

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.6732984

</td>

<td style="text-align:right;">

0.7605288

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(C,k)

</td>

<td style="text-align:right;">

0.8718778

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.8456654

</td>

<td style="text-align:right;">

0.8941111

</td>

</tr>

</tbody>

</table>

\[1\] “CORRELATION MATRIX”

<table>

<thead>

<tr>

<th style="text-align:left;">

</th>

<th style="text-align:right;">

andrea

</th>

<th style="text-align:right;">

carmen

</th>

<th style="text-align:right;">

edithimg

</th>

<th style="text-align:right;">

jesicatapia

</th>

<th style="text-align:right;">

katia

</th>

<th style="text-align:right;">

sandra

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

andrea

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

0.72

</td>

<td style="text-align:right;">

0.52

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

0.78

</td>

<td style="text-align:right;">

0.88

</td>

</tr>

<tr>

<td style="text-align:left;">

carmen

</td>

<td style="text-align:right;">

0.72

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

0.55

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

0.74

</td>

<td style="text-align:right;">

0.87

</td>

</tr>

<tr>

<td style="text-align:left;">

edithimg

</td>

<td style="text-align:right;">

0.52

</td>

<td style="text-align:right;">

0.55

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

0.54

</td>

<td style="text-align:right;">

0.66

</td>

<td style="text-align:right;">

0.69

</td>

</tr>

<tr>

<td style="text-align:left;">

jesicatapia

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

0.54

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

0.65

</td>

</tr>

<tr>

<td style="text-align:left;">

katia

</td>

<td style="text-align:right;">

0.78

</td>

<td style="text-align:right;">

0.74

</td>

<td style="text-align:right;">

0.66

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

0.83

</td>

</tr>

<tr>

<td style="text-align:left;">

sandra

</td>

<td style="text-align:right;">

0.88

</td>

<td style="text-align:right;">

0.87

</td>

<td style="text-align:right;">

0.69

</td>

<td style="text-align:right;">

0.65

</td>

<td style="text-align:right;">

0.83

</td>

<td style="text-align:right;">

1.00

</td>

</tr>

</tbody>

</table>

\[1\] “Kendalls W is: 0.825821100757579”

\[1\] “Binary variable S8\_estadísticas y resultados”

<table>

<thead>

<tr>

<th style="text-align:left;">

folio\_id

</th>

<th style="text-align:right;">

andrea

</th>

<th style="text-align:right;">

carmen

</th>

<th style="text-align:right;">

edithimg

</th>

<th style="text-align:right;">

jesicatapia

</th>

<th style="text-align:right;">

katia

</th>

<th style="text-align:right;">

sandra

</th>

<th style="text-align:right;">

rater\_var

</th>

<th style="text-align:right;">

num\_coders

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

’1610100027310

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0.33

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’0001700078810

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0.33

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’1411100030410

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0.33

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’0001100373615

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0.33

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’1111200046115

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.50

</td>

<td style="text-align:right;">

2

</td>

</tr>

</tbody>

</table>

<table>

<thead>

<tr>

<th style="text-align:left;">

</th>

<th style="text-align:right;">

ICC

</th>

<th style="text-align:right;">

p-value

</th>

<th style="text-align:right;">

lower CI limit

</th>

<th style="text-align:right;">

upper CI limit

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

ICC(1)

</td>

<td style="text-align:right;">

0.4181165

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.3475724

</td>

<td style="text-align:right;">

0.4867272

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(k)

</td>

<td style="text-align:right;">

0.6564128

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.5861646

</td>

<td style="text-align:right;">

0.7160101

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(A,1)

</td>

<td style="text-align:right;">

0.4295172

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.3600493

</td>

<td style="text-align:right;">

0.4970159

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(A,k)

</td>

<td style="text-align:right;">

0.6668646

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.5991347

</td>

<td style="text-align:right;">

0.7244221

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(C,1)

</td>

<td style="text-align:right;">

0.4355892

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.3658050

</td>

<td style="text-align:right;">

0.5031738

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(C,k)

</td>

<td style="text-align:right;">

0.6723375

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.6053012

</td>

<td style="text-align:right;">

0.7291975

</td>

</tr>

</tbody>

</table>

\[1\] “CORRELATION MATRIX”

<table>

<thead>

<tr>

<th style="text-align:left;">

</th>

<th style="text-align:right;">

andrea

</th>

<th style="text-align:right;">

carmen

</th>

<th style="text-align:right;">

edithimg

</th>

<th style="text-align:right;">

jesicatapia

</th>

<th style="text-align:right;">

katia

</th>

<th style="text-align:right;">

sandra

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

andrea

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

0.33

</td>

<td style="text-align:right;">

0.54

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

0.38

</td>

<td style="text-align:right;">

0.63

</td>

</tr>

<tr>

<td style="text-align:left;">

carmen

</td>

<td style="text-align:right;">

0.33

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

0.68

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

0.03

</td>

<td style="text-align:right;">

NA

</td>

</tr>

<tr>

<td style="text-align:left;">

edithimg

</td>

<td style="text-align:right;">

0.54

</td>

<td style="text-align:right;">

0.68

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

0.69

</td>

<td style="text-align:right;">

0.45

</td>

</tr>

<tr>

<td style="text-align:left;">

jesicatapia

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

0.67

</td>

</tr>

<tr>

<td style="text-align:left;">

katia

</td>

<td style="text-align:right;">

0.38

</td>

<td style="text-align:right;">

0.03

</td>

<td style="text-align:right;">

0.69

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

0.77

</td>

</tr>

<tr>

<td style="text-align:left;">

sandra

</td>

<td style="text-align:right;">

0.63

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

0.45

</td>

<td style="text-align:right;">

0.67

</td>

<td style="text-align:right;">

0.77

</td>

<td style="text-align:right;">

1.00

</td>

</tr>

</tbody>

</table>

\[1\] “Kendalls W is: 0.635980505180514”

\[1\] “Binary variable S8\_Estructura de la institución”

<table>

<thead>

<tr>

<th style="text-align:left;">

folio\_id

</th>

<th style="text-align:right;">

andrea

</th>

<th style="text-align:right;">

carmen

</th>

<th style="text-align:right;">

edithimg

</th>

<th style="text-align:right;">

jesicatapia

</th>

<th style="text-align:right;">

katia

</th>

<th style="text-align:right;">

sandra

</th>

<th style="text-align:right;">

rater\_var

</th>

<th style="text-align:right;">

num\_coders

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

’1111200007709

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0.33

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’1117100000109

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0.50

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’2510100061415

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0.33

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’0001100171309

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0.33

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’0002700040515

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0.33

</td>

<td style="text-align:right;">

3

</td>

</tr>

</tbody>

</table>

<table>

<thead>

<tr>

<th style="text-align:left;">

</th>

<th style="text-align:right;">

ICC

</th>

<th style="text-align:right;">

p-value

</th>

<th style="text-align:right;">

lower CI limit

</th>

<th style="text-align:right;">

upper CI limit

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

ICC(1)

</td>

<td style="text-align:right;">

0.6638601

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.6117801

</td>

<td style="text-align:right;">

0.7116776

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(k)

</td>

<td style="text-align:right;">

0.8400235

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.8073155

</td>

<td style="text-align:right;">

0.8677724

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(A,1)

</td>

<td style="text-align:right;">

0.6637443

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.6118085

</td>

<td style="text-align:right;">

0.7114679

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(A,k)

</td>

<td style="text-align:right;">

0.8399538

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.8073149

</td>

<td style="text-align:right;">

0.8676660

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(C,1)

</td>

<td style="text-align:right;">

0.6670166

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.6152445

</td>

<td style="text-align:right;">

0.7145079

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(C,k)

</td>

<td style="text-align:right;">

0.8419197

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.8095781

</td>

<td style="text-align:right;">

0.8693517

</td>

</tr>

</tbody>

</table>

\[1\] “CORRELATION MATRIX”

<table>

<thead>

<tr>

<th style="text-align:left;">

</th>

<th style="text-align:right;">

andrea

</th>

<th style="text-align:right;">

carmen

</th>

<th style="text-align:right;">

edithimg

</th>

<th style="text-align:right;">

jesicatapia

</th>

<th style="text-align:right;">

katia

</th>

<th style="text-align:right;">

sandra

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

andrea

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

0.65

</td>

<td style="text-align:right;">

0.73

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

0.63

</td>

<td style="text-align:right;">

0.71

</td>

</tr>

<tr>

<td style="text-align:left;">

carmen

</td>

<td style="text-align:right;">

0.65

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

0.58

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

0.55

</td>

<td style="text-align:right;">

1.00

</td>

</tr>

<tr>

<td style="text-align:left;">

edithimg

</td>

<td style="text-align:right;">

0.73

</td>

<td style="text-align:right;">

0.58

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

0.57

</td>

<td style="text-align:right;">

0.87

</td>

</tr>

<tr>

<td style="text-align:left;">

jesicatapia

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

0.66

</td>

</tr>

<tr>

<td style="text-align:left;">

katia

</td>

<td style="text-align:right;">

0.63

</td>

<td style="text-align:right;">

0.55

</td>

<td style="text-align:right;">

0.57

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

0.59

</td>

</tr>

<tr>

<td style="text-align:left;">

sandra

</td>

<td style="text-align:right;">

0.71

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

0.87

</td>

<td style="text-align:right;">

0.66

</td>

<td style="text-align:right;">

0.59

</td>

<td style="text-align:right;">

1.00

</td>

</tr>

</tbody>

</table>

\[1\] “Kendalls W is: 0.785290245257444”

\[1\] “Binary variable S8\_Evaluaciones”

<table>

<thead>

<tr>

<th style="text-align:left;">

folio\_id

</th>

<th style="text-align:right;">

andrea

</th>

<th style="text-align:right;">

carmen

</th>

<th style="text-align:right;">

edithimg

</th>

<th style="text-align:right;">

jesicatapia

</th>

<th style="text-align:right;">

katia

</th>

<th style="text-align:right;">

sandra

</th>

<th style="text-align:right;">

rater\_var

</th>

<th style="text-align:right;">

num\_coders

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

’1610100091713

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0.33

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’0610100064610

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0.33

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’1215100138911

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0.33

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’1111200007709

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0.33

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’0064103030312

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0.33

</td>

<td style="text-align:right;">

3

</td>

</tr>

</tbody>

</table>

<table>

<thead>

<tr>

<th style="text-align:left;">

</th>

<th style="text-align:right;">

ICC

</th>

<th style="text-align:right;">

p-value

</th>

<th style="text-align:right;">

lower CI limit

</th>

<th style="text-align:right;">

upper CI limit

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

ICC(1)

</td>

<td style="text-align:right;">

0.4181165

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.3475724

</td>

<td style="text-align:right;">

0.4867272

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(k)

</td>

<td style="text-align:right;">

0.6564128

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.5861646

</td>

<td style="text-align:right;">

0.7160101

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(A,1)

</td>

<td style="text-align:right;">

0.4295172

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.3600493

</td>

<td style="text-align:right;">

0.4970159

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(A,k)

</td>

<td style="text-align:right;">

0.6668646

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.5991347

</td>

<td style="text-align:right;">

0.7244221

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(C,1)

</td>

<td style="text-align:right;">

0.4355892

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.3658050

</td>

<td style="text-align:right;">

0.5031738

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(C,k)

</td>

<td style="text-align:right;">

0.6723375

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.6053012

</td>

<td style="text-align:right;">

0.7291975

</td>

</tr>

</tbody>

</table>

\[1\] “CORRELATION MATRIX”

<table>

<thead>

<tr>

<th style="text-align:left;">

</th>

<th style="text-align:right;">

andrea

</th>

<th style="text-align:right;">

carmen

</th>

<th style="text-align:right;">

edithimg

</th>

<th style="text-align:right;">

jesicatapia

</th>

<th style="text-align:right;">

katia

</th>

<th style="text-align:right;">

sandra

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

andrea

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

0.33

</td>

<td style="text-align:right;">

0.54

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

0.38

</td>

<td style="text-align:right;">

0.63

</td>

</tr>

<tr>

<td style="text-align:left;">

carmen

</td>

<td style="text-align:right;">

0.33

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

0.68

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

0.03

</td>

<td style="text-align:right;">

NA

</td>

</tr>

<tr>

<td style="text-align:left;">

edithimg

</td>

<td style="text-align:right;">

0.54

</td>

<td style="text-align:right;">

0.68

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

0.69

</td>

<td style="text-align:right;">

0.45

</td>

</tr>

<tr>

<td style="text-align:left;">

jesicatapia

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

0.67

</td>

</tr>

<tr>

<td style="text-align:left;">

katia

</td>

<td style="text-align:right;">

0.38

</td>

<td style="text-align:right;">

0.03

</td>

<td style="text-align:right;">

0.69

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

0.77

</td>

</tr>

<tr>

<td style="text-align:left;">

sandra

</td>

<td style="text-align:right;">

0.63

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

0.45

</td>

<td style="text-align:right;">

0.67

</td>

<td style="text-align:right;">

0.77

</td>

<td style="text-align:right;">

1.00

</td>

</tr>

</tbody>

</table>

\[1\] “Kendalls W is: 0.635980505180514”

\[1\] “Binary variable S8\_gasto”

<table>

<thead>

<tr>

<th style="text-align:left;">

folio\_id

</th>

<th style="text-align:right;">

andrea

</th>

<th style="text-align:right;">

carmen

</th>

<th style="text-align:right;">

edithimg

</th>

<th style="text-align:right;">

jesicatapia

</th>

<th style="text-align:right;">

katia

</th>

<th style="text-align:right;">

sandra

</th>

<th style="text-align:right;">

rater\_var

</th>

<th style="text-align:right;">

num\_coders

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

’0000400209014

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0.33

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’0064101584110

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0.50

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’0610100023908

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0.33

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’2014300001807

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0.33

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’2135500004809

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0.33

</td>

<td style="text-align:right;">

3

</td>

</tr>

</tbody>

</table>

<table>

<thead>

<tr>

<th style="text-align:left;">

</th>

<th style="text-align:right;">

ICC

</th>

<th style="text-align:right;">

p-value

</th>

<th style="text-align:right;">

lower CI limit

</th>

<th style="text-align:right;">

upper CI limit

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

ICC(1)

</td>

<td style="text-align:right;">

0.6378552

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.5830723

</td>

<td style="text-align:right;">

0.6884622

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(k)

</td>

<td style="text-align:right;">

0.8240348

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.7880577

</td>

<td style="text-align:right;">

0.8545570

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(A,1)

</td>

<td style="text-align:right;">

0.6390297

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.5844958

</td>

<td style="text-align:right;">

0.6894245

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(A,k)

</td>

<td style="text-align:right;">

0.8247713

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.7890361

</td>

<td style="text-align:right;">

0.8551133

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(C,1)

</td>

<td style="text-align:right;">

0.6403935

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.5858322

</td>

<td style="text-align:right;">

0.6907572

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(C,k)

</td>

<td style="text-align:right;">

0.8256249

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.7899496

</td>

<td style="text-align:right;">

0.8558846

</td>

</tr>

</tbody>

</table>

\[1\] “CORRELATION MATRIX”

<table>

<thead>

<tr>

<th style="text-align:left;">

</th>

<th style="text-align:right;">

andrea

</th>

<th style="text-align:right;">

carmen

</th>

<th style="text-align:right;">

edithimg

</th>

<th style="text-align:right;">

jesicatapia

</th>

<th style="text-align:right;">

katia

</th>

<th style="text-align:right;">

sandra

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

andrea

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

0.54

</td>

<td style="text-align:right;">

0.42

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

0.57

</td>

<td style="text-align:right;">

\-0.10

</td>

</tr>

<tr>

<td style="text-align:left;">

carmen

</td>

<td style="text-align:right;">

0.54

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

0.72

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

0.68

</td>

<td style="text-align:right;">

1.00

</td>

</tr>

<tr>

<td style="text-align:left;">

edithimg

</td>

<td style="text-align:right;">

0.42

</td>

<td style="text-align:right;">

0.72

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

0.71

</td>

<td style="text-align:right;">

0.76

</td>

<td style="text-align:right;">

0.84

</td>

</tr>

<tr>

<td style="text-align:left;">

jesicatapia

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

0.71

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

0.69

</td>

</tr>

<tr>

<td style="text-align:left;">

katia

</td>

<td style="text-align:right;">

0.57

</td>

<td style="text-align:right;">

0.68

</td>

<td style="text-align:right;">

0.76

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

0.60

</td>

</tr>

<tr>

<td style="text-align:left;">

sandra

</td>

<td style="text-align:right;">

\-0.10

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

0.84

</td>

<td style="text-align:right;">

0.69

</td>

<td style="text-align:right;">

0.60

</td>

<td style="text-align:right;">

1.00

</td>

</tr>

</tbody>

</table>

\[1\] “Kendalls W is: 0.751892413531897”

\[1\] “Binary variable S8\_Otro”

<table>

<thead>

<tr>

<th style="text-align:left;">

folio\_id

</th>

<th style="text-align:right;">

andrea

</th>

<th style="text-align:right;">

carmen

</th>

<th style="text-align:right;">

edithimg

</th>

<th style="text-align:right;">

jesicatapia

</th>

<th style="text-align:right;">

katia

</th>

<th style="text-align:right;">

sandra

</th>

<th style="text-align:right;">

rater\_var

</th>

<th style="text-align:right;">

num\_coders

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

’0000900247913

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0.33

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’0001100570007

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0.33

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’1615100031912

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0.33

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’0000600149312

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0.33

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’1857200172714

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.50

</td>

<td style="text-align:right;">

2

</td>

</tr>

</tbody>

</table>

<table>

<thead>

<tr>

<th style="text-align:left;">

</th>

<th style="text-align:right;">

ICC

</th>

<th style="text-align:right;">

p-value

</th>

<th style="text-align:right;">

lower CI limit

</th>

<th style="text-align:right;">

upper CI limit

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

ICC(1)

</td>

<td style="text-align:right;">

0.1720856

</td>

<td style="text-align:right;">

1.6e-06

</td>

<td style="text-align:right;">

0.0980638

</td>

<td style="text-align:right;">

0.2483489

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(k)

</td>

<td style="text-align:right;">

0.3559334

</td>

<td style="text-align:right;">

1.6e-06

</td>

<td style="text-align:right;">

0.2242505

</td>

<td style="text-align:right;">

0.4676507

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(A,1)

</td>

<td style="text-align:right;">

0.1836840

</td>

<td style="text-align:right;">

3.0e-07

</td>

<td style="text-align:right;">

0.1101290

</td>

<td style="text-align:right;">

0.2593693

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(A,k)

</td>

<td style="text-align:right;">

0.3743206

</td>

<td style="text-align:right;">

3.0e-07

</td>

<td style="text-align:right;">

0.2476181

</td>

<td style="text-align:right;">

0.4821380

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(C,1)

</td>

<td style="text-align:right;">

0.1853422

</td>

<td style="text-align:right;">

3.0e-07

</td>

<td style="text-align:right;">

0.1111021

</td>

<td style="text-align:right;">

0.2615847

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(C,k)

</td>

<td style="text-align:right;">

0.3769052

</td>

<td style="text-align:right;">

3.0e-07

</td>

<td style="text-align:right;">

0.2494264

</td>

<td style="text-align:right;">

0.4850321

</td>

</tr>

</tbody>

</table>

\[1\] “CORRELATION MATRIX”

<table>

<thead>

<tr>

<th style="text-align:left;">

</th>

<th style="text-align:right;">

andrea

</th>

<th style="text-align:right;">

carmen

</th>

<th style="text-align:right;">

edithimg

</th>

<th style="text-align:right;">

jesicatapia

</th>

<th style="text-align:right;">

katia

</th>

<th style="text-align:right;">

sandra

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

andrea

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

0.40

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

0.19

</td>

<td style="text-align:right;">

NA

</td>

</tr>

<tr>

<td style="text-align:left;">

carmen

</td>

<td style="text-align:right;">

0.40

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

0.17

</td>

<td style="text-align:right;">

NA

</td>

</tr>

<tr>

<td style="text-align:left;">

edithimg

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

0.43

</td>

</tr>

<tr>

<td style="text-align:left;">

jesicatapia

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

NA

</td>

</tr>

<tr>

<td style="text-align:left;">

katia

</td>

<td style="text-align:right;">

0.19

</td>

<td style="text-align:right;">

0.17

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

NA

</td>

</tr>

<tr>

<td style="text-align:left;">

sandra

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

0.43

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

1.00

</td>

</tr>

</tbody>

</table>

\[1\] “Kendalls W is: 0.483631634650783”

\[1\] “Binary variable S8\_permisos”

<table>

<thead>

<tr>

<th style="text-align:left;">

folio\_id

</th>

<th style="text-align:right;">

andrea

</th>

<th style="text-align:right;">

carmen

</th>

<th style="text-align:right;">

edithimg

</th>

<th style="text-align:right;">

jesicatapia

</th>

<th style="text-align:right;">

katia

</th>

<th style="text-align:right;">

sandra

</th>

<th style="text-align:right;">

rater\_var

</th>

<th style="text-align:right;">

num\_coders

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

’0002700053513

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.50

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’1511100015108

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0.33

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’0002700040515

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0.33

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’0001200067405

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0.33

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’0000700001309

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0.33

</td>

<td style="text-align:right;">

3

</td>

</tr>

</tbody>

</table>

<table>

<thead>

<tr>

<th style="text-align:left;">

</th>

<th style="text-align:right;">

ICC

</th>

<th style="text-align:right;">

p-value

</th>

<th style="text-align:right;">

lower CI limit

</th>

<th style="text-align:right;">

upper CI limit

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

ICC(1)

</td>

<td style="text-align:right;">

0.4058433

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.3347807

</td>

<td style="text-align:right;">

0.4751572

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(k)

</td>

<td style="text-align:right;">

0.6448971

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.5722943

</td>

<td style="text-align:right;">

0.7064918

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(A,1)

</td>

<td style="text-align:right;">

0.4220531

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.3522027

</td>

<td style="text-align:right;">

0.4900348

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(A,k)

</td>

<td style="text-align:right;">

0.6600481

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.5909993

</td>

<td style="text-align:right;">

0.7187478

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(C,1)

</td>

<td style="text-align:right;">

0.4264757

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.3562632

</td>

<td style="text-align:right;">

0.4946206

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(C,k)

</td>

<td style="text-align:right;">

0.6640989

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.5953770

</td>

<td style="text-align:right;">

0.7223885

</td>

</tr>

</tbody>

</table>

\[1\] “CORRELATION MATRIX”

<table>

<thead>

<tr>

<th style="text-align:left;">

</th>

<th style="text-align:right;">

andrea

</th>

<th style="text-align:right;">

carmen

</th>

<th style="text-align:right;">

edithimg

</th>

<th style="text-align:right;">

jesicatapia

</th>

<th style="text-align:right;">

katia

</th>

<th style="text-align:right;">

sandra

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

andrea

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

0.44

</td>

<td style="text-align:right;">

0.56

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

0.37

</td>

<td style="text-align:right;">

0.56

</td>

</tr>

<tr>

<td style="text-align:left;">

carmen

</td>

<td style="text-align:right;">

0.44

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

0.63

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

0.44

</td>

<td style="text-align:right;">

NA

</td>

</tr>

<tr>

<td style="text-align:left;">

edithimg

</td>

<td style="text-align:right;">

0.56

</td>

<td style="text-align:right;">

0.63

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

0.45

</td>

<td style="text-align:right;">

0.54

</td>

<td style="text-align:right;">

0.61

</td>

</tr>

<tr>

<td style="text-align:left;">

jesicatapia

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

0.45

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

0.42

</td>

</tr>

<tr>

<td style="text-align:left;">

katia

</td>

<td style="text-align:right;">

0.37

</td>

<td style="text-align:right;">

0.44

</td>

<td style="text-align:right;">

0.54

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

0.27

</td>

</tr>

<tr>

<td style="text-align:left;">

sandra

</td>

<td style="text-align:right;">

0.56

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

0.61

</td>

<td style="text-align:right;">

0.42

</td>

<td style="text-align:right;">

0.27

</td>

<td style="text-align:right;">

1.00

</td>

</tr>

</tbody>

</table>

\[1\] “Kendalls W is: 0.645740947665241”

\[1\] “Binary variable S8\_personal”

<table>

<thead>

<tr>

<th style="text-align:left;">

folio\_id

</th>

<th style="text-align:right;">

andrea

</th>

<th style="text-align:right;">

carmen

</th>

<th style="text-align:right;">

edithimg

</th>

<th style="text-align:right;">

jesicatapia

</th>

<th style="text-align:right;">

katia

</th>

<th style="text-align:right;">

sandra

</th>

<th style="text-align:right;">

rater\_var

</th>

<th style="text-align:right;">

num\_coders

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

’0000800101312

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0.33

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’0674700008307

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0.33

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’0000900005113

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0.33

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’0064101095409

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0.33

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’0001100570007

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0.33

</td>

<td style="text-align:right;">

3

</td>

</tr>

</tbody>

</table>

<table>

<thead>

<tr>

<th style="text-align:left;">

</th>

<th style="text-align:right;">

ICC

</th>

<th style="text-align:right;">

p-value

</th>

<th style="text-align:right;">

lower CI limit

</th>

<th style="text-align:right;">

upper CI limit

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

ICC(1)

</td>

<td style="text-align:right;">

0.6638601

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.6117801

</td>

<td style="text-align:right;">

0.7116776

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(k)

</td>

<td style="text-align:right;">

0.8400235

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.8073155

</td>

<td style="text-align:right;">

0.8677724

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(A,1)

</td>

<td style="text-align:right;">

0.6637443

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.6118085

</td>

<td style="text-align:right;">

0.7114679

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(A,k)

</td>

<td style="text-align:right;">

0.8399538

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.8073149

</td>

<td style="text-align:right;">

0.8676660

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(C,1)

</td>

<td style="text-align:right;">

0.6670166

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.6152445

</td>

<td style="text-align:right;">

0.7145079

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(C,k)

</td>

<td style="text-align:right;">

0.8419197

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.8095781

</td>

<td style="text-align:right;">

0.8693517

</td>

</tr>

</tbody>

</table>

\[1\] “CORRELATION MATRIX”

<table>

<thead>

<tr>

<th style="text-align:left;">

</th>

<th style="text-align:right;">

andrea

</th>

<th style="text-align:right;">

carmen

</th>

<th style="text-align:right;">

edithimg

</th>

<th style="text-align:right;">

jesicatapia

</th>

<th style="text-align:right;">

katia

</th>

<th style="text-align:right;">

sandra

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

andrea

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

0.65

</td>

<td style="text-align:right;">

0.73

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

0.63

</td>

<td style="text-align:right;">

0.71

</td>

</tr>

<tr>

<td style="text-align:left;">

carmen

</td>

<td style="text-align:right;">

0.65

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

0.58

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

0.55

</td>

<td style="text-align:right;">

1.00

</td>

</tr>

<tr>

<td style="text-align:left;">

edithimg

</td>

<td style="text-align:right;">

0.73

</td>

<td style="text-align:right;">

0.58

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

0.57

</td>

<td style="text-align:right;">

0.87

</td>

</tr>

<tr>

<td style="text-align:left;">

jesicatapia

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

0.66

</td>

</tr>

<tr>

<td style="text-align:left;">

katia

</td>

<td style="text-align:right;">

0.63

</td>

<td style="text-align:right;">

0.55

</td>

<td style="text-align:right;">

0.57

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

0.59

</td>

</tr>

<tr>

<td style="text-align:left;">

sandra

</td>

<td style="text-align:right;">

0.71

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

0.87

</td>

<td style="text-align:right;">

0.66

</td>

<td style="text-align:right;">

0.59

</td>

<td style="text-align:right;">

1.00

</td>

</tr>

</tbody>

</table>

\[1\] “Kendalls W is: 0.785290245257444”

\[1\] “Binary variable S8\_Presupuesto”

<table>

<thead>

<tr>

<th style="text-align:left;">

folio\_id

</th>

<th style="text-align:right;">

andrea

</th>

<th style="text-align:right;">

carmen

</th>

<th style="text-align:right;">

edithimg

</th>

<th style="text-align:right;">

jesicatapia

</th>

<th style="text-align:right;">

katia

</th>

<th style="text-align:right;">

sandra

</th>

<th style="text-align:right;">

rater\_var

</th>

<th style="text-align:right;">

num\_coders

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

’0064102522611

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0.33

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’0001200073410

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0.25

</td>

<td style="text-align:right;">

4

</td>

</tr>

<tr>

<td style="text-align:left;">

’0210000030806

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0.33

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’2014300001807

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0.33

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’0002000005615

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0.33

</td>

<td style="text-align:right;">

3

</td>

</tr>

</tbody>

</table>

<table>

<thead>

<tr>

<th style="text-align:left;">

</th>

<th style="text-align:right;">

ICC

</th>

<th style="text-align:right;">

p-value

</th>

<th style="text-align:right;">

lower CI limit

</th>

<th style="text-align:right;">

upper CI limit

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

ICC(1)

</td>

<td style="text-align:right;">

0.6378552

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.5830723

</td>

<td style="text-align:right;">

0.6884622

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(k)

</td>

<td style="text-align:right;">

0.8240348

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.7880577

</td>

<td style="text-align:right;">

0.8545570

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(A,1)

</td>

<td style="text-align:right;">

0.6390297

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.5844958

</td>

<td style="text-align:right;">

0.6894245

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(A,k)

</td>

<td style="text-align:right;">

0.8247713

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.7890361

</td>

<td style="text-align:right;">

0.8551133

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(C,1)

</td>

<td style="text-align:right;">

0.6403935

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.5858322

</td>

<td style="text-align:right;">

0.6907572

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(C,k)

</td>

<td style="text-align:right;">

0.8256249

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.7899496

</td>

<td style="text-align:right;">

0.8558846

</td>

</tr>

</tbody>

</table>

\[1\] “CORRELATION MATRIX”

<table>

<thead>

<tr>

<th style="text-align:left;">

</th>

<th style="text-align:right;">

andrea

</th>

<th style="text-align:right;">

carmen

</th>

<th style="text-align:right;">

edithimg

</th>

<th style="text-align:right;">

jesicatapia

</th>

<th style="text-align:right;">

katia

</th>

<th style="text-align:right;">

sandra

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

andrea

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

0.54

</td>

<td style="text-align:right;">

0.42

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

0.57

</td>

<td style="text-align:right;">

\-0.10

</td>

</tr>

<tr>

<td style="text-align:left;">

carmen

</td>

<td style="text-align:right;">

0.54

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

0.72

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

0.68

</td>

<td style="text-align:right;">

1.00

</td>

</tr>

<tr>

<td style="text-align:left;">

edithimg

</td>

<td style="text-align:right;">

0.42

</td>

<td style="text-align:right;">

0.72

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

0.71

</td>

<td style="text-align:right;">

0.76

</td>

<td style="text-align:right;">

0.84

</td>

</tr>

<tr>

<td style="text-align:left;">

jesicatapia

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

0.71

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

0.69

</td>

</tr>

<tr>

<td style="text-align:left;">

katia

</td>

<td style="text-align:right;">

0.57

</td>

<td style="text-align:right;">

0.68

</td>

<td style="text-align:right;">

0.76

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

0.60

</td>

</tr>

<tr>

<td style="text-align:left;">

sandra

</td>

<td style="text-align:right;">

\-0.10

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

0.84

</td>

<td style="text-align:right;">

0.69

</td>

<td style="text-align:right;">

0.60

</td>

<td style="text-align:right;">

1.00

</td>

</tr>

</tbody>

</table>

\[1\] “Kendalls W is: 0.751892413531897”

\[1\] “Binary variable S8\_recursos humanos”

<table>

<thead>

<tr>

<th style="text-align:left;">

folio\_id

</th>

<th style="text-align:right;">

andrea

</th>

<th style="text-align:right;">

carmen

</th>

<th style="text-align:right;">

edithimg

</th>

<th style="text-align:right;">

jesicatapia

</th>

<th style="text-align:right;">

katia

</th>

<th style="text-align:right;">

sandra

</th>

<th style="text-align:right;">

rater\_var

</th>

<th style="text-align:right;">

num\_coders

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

’0064101996410

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.50

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’0001300003707

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.50

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’1026500013908

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0.33

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’0064100522913

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0.33

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’0610100066912

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0.33

</td>

<td style="text-align:right;">

3

</td>

</tr>

</tbody>

</table>

<table>

<thead>

<tr>

<th style="text-align:left;">

</th>

<th style="text-align:right;">

ICC

</th>

<th style="text-align:right;">

p-value

</th>

<th style="text-align:right;">

lower CI limit

</th>

<th style="text-align:right;">

upper CI limit

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

ICC(1)

</td>

<td style="text-align:right;">

0.6638601

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.6117801

</td>

<td style="text-align:right;">

0.7116776

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(k)

</td>

<td style="text-align:right;">

0.8400235

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.8073155

</td>

<td style="text-align:right;">

0.8677724

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(A,1)

</td>

<td style="text-align:right;">

0.6637443

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.6118085

</td>

<td style="text-align:right;">

0.7114679

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(A,k)

</td>

<td style="text-align:right;">

0.8399538

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.8073149

</td>

<td style="text-align:right;">

0.8676660

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(C,1)

</td>

<td style="text-align:right;">

0.6670166

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.6152445

</td>

<td style="text-align:right;">

0.7145079

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(C,k)

</td>

<td style="text-align:right;">

0.8419197

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.8095781

</td>

<td style="text-align:right;">

0.8693517

</td>

</tr>

</tbody>

</table>

\[1\] “CORRELATION MATRIX”

<table>

<thead>

<tr>

<th style="text-align:left;">

</th>

<th style="text-align:right;">

andrea

</th>

<th style="text-align:right;">

carmen

</th>

<th style="text-align:right;">

edithimg

</th>

<th style="text-align:right;">

jesicatapia

</th>

<th style="text-align:right;">

katia

</th>

<th style="text-align:right;">

sandra

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

andrea

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

0.65

</td>

<td style="text-align:right;">

0.73

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

0.63

</td>

<td style="text-align:right;">

0.71

</td>

</tr>

<tr>

<td style="text-align:left;">

carmen

</td>

<td style="text-align:right;">

0.65

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

0.58

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

0.55

</td>

<td style="text-align:right;">

1.00

</td>

</tr>

<tr>

<td style="text-align:left;">

edithimg

</td>

<td style="text-align:right;">

0.73

</td>

<td style="text-align:right;">

0.58

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

0.57

</td>

<td style="text-align:right;">

0.87

</td>

</tr>

<tr>

<td style="text-align:left;">

jesicatapia

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

0.66

</td>

</tr>

<tr>

<td style="text-align:left;">

katia

</td>

<td style="text-align:right;">

0.63

</td>

<td style="text-align:right;">

0.55

</td>

<td style="text-align:right;">

0.57

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

0.59

</td>

</tr>

<tr>

<td style="text-align:left;">

sandra

</td>

<td style="text-align:right;">

0.71

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

0.87

</td>

<td style="text-align:right;">

0.66

</td>

<td style="text-align:right;">

0.59

</td>

<td style="text-align:right;">

1.00

</td>

</tr>

</tbody>

</table>

\[1\] “Kendalls W is: 0.785290245257444”

\[1\] “Binary variable S8\_Regulatorio”

<table>

<thead>

<tr>

<th style="text-align:left;">

folio\_id

</th>

<th style="text-align:right;">

andrea

</th>

<th style="text-align:right;">

carmen

</th>

<th style="text-align:right;">

edithimg

</th>

<th style="text-align:right;">

jesicatapia

</th>

<th style="text-align:right;">

katia

</th>

<th style="text-align:right;">

sandra

</th>

<th style="text-align:right;">

rater\_var

</th>

<th style="text-align:right;">

num\_coders

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

’0002700053513

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.50

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’0064100319507

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.50

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’0001700203910

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.33

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’0001000162110

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.50

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’0912100016603

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.50

</td>

<td style="text-align:right;">

2

</td>

</tr>

</tbody>

</table>

<table>

<thead>

<tr>

<th style="text-align:left;">

</th>

<th style="text-align:right;">

ICC

</th>

<th style="text-align:right;">

p-value

</th>

<th style="text-align:right;">

lower CI limit

</th>

<th style="text-align:right;">

upper CI limit

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

ICC(1)

</td>

<td style="text-align:right;">

0.4058433

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.3347807

</td>

<td style="text-align:right;">

0.4751572

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(k)

</td>

<td style="text-align:right;">

0.6448971

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.5722943

</td>

<td style="text-align:right;">

0.7064918

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(A,1)

</td>

<td style="text-align:right;">

0.4220531

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.3522027

</td>

<td style="text-align:right;">

0.4900348

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(A,k)

</td>

<td style="text-align:right;">

0.6600481

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.5909993

</td>

<td style="text-align:right;">

0.7187478

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(C,1)

</td>

<td style="text-align:right;">

0.4264757

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.3562632

</td>

<td style="text-align:right;">

0.4946206

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(C,k)

</td>

<td style="text-align:right;">

0.6640989

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.5953770

</td>

<td style="text-align:right;">

0.7223885

</td>

</tr>

</tbody>

</table>

\[1\] “CORRELATION MATRIX”

<table>

<thead>

<tr>

<th style="text-align:left;">

</th>

<th style="text-align:right;">

andrea

</th>

<th style="text-align:right;">

carmen

</th>

<th style="text-align:right;">

edithimg

</th>

<th style="text-align:right;">

jesicatapia

</th>

<th style="text-align:right;">

katia

</th>

<th style="text-align:right;">

sandra

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

andrea

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

0.44

</td>

<td style="text-align:right;">

0.56

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

0.37

</td>

<td style="text-align:right;">

0.56

</td>

</tr>

<tr>

<td style="text-align:left;">

carmen

</td>

<td style="text-align:right;">

0.44

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

0.63

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

0.44

</td>

<td style="text-align:right;">

NA

</td>

</tr>

<tr>

<td style="text-align:left;">

edithimg

</td>

<td style="text-align:right;">

0.56

</td>

<td style="text-align:right;">

0.63

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

0.45

</td>

<td style="text-align:right;">

0.54

</td>

<td style="text-align:right;">

0.61

</td>

</tr>

<tr>

<td style="text-align:left;">

jesicatapia

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

0.45

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

0.42

</td>

</tr>

<tr>

<td style="text-align:left;">

katia

</td>

<td style="text-align:right;">

0.37

</td>

<td style="text-align:right;">

0.44

</td>

<td style="text-align:right;">

0.54

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

0.27

</td>

</tr>

<tr>

<td style="text-align:left;">

sandra

</td>

<td style="text-align:right;">

0.56

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

0.61

</td>

<td style="text-align:right;">

0.42

</td>

<td style="text-align:right;">

0.27

</td>

<td style="text-align:right;">

1.00

</td>

</tr>

</tbody>

</table>

\[1\] “Kendalls W is: 0.645740947665241”

\[1\] “Binary variable S10\_1”

<table>

<thead>

<tr>

<th style="text-align:left;">

folio\_id

</th>

<th style="text-align:right;">

andrea

</th>

<th style="text-align:right;">

carmen

</th>

<th style="text-align:right;">

edithimg

</th>

<th style="text-align:right;">

jesicatapia

</th>

<th style="text-align:right;">

katia

</th>

<th style="text-align:right;">

sandra

</th>

<th style="text-align:right;">

rater\_var

</th>

<th style="text-align:right;">

num\_coders

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

’0817000010910

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0.50

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’0000600291210

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0.50

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’0673800045808

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0.33

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’0000900248810

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0.33

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’0637000025309

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0.33

</td>

<td style="text-align:right;">

3

</td>

</tr>

</tbody>

</table>

<table>

<thead>

<tr>

<th style="text-align:left;">

</th>

<th style="text-align:right;">

ICC

</th>

<th style="text-align:right;">

p-value

</th>

<th style="text-align:right;">

lower CI limit

</th>

<th style="text-align:right;">

upper CI limit

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

ICC(1)

</td>

<td style="text-align:right;">

0.2642022

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.1898058

</td>

<td style="text-align:right;">

0.3392155

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(k)

</td>

<td style="text-align:right;">

0.4884067

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.3838086

</td>

<td style="text-align:right;">

0.5771456

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(A,1)

</td>

<td style="text-align:right;">

0.2877537

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.2139387

</td>

<td style="text-align:right;">

0.3618408

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(A,k)

</td>

<td style="text-align:right;">

0.5178775

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.4198519

</td>

<td style="text-align:right;">

0.6011875

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(C,1)

</td>

<td style="text-align:right;">

0.2892708

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.2150700

</td>

<td style="text-align:right;">

0.3636395

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(C,k)

</td>

<td style="text-align:right;">

0.5197225

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.4214627

</td>

<td style="text-align:right;">

0.6030660

</td>

</tr>

</tbody>

</table>

\[1\] “CORRELATION MATRIX”

<table>

<thead>

<tr>

<th style="text-align:left;">

</th>

<th style="text-align:right;">

andrea

</th>

<th style="text-align:right;">

carmen

</th>

<th style="text-align:right;">

edithimg

</th>

<th style="text-align:right;">

jesicatapia

</th>

<th style="text-align:right;">

katia

</th>

<th style="text-align:right;">

sandra

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

andrea

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

0.30

</td>

<td style="text-align:right;">

\-0.08

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

0.39

</td>

<td style="text-align:right;">

0.62

</td>

</tr>

<tr>

<td style="text-align:left;">

carmen

</td>

<td style="text-align:right;">

0.30

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

\-0.10

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

0.33

</td>

<td style="text-align:right;">

0.34

</td>

</tr>

<tr>

<td style="text-align:left;">

edithimg

</td>

<td style="text-align:right;">

\-0.08

</td>

<td style="text-align:right;">

\-0.10

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

\-0.06

</td>

<td style="text-align:right;">

0.38

</td>

</tr>

<tr>

<td style="text-align:left;">

jesicatapia

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

\-0.05

</td>

</tr>

<tr>

<td style="text-align:left;">

katia

</td>

<td style="text-align:right;">

0.39

</td>

<td style="text-align:right;">

0.33

</td>

<td style="text-align:right;">

\-0.06

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

0.41

</td>

</tr>

<tr>

<td style="text-align:left;">

sandra

</td>

<td style="text-align:right;">

0.62

</td>

<td style="text-align:right;">

0.34

</td>

<td style="text-align:right;">

0.38

</td>

<td style="text-align:right;">

\-0.05

</td>

<td style="text-align:right;">

0.41

</td>

<td style="text-align:right;">

1.00

</td>

</tr>

</tbody>

</table>

\[1\] “Kendalls W is: 0.551681836308334”

\[1\] “Binary variable S10\_2”

<table>

<thead>

<tr>

<th style="text-align:left;">

folio\_id

</th>

<th style="text-align:right;">

andrea

</th>

<th style="text-align:right;">

carmen

</th>

<th style="text-align:right;">

edithimg

</th>

<th style="text-align:right;">

jesicatapia

</th>

<th style="text-align:right;">

katia

</th>

<th style="text-align:right;">

sandra

</th>

<th style="text-align:right;">

rater\_var

</th>

<th style="text-align:right;">

num\_coders

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

’0063500110314

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.50

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’0001200265707

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0.33

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’0610100092815

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.33

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’0002200038205

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0.33

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’0612100019614

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0.33

</td>

<td style="text-align:right;">

3

</td>

</tr>

</tbody>

</table>

<table>

<thead>

<tr>

<th style="text-align:left;">

</th>

<th style="text-align:right;">

ICC

</th>

<th style="text-align:right;">

p-value

</th>

<th style="text-align:right;">

lower CI limit

</th>

<th style="text-align:right;">

upper CI limit

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

ICC(1)

</td>

<td style="text-align:right;">

0.5735198

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.5128261

</td>

<td style="text-align:right;">

0.6304394

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(k)

</td>

<td style="text-align:right;">

0.7814414

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.7367558

</td>

<td style="text-align:right;">

0.8193517

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(A,1)

</td>

<td style="text-align:right;">

0.5928679

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.5332151

</td>

<td style="text-align:right;">

0.6484073

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(A,k)

</td>

<td style="text-align:right;">

0.7947327

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.7522405

</td>

<td style="text-align:right;">

0.8306361

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(C,1)

</td>

<td style="text-align:right;">

0.5893023

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.5299202

</td>

<td style="text-align:right;">

0.6447770

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(C,k)

</td>

<td style="text-align:right;">

0.7923158

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.7498257

</td>

<td style="text-align:right;">

0.8283557

</td>

</tr>

</tbody>

</table>

\[1\] “CORRELATION MATRIX”

<table>

<thead>

<tr>

<th style="text-align:left;">

</th>

<th style="text-align:right;">

andrea

</th>

<th style="text-align:right;">

carmen

</th>

<th style="text-align:right;">

edithimg

</th>

<th style="text-align:right;">

jesicatapia

</th>

<th style="text-align:right;">

katia

</th>

<th style="text-align:right;">

sandra

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

andrea

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

0.60

</td>

<td style="text-align:right;">

0.27

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

0.73

</td>

<td style="text-align:right;">

0.56

</td>

</tr>

<tr>

<td style="text-align:left;">

carmen

</td>

<td style="text-align:right;">

0.60

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

0.84

</td>

<td style="text-align:right;">

\-0.05

</td>

</tr>

<tr>

<td style="text-align:left;">

edithimg

</td>

<td style="text-align:right;">

0.27

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

NA

</td>

</tr>

<tr>

<td style="text-align:left;">

jesicatapia

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

0.66

</td>

</tr>

<tr>

<td style="text-align:left;">

katia

</td>

<td style="text-align:right;">

0.73

</td>

<td style="text-align:right;">

0.84

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

0.63

</td>

</tr>

<tr>

<td style="text-align:left;">

sandra

</td>

<td style="text-align:right;">

0.56

</td>

<td style="text-align:right;">

\-0.05

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

0.66

</td>

<td style="text-align:right;">

0.63

</td>

<td style="text-align:right;">

1.00

</td>

</tr>

</tbody>

</table>

\[1\] “Kendalls W is: 0.722969610949358”

\[1\] “Binary variable S10\_3”

<table>

<thead>

<tr>

<th style="text-align:left;">

folio\_id

</th>

<th style="text-align:right;">

andrea

</th>

<th style="text-align:right;">

carmen

</th>

<th style="text-align:right;">

edithimg

</th>

<th style="text-align:right;">

jesicatapia

</th>

<th style="text-align:right;">

katia

</th>

<th style="text-align:right;">

sandra

</th>

<th style="text-align:right;">

rater\_var

</th>

<th style="text-align:right;">

num\_coders

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

’0000700048104

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0.50

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’1112500008310

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.33

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’1613100056411

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0.33

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’0063700328414

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0.50

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’0912100048710

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0.50

</td>

<td style="text-align:right;">

2

</td>

</tr>

</tbody>

</table>

<table>

<thead>

<tr>

<th style="text-align:left;">

</th>

<th style="text-align:right;">

ICC

</th>

<th style="text-align:right;">

p-value

</th>

<th style="text-align:right;">

lower CI limit

</th>

<th style="text-align:right;">

upper CI limit

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

ICC(1)

</td>

<td style="text-align:right;">

0.4549869

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.3862256

</td>

<td style="text-align:right;">

0.5212886

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(k)

</td>

<td style="text-align:right;">

0.6894005

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.6258967

</td>

<td style="text-align:right;">

0.7432759

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(A,1)

</td>

<td style="text-align:right;">

0.4602235

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.3917834

</td>

<td style="text-align:right;">

0.5261437

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(A,k)

</td>

<td style="text-align:right;">

0.6939001

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.6313672

</td>

<td style="text-align:right;">

0.7469658

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(C,1)

</td>

<td style="text-align:right;">

0.4604222

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.3919105

</td>

<td style="text-align:right;">

0.5263895

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(C,k)

</td>

<td style="text-align:right;">

0.6940699

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.6314798

</td>

<td style="text-align:right;">

0.7471586

</td>

</tr>

</tbody>

</table>

\[1\] “CORRELATION MATRIX”

<table>

<thead>

<tr>

<th style="text-align:left;">

</th>

<th style="text-align:right;">

andrea

</th>

<th style="text-align:right;">

carmen

</th>

<th style="text-align:right;">

edithimg

</th>

<th style="text-align:right;">

jesicatapia

</th>

<th style="text-align:right;">

katia

</th>

<th style="text-align:right;">

sandra

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

andrea

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

0.49

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

0.31

</td>

<td style="text-align:right;">

\-0.08

</td>

</tr>

<tr>

<td style="text-align:left;">

carmen

</td>

<td style="text-align:right;">

0.49

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

0.50

</td>

<td style="text-align:right;">

1.00

</td>

</tr>

<tr>

<td style="text-align:left;">

edithimg

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

1.00

</td>

</tr>

<tr>

<td style="text-align:left;">

jesicatapia

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

0.24

</td>

</tr>

<tr>

<td style="text-align:left;">

katia

</td>

<td style="text-align:right;">

0.31

</td>

<td style="text-align:right;">

0.50

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

0.81

</td>

</tr>

<tr>

<td style="text-align:left;">

sandra

</td>

<td style="text-align:right;">

\-0.08

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0.24

</td>

<td style="text-align:right;">

0.81

</td>

<td style="text-align:right;">

1.00

</td>

</tr>

</tbody>

</table>

\[1\] “Kendalls W is: 0.666347565269899”

\[1\] “Binary variable S10\_4”

<table>

<thead>

<tr>

<th style="text-align:left;">

folio\_id

</th>

<th style="text-align:right;">

andrea

</th>

<th style="text-align:right;">

carmen

</th>

<th style="text-align:right;">

edithimg

</th>

<th style="text-align:right;">

jesicatapia

</th>

<th style="text-align:right;">

katia

</th>

<th style="text-align:right;">

sandra

</th>

<th style="text-align:right;">

rater\_var

</th>

<th style="text-align:right;">

num\_coders

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

’0001100502111

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0.50

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’2136

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0.50

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’1857600018305

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.33

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’0000700003014

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0.50

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’1111200025211

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.33

</td>

<td style="text-align:right;">

3

</td>

</tr>

</tbody>

</table>

<table>

<thead>

<tr>

<th style="text-align:left;">

</th>

<th style="text-align:right;">

ICC

</th>

<th style="text-align:right;">

p-value

</th>

<th style="text-align:right;">

lower CI limit

</th>

<th style="text-align:right;">

upper CI limit

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

ICC(1)

</td>

<td style="text-align:right;">

0.1556294

</td>

<td style="text-align:right;">

1.22e-05

</td>

<td style="text-align:right;">

0.0818795

</td>

<td style="text-align:right;">

0.2319062

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(k)

</td>

<td style="text-align:right;">

0.3288801

</td>

<td style="text-align:right;">

1.22e-05

</td>

<td style="text-align:right;">

0.1916660

</td>

<td style="text-align:right;">

0.4452899

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(A,1)

</td>

<td style="text-align:right;">

0.1540992

</td>

<td style="text-align:right;">

1.29e-05

</td>

<td style="text-align:right;">

0.0809108

</td>

<td style="text-align:right;">

0.2299128

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(A,k)

</td>

<td style="text-align:right;">

0.3263047

</td>

<td style="text-align:right;">

1.29e-05

</td>

<td style="text-align:right;">

0.1897342

</td>

<td style="text-align:right;">

0.4424807

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(C,1)

</td>

<td style="text-align:right;">

0.1552809

</td>

<td style="text-align:right;">

1.29e-05

</td>

<td style="text-align:right;">

0.0814937

</td>

<td style="text-align:right;">

0.2315941

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(C,k)

</td>

<td style="text-align:right;">

0.3282945

</td>

<td style="text-align:right;">

1.29e-05

</td>

<td style="text-align:right;">

0.1908705

</td>

<td style="text-align:right;">

0.4448570

</td>

</tr>

</tbody>

</table>

\[1\] “CORRELATION MATRIX”

<table>

<thead>

<tr>

<th style="text-align:left;">

</th>

<th style="text-align:right;">

andrea

</th>

<th style="text-align:right;">

carmen

</th>

<th style="text-align:right;">

edithimg

</th>

<th style="text-align:right;">

jesicatapia

</th>

<th style="text-align:right;">

katia

</th>

<th style="text-align:right;">

sandra

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

andrea

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

\-0.01

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

\-0.01

</td>

<td style="text-align:right;">

0.56

</td>

</tr>

<tr>

<td style="text-align:left;">

carmen

</td>

<td style="text-align:right;">

\-0.01

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

NA

</td>

</tr>

<tr>

<td style="text-align:left;">

edithimg

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

0.68

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

\-0.06

</td>

</tr>

<tr>

<td style="text-align:left;">

jesicatapia

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

0.68

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

\-0.05

</td>

</tr>

<tr>

<td style="text-align:left;">

katia

</td>

<td style="text-align:right;">

\-0.01

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

0.43

</td>

</tr>

<tr>

<td style="text-align:left;">

sandra

</td>

<td style="text-align:right;">

0.56

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

\-0.06

</td>

<td style="text-align:right;">

\-0.05

</td>

<td style="text-align:right;">

0.43

</td>

<td style="text-align:right;">

1.00

</td>

</tr>

</tbody>

</table>

\[1\] “Kendalls W is: 0.406780781235751”

\[1\] “Binary variable S11\_colonia”

<table>

<thead>

<tr>

<th style="text-align:left;">

folio\_id

</th>

<th style="text-align:right;">

andrea

</th>

<th style="text-align:right;">

carmen

</th>

<th style="text-align:right;">

edithimg

</th>

<th style="text-align:right;">

jesicatapia

</th>

<th style="text-align:right;">

katia

</th>

<th style="text-align:right;">

sandra

</th>

<th style="text-align:right;">

rater\_var

</th>

<th style="text-align:right;">

num\_coders

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

’1816400010805

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0.50

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’0064102522611

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0.33

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’0063700065013

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0.50

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’1857500071010

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0.33

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’0001200067405

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0.33

</td>

<td style="text-align:right;">

3

</td>

</tr>

</tbody>

</table>

<table>

<thead>

<tr>

<th style="text-align:left;">

</th>

<th style="text-align:right;">

ICC

</th>

<th style="text-align:right;">

p-value

</th>

<th style="text-align:right;">

lower CI limit

</th>

<th style="text-align:right;">

upper CI limit

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

ICC(1)

</td>

<td style="text-align:right;">

0.5821062

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.5221380

</td>

<td style="text-align:right;">

0.6382322

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(k)

</td>

<td style="text-align:right;">

0.7873934

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.7439249

</td>

<td style="text-align:right;">

0.8242713

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(A,1)

</td>

<td style="text-align:right;">

0.6081770

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.5507457

</td>

<td style="text-align:right;">

0.6616512

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(A,k)

</td>

<td style="text-align:right;">

0.8049485

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.7652047

</td>

<td style="text-align:right;">

0.8387026

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(C,1)

</td>

<td style="text-align:right;">

0.6113822

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.5539981

</td>

<td style="text-align:right;">

0.6647132

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(C,k)

</td>

<td style="text-align:right;">

0.8070547

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.7675801

</td>

<td style="text-align:right;">

0.8405369

</td>

</tr>

</tbody>

</table>

\[1\] “CORRELATION MATRIX”

<table>

<thead>

<tr>

<th style="text-align:left;">

</th>

<th style="text-align:right;">

andrea

</th>

<th style="text-align:right;">

carmen

</th>

<th style="text-align:right;">

edithimg

</th>

<th style="text-align:right;">

jesicatapia

</th>

<th style="text-align:right;">

katia

</th>

<th style="text-align:right;">

sandra

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

andrea

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

0.54

</td>

<td style="text-align:right;">

0.80

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

0.67

</td>

<td style="text-align:right;">

0.53

</td>

</tr>

<tr>

<td style="text-align:left;">

carmen

</td>

<td style="text-align:right;">

0.54

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

0.79

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

0.46

</td>

<td style="text-align:right;">

0.46

</td>

</tr>

<tr>

<td style="text-align:left;">

edithimg

</td>

<td style="text-align:right;">

0.80

</td>

<td style="text-align:right;">

0.79

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

0.92

</td>

</tr>

<tr>

<td style="text-align:left;">

jesicatapia

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

0.56

</td>

</tr>

<tr>

<td style="text-align:left;">

katia

</td>

<td style="text-align:right;">

0.67

</td>

<td style="text-align:right;">

0.46

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

0.88

</td>

</tr>

<tr>

<td style="text-align:left;">

sandra

</td>

<td style="text-align:right;">

0.53

</td>

<td style="text-align:right;">

0.46

</td>

<td style="text-align:right;">

0.92

</td>

<td style="text-align:right;">

0.56

</td>

<td style="text-align:right;">

0.88

</td>

<td style="text-align:right;">

1.00

</td>

</tr>

</tbody>

</table>

\[1\] “Kendalls W is: 0.757278069247416”

\[1\] “Binary variable S11\_cuando la información fue generada o cuando
ocurrió un evento (una referencia temporal más específica que un año)”

<table>

<thead>

<tr>

<th style="text-align:left;">

folio\_id

</th>

<th style="text-align:right;">

andrea

</th>

<th style="text-align:right;">

carmen

</th>

<th style="text-align:right;">

edithimg

</th>

<th style="text-align:right;">

jesicatapia

</th>

<th style="text-align:right;">

katia

</th>

<th style="text-align:right;">

sandra

</th>

<th style="text-align:right;">

rater\_var

</th>

<th style="text-align:right;">

num\_coders

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

’0064100060013

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0.33

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’0918600000510

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0.33

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’0063700261615

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0.33

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’1113100032713

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0.33

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’0064103030312

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0.33

</td>

<td style="text-align:right;">

3

</td>

</tr>

</tbody>

</table>

<table>

<thead>

<tr>

<th style="text-align:left;">

</th>

<th style="text-align:right;">

ICC

</th>

<th style="text-align:right;">

p-value

</th>

<th style="text-align:right;">

lower CI limit

</th>

<th style="text-align:right;">

upper CI limit

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

ICC(1)

</td>

<td style="text-align:right;">

0.5802537

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.5201273

</td>

<td style="text-align:right;">

0.6365522

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(k)

</td>

<td style="text-align:right;">

0.7861166

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.7423870

</td>

<td style="text-align:right;">

0.8232160

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(A,1)

</td>

<td style="text-align:right;">

0.5941595

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.5353042

</td>

<td style="text-align:right;">

0.6491033

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(A,k)

</td>

<td style="text-align:right;">

0.7956047

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.7538665

</td>

<td style="text-align:right;">

0.8310289

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(C,1)

</td>

<td style="text-align:right;">

0.5945837

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.5356678

</td>

<td style="text-align:right;">

0.6495547

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(C,k)

</td>

<td style="text-align:right;">

0.7958907

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.7541321

</td>

<td style="text-align:right;">

0.8313102

</td>

</tr>

</tbody>

</table>

\[1\] “CORRELATION MATRIX”

<table>

<thead>

<tr>

<th style="text-align:left;">

</th>

<th style="text-align:right;">

andrea

</th>

<th style="text-align:right;">

carmen

</th>

<th style="text-align:right;">

edithimg

</th>

<th style="text-align:right;">

jesicatapia

</th>

<th style="text-align:right;">

katia

</th>

<th style="text-align:right;">

sandra

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

andrea

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

0.55

</td>

<td style="text-align:right;">

0.76

</td>

<td style="text-align:right;">

0.50

</td>

<td style="text-align:right;">

0.65

</td>

<td style="text-align:right;">

0.36

</td>

</tr>

<tr>

<td style="text-align:left;">

carmen

</td>

<td style="text-align:right;">

0.55

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

0.60

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

0.56

</td>

<td style="text-align:right;">

0.69

</td>

</tr>

<tr>

<td style="text-align:left;">

edithimg

</td>

<td style="text-align:right;">

0.76

</td>

<td style="text-align:right;">

0.60

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

0.65

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

0.81

</td>

</tr>

<tr>

<td style="text-align:left;">

jesicatapia

</td>

<td style="text-align:right;">

0.50

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

0.65

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

0.59

</td>

</tr>

<tr>

<td style="text-align:left;">

katia

</td>

<td style="text-align:right;">

0.65

</td>

<td style="text-align:right;">

0.56

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

0.67

</td>

</tr>

<tr>

<td style="text-align:left;">

sandra

</td>

<td style="text-align:right;">

0.36

</td>

<td style="text-align:right;">

0.69

</td>

<td style="text-align:right;">

0.81

</td>

<td style="text-align:right;">

0.59

</td>

<td style="text-align:right;">

0.67

</td>

<td style="text-align:right;">

1.00

</td>

</tr>

</tbody>

</table>

\[1\] “Kendalls W is: 0.759431807300371”

\[1\] “Binary variable S11\_etc.)”

<table>

<thead>

<tr>

<th style="text-align:left;">

folio\_id

</th>

<th style="text-align:right;">

andrea

</th>

<th style="text-align:right;">

carmen

</th>

<th style="text-align:right;">

edithimg

</th>

<th style="text-align:right;">

jesicatapia

</th>

<th style="text-align:right;">

katia

</th>

<th style="text-align:right;">

sandra

</th>

<th style="text-align:right;">

rater\_var

</th>

<th style="text-align:right;">

num\_coders

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

’1857600021710

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0.33

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’0001000087410

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0.33

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’0002700122913

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0.33

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’1111500050314

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0.50

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’0002000100913

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0.33

</td>

<td style="text-align:right;">

3

</td>

</tr>

</tbody>

</table>

<table>

<thead>

<tr>

<th style="text-align:left;">

</th>

<th style="text-align:right;">

ICC

</th>

<th style="text-align:right;">

p-value

</th>

<th style="text-align:right;">

lower CI limit

</th>

<th style="text-align:right;">

upper CI limit

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

ICC(1)

</td>

<td style="text-align:right;">

0.5821062

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.5221380

</td>

<td style="text-align:right;">

0.6382322

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(k)

</td>

<td style="text-align:right;">

0.7873934

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.7439249

</td>

<td style="text-align:right;">

0.8242713

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(A,1)

</td>

<td style="text-align:right;">

0.6081770

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.5507457

</td>

<td style="text-align:right;">

0.6616512

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(A,k)

</td>

<td style="text-align:right;">

0.8049485

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.7652047

</td>

<td style="text-align:right;">

0.8387026

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(C,1)

</td>

<td style="text-align:right;">

0.6113822

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.5539981

</td>

<td style="text-align:right;">

0.6647132

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(C,k)

</td>

<td style="text-align:right;">

0.8070547

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.7675801

</td>

<td style="text-align:right;">

0.8405369

</td>

</tr>

</tbody>

</table>

\[1\] “CORRELATION MATRIX”

<table>

<thead>

<tr>

<th style="text-align:left;">

</th>

<th style="text-align:right;">

andrea

</th>

<th style="text-align:right;">

carmen

</th>

<th style="text-align:right;">

edithimg

</th>

<th style="text-align:right;">

jesicatapia

</th>

<th style="text-align:right;">

katia

</th>

<th style="text-align:right;">

sandra

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

andrea

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

0.54

</td>

<td style="text-align:right;">

0.80

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

0.67

</td>

<td style="text-align:right;">

0.53

</td>

</tr>

<tr>

<td style="text-align:left;">

carmen

</td>

<td style="text-align:right;">

0.54

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

0.79

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

0.46

</td>

<td style="text-align:right;">

0.46

</td>

</tr>

<tr>

<td style="text-align:left;">

edithimg

</td>

<td style="text-align:right;">

0.80

</td>

<td style="text-align:right;">

0.79

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

0.92

</td>

</tr>

<tr>

<td style="text-align:left;">

jesicatapia

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

0.56

</td>

</tr>

<tr>

<td style="text-align:left;">

katia

</td>

<td style="text-align:right;">

0.67

</td>

<td style="text-align:right;">

0.46

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

0.88

</td>

</tr>

<tr>

<td style="text-align:left;">

sandra

</td>

<td style="text-align:right;">

0.53

</td>

<td style="text-align:right;">

0.46

</td>

<td style="text-align:right;">

0.92

</td>

<td style="text-align:right;">

0.56

</td>

<td style="text-align:right;">

0.88

</td>

<td style="text-align:right;">

1.00

</td>

</tr>

</tbody>

</table>

\[1\] “Kendalls W is: 0.757278069247416”

\[1\] “Binary variable S11\_por ejemplo”

<table>

<thead>

<tr>

<th style="text-align:left;">

folio\_id

</th>

<th style="text-align:right;">

andrea

</th>

<th style="text-align:right;">

carmen

</th>

<th style="text-align:right;">

edithimg

</th>

<th style="text-align:right;">

jesicatapia

</th>

<th style="text-align:right;">

katia

</th>

<th style="text-align:right;">

sandra

</th>

<th style="text-align:right;">

rater\_var

</th>

<th style="text-align:right;">

num\_coders

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

’1850000028006

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0.50

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’0064100551415

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0.33

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’1222600012909

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0.33

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’0000400020312

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0.33

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’0000900185907

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0.33

</td>

<td style="text-align:right;">

3

</td>

</tr>

</tbody>

</table>

<table>

<thead>

<tr>

<th style="text-align:left;">

</th>

<th style="text-align:right;">

ICC

</th>

<th style="text-align:right;">

p-value

</th>

<th style="text-align:right;">

lower CI limit

</th>

<th style="text-align:right;">

upper CI limit

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

ICC(1)

</td>

<td style="text-align:right;">

0.5343251

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.4705638

</td>

<td style="text-align:right;">

0.5946733

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(k)

</td>

<td style="text-align:right;">

0.7531300

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.7026561

</td>

<td style="text-align:right;">

0.7959511

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(A,1)

</td>

<td style="text-align:right;">

0.5624578

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.5008676

</td>

<td style="text-align:right;">

0.6203704

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(A,k)

</td>

<td style="text-align:right;">

0.7736439

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.7273774

</td>

<td style="text-align:right;">

0.8128992

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(C,1)

</td>

<td style="text-align:right;">

0.5624311

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.5007906

</td>

<td style="text-align:right;">

0.6203796

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(C,k)

</td>

<td style="text-align:right;">

0.7736248

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.7273108

</td>

<td style="text-align:right;">

0.8129082

</td>

</tr>

</tbody>

</table>

\[1\] “CORRELATION MATRIX”

<table>

<thead>

<tr>

<th style="text-align:left;">

</th>

<th style="text-align:right;">

andrea

</th>

<th style="text-align:right;">

carmen

</th>

<th style="text-align:right;">

edithimg

</th>

<th style="text-align:right;">

jesicatapia

</th>

<th style="text-align:right;">

katia

</th>

<th style="text-align:right;">

sandra

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

andrea

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

0.62

</td>

<td style="text-align:right;">

0.63

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

0.62

</td>

<td style="text-align:right;">

0.52

</td>

</tr>

<tr>

<td style="text-align:left;">

carmen

</td>

<td style="text-align:right;">

0.62

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

0.75

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

0.49

</td>

<td style="text-align:right;">

0.44

</td>

</tr>

<tr>

<td style="text-align:left;">

edithimg

</td>

<td style="text-align:right;">

0.63

</td>

<td style="text-align:right;">

0.75

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

0.48

</td>

<td style="text-align:right;">

0.45

</td>

<td style="text-align:right;">

0.65

</td>

</tr>

<tr>

<td style="text-align:left;">

jesicatapia

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

0.48

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

0.46

</td>

</tr>

<tr>

<td style="text-align:left;">

katia

</td>

<td style="text-align:right;">

0.62

</td>

<td style="text-align:right;">

0.49

</td>

<td style="text-align:right;">

0.45

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

0.56

</td>

</tr>

<tr>

<td style="text-align:left;">

sandra

</td>

<td style="text-align:right;">

0.52

</td>

<td style="text-align:right;">

0.44

</td>

<td style="text-align:right;">

0.65

</td>

<td style="text-align:right;">

0.46

</td>

<td style="text-align:right;">

0.56

</td>

<td style="text-align:right;">

1.00

</td>

</tr>

</tbody>

</table>

\[1\] “Kendalls W is: 0.727894966473928”

\[1\] “Binary variable S11\_Un documento específico por nombre o número”

<table>

<thead>

<tr>

<th style="text-align:left;">

folio\_id

</th>

<th style="text-align:right;">

andrea

</th>

<th style="text-align:right;">

carmen

</th>

<th style="text-align:right;">

edithimg

</th>

<th style="text-align:right;">

jesicatapia

</th>

<th style="text-align:right;">

katia

</th>

<th style="text-align:right;">

sandra

</th>

<th style="text-align:right;">

rater\_var

</th>

<th style="text-align:right;">

num\_coders

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

’0000900247913

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0.33

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’2510100035511

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0.50

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’0681200014207

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.50

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’0063700025011

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0.33

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’0001000162110

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0.50

</td>

<td style="text-align:right;">

2

</td>

</tr>

</tbody>

</table>

<table>

<thead>

<tr>

<th style="text-align:left;">

</th>

<th style="text-align:right;">

ICC

</th>

<th style="text-align:right;">

p-value

</th>

<th style="text-align:right;">

lower CI limit

</th>

<th style="text-align:right;">

upper CI limit

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

ICC(1)

</td>

<td style="text-align:right;">

0.5700087

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.5090238

</td>

<td style="text-align:right;">

0.6272484

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(k)

</td>

<td style="text-align:right;">

0.7789824

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.7337941

</td>

<td style="text-align:right;">

0.8173192

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(A,1)

</td>

<td style="text-align:right;">

0.5892186

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.5300964

</td>

<td style="text-align:right;">

0.6445141

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(A,k)

</td>

<td style="text-align:right;">

0.7922589

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.7499406

</td>

<td style="text-align:right;">

0.8282025

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(C,1)

</td>

<td style="text-align:right;">

0.5922423

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.5331188

</td>

<td style="text-align:right;">

0.6474373

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(C,k)

</td>

<td style="text-align:right;">

0.7943097

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.7522276

</td>

<td style="text-align:right;">

0.8300036

</td>

</tr>

</tbody>

</table>

\[1\] “CORRELATION MATRIX”

<table>

<thead>

<tr>

<th style="text-align:left;">

</th>

<th style="text-align:right;">

andrea

</th>

<th style="text-align:right;">

carmen

</th>

<th style="text-align:right;">

edithimg

</th>

<th style="text-align:right;">

jesicatapia

</th>

<th style="text-align:right;">

katia

</th>

<th style="text-align:right;">

sandra

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

andrea

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

0.62

</td>

<td style="text-align:right;">

0.88

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

0.43

</td>

<td style="text-align:right;">

0.48

</td>

</tr>

<tr>

<td style="text-align:left;">

carmen

</td>

<td style="text-align:right;">

0.62

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

0.52

</td>

<td style="text-align:right;">

0.67

</td>

</tr>

<tr>

<td style="text-align:left;">

edithimg

</td>

<td style="text-align:right;">

0.88

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

0.83

</td>

<td style="text-align:right;">

0.69

</td>

<td style="text-align:right;">

0.73

</td>

</tr>

<tr>

<td style="text-align:left;">

jesicatapia

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

0.83

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

0.70

</td>

</tr>

<tr>

<td style="text-align:left;">

katia

</td>

<td style="text-align:right;">

0.43

</td>

<td style="text-align:right;">

0.52

</td>

<td style="text-align:right;">

0.69

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

0.63

</td>

</tr>

<tr>

<td style="text-align:left;">

sandra

</td>

<td style="text-align:right;">

0.48

</td>

<td style="text-align:right;">

0.67

</td>

<td style="text-align:right;">

0.73

</td>

<td style="text-align:right;">

0.70

</td>

<td style="text-align:right;">

0.63

</td>

<td style="text-align:right;">

1.00

</td>

</tr>

</tbody>

</table>

\[1\] “Kendalls W is: 0.753568911801142”

\[1\] “Binary variable S11\_Un lugar específico (una referencia más
específica que el nombre de un estado”

<table>

<thead>

<tr>

<th style="text-align:left;">

folio\_id

</th>

<th style="text-align:right;">

andrea

</th>

<th style="text-align:right;">

carmen

</th>

<th style="text-align:right;">

edithimg

</th>

<th style="text-align:right;">

jesicatapia

</th>

<th style="text-align:right;">

katia

</th>

<th style="text-align:right;">

sandra

</th>

<th style="text-align:right;">

rater\_var

</th>

<th style="text-align:right;">

num\_coders

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

’1511100022411

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.50

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’0002700122913

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0.33

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’0917900001507

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0.33

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’0064100332409

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0.33

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’1610100091713

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0.33

</td>

<td style="text-align:right;">

3

</td>

</tr>

</tbody>

</table>

<table>

<thead>

<tr>

<th style="text-align:left;">

</th>

<th style="text-align:right;">

ICC

</th>

<th style="text-align:right;">

p-value

</th>

<th style="text-align:right;">

lower CI limit

</th>

<th style="text-align:right;">

upper CI limit

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

ICC(1)

</td>

<td style="text-align:right;">

0.5821062

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.5221380

</td>

<td style="text-align:right;">

0.6382322

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(k)

</td>

<td style="text-align:right;">

0.7873934

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.7439249

</td>

<td style="text-align:right;">

0.8242713

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(A,1)

</td>

<td style="text-align:right;">

0.6081770

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.5507457

</td>

<td style="text-align:right;">

0.6616512

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(A,k)

</td>

<td style="text-align:right;">

0.8049485

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.7652047

</td>

<td style="text-align:right;">

0.8387026

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(C,1)

</td>

<td style="text-align:right;">

0.6113822

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.5539981

</td>

<td style="text-align:right;">

0.6647132

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(C,k)

</td>

<td style="text-align:right;">

0.8070547

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.7675801

</td>

<td style="text-align:right;">

0.8405369

</td>

</tr>

</tbody>

</table>

\[1\] “CORRELATION MATRIX”

<table>

<thead>

<tr>

<th style="text-align:left;">

</th>

<th style="text-align:right;">

andrea

</th>

<th style="text-align:right;">

carmen

</th>

<th style="text-align:right;">

edithimg

</th>

<th style="text-align:right;">

jesicatapia

</th>

<th style="text-align:right;">

katia

</th>

<th style="text-align:right;">

sandra

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

andrea

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

0.54

</td>

<td style="text-align:right;">

0.80

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

0.67

</td>

<td style="text-align:right;">

0.53

</td>

</tr>

<tr>

<td style="text-align:left;">

carmen

</td>

<td style="text-align:right;">

0.54

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

0.79

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

0.46

</td>

<td style="text-align:right;">

0.46

</td>

</tr>

<tr>

<td style="text-align:left;">

edithimg

</td>

<td style="text-align:right;">

0.80

</td>

<td style="text-align:right;">

0.79

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

0.92

</td>

</tr>

<tr>

<td style="text-align:left;">

jesicatapia

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

0.56

</td>

</tr>

<tr>

<td style="text-align:left;">

katia

</td>

<td style="text-align:right;">

0.67

</td>

<td style="text-align:right;">

0.46

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

0.88

</td>

</tr>

<tr>

<td style="text-align:left;">

sandra

</td>

<td style="text-align:right;">

0.53

</td>

<td style="text-align:right;">

0.46

</td>

<td style="text-align:right;">

0.92

</td>

<td style="text-align:right;">

0.56

</td>

<td style="text-align:right;">

0.88

</td>

<td style="text-align:right;">

1.00

</td>

</tr>

</tbody>

</table>

\[1\] “Kendalls W is: 0.757278069247416”

\[1\] “Binary variable S11\_un municipio”

<table>

<thead>

<tr>

<th style="text-align:left;">

folio\_id

</th>

<th style="text-align:right;">

andrea

</th>

<th style="text-align:right;">

carmen

</th>

<th style="text-align:right;">

edithimg

</th>

<th style="text-align:right;">

jesicatapia

</th>

<th style="text-align:right;">

katia

</th>

<th style="text-align:right;">

sandra

</th>

<th style="text-align:right;">

rater\_var

</th>

<th style="text-align:right;">

num\_coders

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

’2136

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0.50

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’0064102525911

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0.33

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’0810000004911

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0.50

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’1117100000109

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0.50

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’0064102522611

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0.33

</td>

<td style="text-align:right;">

3

</td>

</tr>

</tbody>

</table>

<table>

<thead>

<tr>

<th style="text-align:left;">

</th>

<th style="text-align:right;">

ICC

</th>

<th style="text-align:right;">

p-value

</th>

<th style="text-align:right;">

lower CI limit

</th>

<th style="text-align:right;">

upper CI limit

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

ICC(1)

</td>

<td style="text-align:right;">

0.5821062

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.5221380

</td>

<td style="text-align:right;">

0.6382322

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(k)

</td>

<td style="text-align:right;">

0.7873934

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.7439249

</td>

<td style="text-align:right;">

0.8242713

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(A,1)

</td>

<td style="text-align:right;">

0.6081770

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.5507457

</td>

<td style="text-align:right;">

0.6616512

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(A,k)

</td>

<td style="text-align:right;">

0.8049485

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.7652047

</td>

<td style="text-align:right;">

0.8387026

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(C,1)

</td>

<td style="text-align:right;">

0.6113822

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.5539981

</td>

<td style="text-align:right;">

0.6647132

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(C,k)

</td>

<td style="text-align:right;">

0.8070547

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.7675801

</td>

<td style="text-align:right;">

0.8405369

</td>

</tr>

</tbody>

</table>

\[1\] “CORRELATION MATRIX”

<table>

<thead>

<tr>

<th style="text-align:left;">

</th>

<th style="text-align:right;">

andrea

</th>

<th style="text-align:right;">

carmen

</th>

<th style="text-align:right;">

edithimg

</th>

<th style="text-align:right;">

jesicatapia

</th>

<th style="text-align:right;">

katia

</th>

<th style="text-align:right;">

sandra

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

andrea

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

0.54

</td>

<td style="text-align:right;">

0.80

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

0.67

</td>

<td style="text-align:right;">

0.53

</td>

</tr>

<tr>

<td style="text-align:left;">

carmen

</td>

<td style="text-align:right;">

0.54

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

0.79

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

0.46

</td>

<td style="text-align:right;">

0.46

</td>

</tr>

<tr>

<td style="text-align:left;">

edithimg

</td>

<td style="text-align:right;">

0.80

</td>

<td style="text-align:right;">

0.79

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

0.92

</td>

</tr>

<tr>

<td style="text-align:left;">

jesicatapia

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

0.56

</td>

</tr>

<tr>

<td style="text-align:left;">

katia

</td>

<td style="text-align:right;">

0.67

</td>

<td style="text-align:right;">

0.46

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

0.88

</td>

</tr>

<tr>

<td style="text-align:left;">

sandra

</td>

<td style="text-align:right;">

0.53

</td>

<td style="text-align:right;">

0.46

</td>

<td style="text-align:right;">

0.92

</td>

<td style="text-align:right;">

0.56

</td>

<td style="text-align:right;">

0.88

</td>

<td style="text-align:right;">

1.00

</td>

</tr>

</tbody>

</table>

\[1\] “Kendalls W is: 0.757278069247416”

\[1\] “Binary variable S11\_una empresa u ONG)”

<table>

<thead>

<tr>

<th style="text-align:left;">

folio\_id

</th>

<th style="text-align:right;">

andrea

</th>

<th style="text-align:right;">

carmen

</th>

<th style="text-align:right;">

edithimg

</th>

<th style="text-align:right;">

jesicatapia

</th>

<th style="text-align:right;">

katia

</th>

<th style="text-align:right;">

sandra

</th>

<th style="text-align:right;">

rater\_var

</th>

<th style="text-align:right;">

num\_coders

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

’0002000155613

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.50

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’0064100029408

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0.50

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’0413100024507

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0.33

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’0681200032313

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0.33

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’0001100183308

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0.33

</td>

<td style="text-align:right;">

3

</td>

</tr>

</tbody>

</table>

<table>

<thead>

<tr>

<th style="text-align:left;">

</th>

<th style="text-align:right;">

ICC

</th>

<th style="text-align:right;">

p-value

</th>

<th style="text-align:right;">

lower CI limit

</th>

<th style="text-align:right;">

upper CI limit

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

ICC(1)

</td>

<td style="text-align:right;">

0.6232898

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.5670723

</td>

<td style="text-align:right;">

0.6753997

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(k)

</td>

<td style="text-align:right;">

0.8147831

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.7769145

</td>

<td style="text-align:right;">

0.8469101

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(A,1)

</td>

<td style="text-align:right;">

0.6268877

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.5711632

</td>

<td style="text-align:right;">

0.6785318

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(A,k)

</td>

<td style="text-align:right;">

0.8170888

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.7797935

</td>

<td style="text-align:right;">

0.8487573

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(C,1)

</td>

<td style="text-align:right;">

0.6283941

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.5726380

</td>

<td style="text-align:right;">

0.6800058

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(C,k)

</td>

<td style="text-align:right;">

0.8180502

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.7808252

</td>

<td style="text-align:right;">

0.8496243

</td>

</tr>

</tbody>

</table>

\[1\] “CORRELATION MATRIX”

<table>

<thead>

<tr>

<th style="text-align:left;">

</th>

<th style="text-align:right;">

andrea

</th>

<th style="text-align:right;">

carmen

</th>

<th style="text-align:right;">

edithimg

</th>

<th style="text-align:right;">

jesicatapia

</th>

<th style="text-align:right;">

katia

</th>

<th style="text-align:right;">

sandra

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

andrea

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

0.61

</td>

<td style="text-align:right;">

0.56

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

0.61

</td>

<td style="text-align:right;">

1.00

</td>

</tr>

<tr>

<td style="text-align:left;">

carmen

</td>

<td style="text-align:right;">

0.61

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

0.70

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

0.62

</td>

<td style="text-align:right;">

NA

</td>

</tr>

<tr>

<td style="text-align:left;">

edithimg

</td>

<td style="text-align:right;">

0.56

</td>

<td style="text-align:right;">

0.70

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

0.46

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

0.57

</td>

</tr>

<tr>

<td style="text-align:left;">

jesicatapia

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

0.46

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

0.76

</td>

</tr>

<tr>

<td style="text-align:left;">

katia

</td>

<td style="text-align:right;">

0.61

</td>

<td style="text-align:right;">

0.62

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

1.00

</td>

</tr>

<tr>

<td style="text-align:left;">

sandra

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

0.57

</td>

<td style="text-align:right;">

0.76

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

1.00

</td>

</tr>

</tbody>

</table>

\[1\] “Kendalls W is: 0.784090376235521”

\[1\] “Binary variable S11\_Una fecha específica”

<table>

<thead>

<tr>

<th style="text-align:left;">

folio\_id

</th>

<th style="text-align:right;">

andrea

</th>

<th style="text-align:right;">

carmen

</th>

<th style="text-align:right;">

edithimg

</th>

<th style="text-align:right;">

jesicatapia

</th>

<th style="text-align:right;">

katia

</th>

<th style="text-align:right;">

sandra

</th>

<th style="text-align:right;">

rater\_var

</th>

<th style="text-align:right;">

num\_coders

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

’0000700100909

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0.33

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’1215100127013

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0.33

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’0821000005107

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.33

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’0000700013215

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.50

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’0064101676309

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0.33

</td>

<td style="text-align:right;">

3

</td>

</tr>

</tbody>

</table>

<table>

<thead>

<tr>

<th style="text-align:left;">

</th>

<th style="text-align:right;">

ICC

</th>

<th style="text-align:right;">

p-value

</th>

<th style="text-align:right;">

lower CI limit

</th>

<th style="text-align:right;">

upper CI limit

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

ICC(1)

</td>

<td style="text-align:right;">

0.5802537

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.5201273

</td>

<td style="text-align:right;">

0.6365522

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(k)

</td>

<td style="text-align:right;">

0.7861166

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.7423870

</td>

<td style="text-align:right;">

0.8232160

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(A,1)

</td>

<td style="text-align:right;">

0.5941595

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.5353042

</td>

<td style="text-align:right;">

0.6491033

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(A,k)

</td>

<td style="text-align:right;">

0.7956047

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.7538665

</td>

<td style="text-align:right;">

0.8310289

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(C,1)

</td>

<td style="text-align:right;">

0.5945837

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.5356678

</td>

<td style="text-align:right;">

0.6495547

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(C,k)

</td>

<td style="text-align:right;">

0.7958907

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.7541321

</td>

<td style="text-align:right;">

0.8313102

</td>

</tr>

</tbody>

</table>

\[1\] “CORRELATION MATRIX”

<table>

<thead>

<tr>

<th style="text-align:left;">

</th>

<th style="text-align:right;">

andrea

</th>

<th style="text-align:right;">

carmen

</th>

<th style="text-align:right;">

edithimg

</th>

<th style="text-align:right;">

jesicatapia

</th>

<th style="text-align:right;">

katia

</th>

<th style="text-align:right;">

sandra

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

andrea

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

0.55

</td>

<td style="text-align:right;">

0.76

</td>

<td style="text-align:right;">

0.50

</td>

<td style="text-align:right;">

0.65

</td>

<td style="text-align:right;">

0.36

</td>

</tr>

<tr>

<td style="text-align:left;">

carmen

</td>

<td style="text-align:right;">

0.55

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

0.60

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

0.56

</td>

<td style="text-align:right;">

0.69

</td>

</tr>

<tr>

<td style="text-align:left;">

edithimg

</td>

<td style="text-align:right;">

0.76

</td>

<td style="text-align:right;">

0.60

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

0.65

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

0.81

</td>

</tr>

<tr>

<td style="text-align:left;">

jesicatapia

</td>

<td style="text-align:right;">

0.50

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

0.65

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

0.59

</td>

</tr>

<tr>

<td style="text-align:left;">

katia

</td>

<td style="text-align:right;">

0.65

</td>

<td style="text-align:right;">

0.56

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

0.67

</td>

</tr>

<tr>

<td style="text-align:left;">

sandra

</td>

<td style="text-align:right;">

0.36

</td>

<td style="text-align:right;">

0.69

</td>

<td style="text-align:right;">

0.81

</td>

<td style="text-align:right;">

0.59

</td>

<td style="text-align:right;">

0.67

</td>

<td style="text-align:right;">

1.00

</td>

</tr>

</tbody>

</table>

\[1\] “Kendalls W is: 0.759431807300371”

\[1\] “Binary variable S11\_Una institución pública específica (una
referencia más específica que el nombre de la dependencia”

<table>

<thead>

<tr>

<th style="text-align:left;">

folio\_id

</th>

<th style="text-align:right;">

andrea

</th>

<th style="text-align:right;">

carmen

</th>

<th style="text-align:right;">

edithimg

</th>

<th style="text-align:right;">

jesicatapia

</th>

<th style="text-align:right;">

katia

</th>

<th style="text-align:right;">

sandra

</th>

<th style="text-align:right;">

rater\_var

</th>

<th style="text-align:right;">

num\_coders

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

’0912100065110

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.33

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’0064101519911

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0.33

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’0063700025011

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0.33

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’1610100027310

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0.33

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’0681200037513

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0.33

</td>

<td style="text-align:right;">

3

</td>

</tr>

</tbody>

</table>

<table>

<thead>

<tr>

<th style="text-align:left;">

</th>

<th style="text-align:right;">

ICC

</th>

<th style="text-align:right;">

p-value

</th>

<th style="text-align:right;">

lower CI limit

</th>

<th style="text-align:right;">

upper CI limit

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

ICC(1)

</td>

<td style="text-align:right;">

0.3772620

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.3051359

</td>

<td style="text-align:right;">

0.4480859

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(k)

</td>

<td style="text-align:right;">

0.6169620

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.5386478

</td>

<td style="text-align:right;">

0.6834023

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(A,1)

</td>

<td style="text-align:right;">

0.3874211

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.3156745

</td>

<td style="text-align:right;">

0.4577102

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(A,k)

</td>

<td style="text-align:right;">

0.6270762

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.5508736

</td>

<td style="text-align:right;">

0.6917371

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(C,1)

</td>

<td style="text-align:right;">

0.3875222

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.3157111

</td>

<td style="text-align:right;">

0.4578577

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(C,k)

</td>

<td style="text-align:right;">

0.6271757

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.5508997

</td>

<td style="text-align:right;">

0.6918727

</td>

</tr>

</tbody>

</table>

\[1\] “CORRELATION MATRIX”

<table>

<thead>

<tr>

<th style="text-align:left;">

</th>

<th style="text-align:right;">

andrea

</th>

<th style="text-align:right;">

carmen

</th>

<th style="text-align:right;">

edithimg

</th>

<th style="text-align:right;">

jesicatapia

</th>

<th style="text-align:right;">

katia

</th>

<th style="text-align:right;">

sandra

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

andrea

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

0.56

</td>

<td style="text-align:right;">

0.51

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

0.43

</td>

<td style="text-align:right;">

0.29

</td>

</tr>

<tr>

<td style="text-align:left;">

carmen

</td>

<td style="text-align:right;">

0.56

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

0.49

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

0.39

</td>

<td style="text-align:right;">

NA

</td>

</tr>

<tr>

<td style="text-align:left;">

edithimg

</td>

<td style="text-align:right;">

0.51

</td>

<td style="text-align:right;">

0.49

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

0.1

</td>

<td style="text-align:right;">

0.54

</td>

<td style="text-align:right;">

0.21

</td>

</tr>

<tr>

<td style="text-align:left;">

jesicatapia

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

0.10

</td>

<td style="text-align:right;">

1.0

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

0.20

</td>

</tr>

<tr>

<td style="text-align:left;">

katia

</td>

<td style="text-align:right;">

0.43

</td>

<td style="text-align:right;">

0.39

</td>

<td style="text-align:right;">

0.54

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

0.31

</td>

</tr>

<tr>

<td style="text-align:left;">

sandra

</td>

<td style="text-align:right;">

0.29

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

0.21

</td>

<td style="text-align:right;">

0.2

</td>

<td style="text-align:right;">

0.31

</td>

<td style="text-align:right;">

1.00

</td>

</tr>

</tbody>

</table>

\[1\] “Kendalls W is: 0.616677775356448”

\[1\] “Binary variable S11\_Una organización no gubernamental específica
(por ejemplo”

<table>

<thead>

<tr>

<th style="text-align:left;">

folio\_id

</th>

<th style="text-align:right;">

andrea

</th>

<th style="text-align:right;">

carmen

</th>

<th style="text-align:right;">

edithimg

</th>

<th style="text-align:right;">

jesicatapia

</th>

<th style="text-align:right;">

katia

</th>

<th style="text-align:right;">

sandra

</th>

<th style="text-align:right;">

rater\_var

</th>

<th style="text-align:right;">

num\_coders

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

’0637000025309

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.33

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’0001100183308

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0.33

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’0000900248713

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0.33

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’1215100127013

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0.33

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’0000900156715

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.33

</td>

<td style="text-align:right;">

3

</td>

</tr>

</tbody>

</table>

<table>

<thead>

<tr>

<th style="text-align:left;">

</th>

<th style="text-align:right;">

ICC

</th>

<th style="text-align:right;">

p-value

</th>

<th style="text-align:right;">

lower CI limit

</th>

<th style="text-align:right;">

upper CI limit

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

ICC(1)

</td>

<td style="text-align:right;">

0.6232898

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.5670723

</td>

<td style="text-align:right;">

0.6753997

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(k)

</td>

<td style="text-align:right;">

0.8147831

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.7769145

</td>

<td style="text-align:right;">

0.8469101

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(A,1)

</td>

<td style="text-align:right;">

0.6268877

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.5711632

</td>

<td style="text-align:right;">

0.6785318

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(A,k)

</td>

<td style="text-align:right;">

0.8170888

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.7797935

</td>

<td style="text-align:right;">

0.8487573

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(C,1)

</td>

<td style="text-align:right;">

0.6283941

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.5726380

</td>

<td style="text-align:right;">

0.6800058

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(C,k)

</td>

<td style="text-align:right;">

0.8180502

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.7808252

</td>

<td style="text-align:right;">

0.8496243

</td>

</tr>

</tbody>

</table>

\[1\] “CORRELATION MATRIX”

<table>

<thead>

<tr>

<th style="text-align:left;">

</th>

<th style="text-align:right;">

andrea

</th>

<th style="text-align:right;">

carmen

</th>

<th style="text-align:right;">

edithimg

</th>

<th style="text-align:right;">

jesicatapia

</th>

<th style="text-align:right;">

katia

</th>

<th style="text-align:right;">

sandra

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

andrea

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

0.61

</td>

<td style="text-align:right;">

0.56

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

0.61

</td>

<td style="text-align:right;">

1.00

</td>

</tr>

<tr>

<td style="text-align:left;">

carmen

</td>

<td style="text-align:right;">

0.61

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

0.70

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

0.62

</td>

<td style="text-align:right;">

NA

</td>

</tr>

<tr>

<td style="text-align:left;">

edithimg

</td>

<td style="text-align:right;">

0.56

</td>

<td style="text-align:right;">

0.70

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

0.46

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

0.57

</td>

</tr>

<tr>

<td style="text-align:left;">

jesicatapia

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

0.46

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

0.76

</td>

</tr>

<tr>

<td style="text-align:left;">

katia

</td>

<td style="text-align:right;">

0.61

</td>

<td style="text-align:right;">

0.62

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

1.00

</td>

</tr>

<tr>

<td style="text-align:left;">

sandra

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

0.57

</td>

<td style="text-align:right;">

0.76

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

1.00

</td>

</tr>

</tbody>

</table>

\[1\] “Kendalls W is: 0.784090376235521”

\[1\] “Binary variable S11\_Una persona específica por nombre o por
título”

<table>

<thead>

<tr>

<th style="text-align:left;">

folio\_id

</th>

<th style="text-align:right;">

andrea

</th>

<th style="text-align:right;">

carmen

</th>

<th style="text-align:right;">

edithimg

</th>

<th style="text-align:right;">

jesicatapia

</th>

<th style="text-align:right;">

katia

</th>

<th style="text-align:right;">

sandra

</th>

<th style="text-align:right;">

rater\_var

</th>

<th style="text-align:right;">

num\_coders

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

’0821000005107

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.33

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’0001000062110

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0.50

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’1511100045309

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.50

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’0063700057807

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0.33

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’0002200158108

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0.33

</td>

<td style="text-align:right;">

3

</td>

</tr>

</tbody>

</table>

<table>

<thead>

<tr>

<th style="text-align:left;">

</th>

<th style="text-align:right;">

ICC

</th>

<th style="text-align:right;">

p-value

</th>

<th style="text-align:right;">

lower CI limit

</th>

<th style="text-align:right;">

upper CI limit

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

ICC(1)

</td>

<td style="text-align:right;">

0.8202351

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.7883347

</td>

<td style="text-align:right;">

0.8484686

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(k)

</td>

<td style="text-align:right;">

0.9238469

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.9082769

</td>

<td style="text-align:right;">

0.9370561

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(A,1)

</td>

<td style="text-align:right;">

0.8220070

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.7904126

</td>

<td style="text-align:right;">

0.8499680

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(A,k)

</td>

<td style="text-align:right;">

0.9246913

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.9093135

</td>

<td style="text-align:right;">

0.9377429

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(C,1)

</td>

<td style="text-align:right;">

0.8223945

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.7908007

</td>

<td style="text-align:right;">

0.8503370

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(C,k)

</td>

<td style="text-align:right;">

0.9248756

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.9095060

</td>

<td style="text-align:right;">

0.9379121

</td>

</tr>

</tbody>

</table>

\[1\] “CORRELATION MATRIX”

<table>

<thead>

<tr>

<th style="text-align:left;">

</th>

<th style="text-align:right;">

andrea

</th>

<th style="text-align:right;">

carmen

</th>

<th style="text-align:right;">

edithimg

</th>

<th style="text-align:right;">

jesicatapia

</th>

<th style="text-align:right;">

katia

</th>

<th style="text-align:right;">

sandra

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

andrea

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

0.83

</td>

<td style="text-align:right;">

0.85

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

0.91

</td>

<td style="text-align:right;">

1.00

</td>

</tr>

<tr>

<td style="text-align:left;">

carmen

</td>

<td style="text-align:right;">

0.83

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

0.88

</td>

<td style="text-align:right;">

1.00

</td>

</tr>

<tr>

<td style="text-align:left;">

edithimg

</td>

<td style="text-align:right;">

0.85

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

0.71

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

0.86

</td>

</tr>

<tr>

<td style="text-align:left;">

jesicatapia

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

0.71

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

0.51

</td>

</tr>

<tr>

<td style="text-align:left;">

katia

</td>

<td style="text-align:right;">

0.91

</td>

<td style="text-align:right;">

0.88

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

0.88

</td>

</tr>

<tr>

<td style="text-align:left;">

sandra

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

0.86

</td>

<td style="text-align:right;">

0.51

</td>

<td style="text-align:right;">

0.88

</td>

<td style="text-align:right;">

1.00

</td>

</tr>

</tbody>

</table>

\[1\] “Kendalls W is: 0.898169284397459”

\[1\] “Binary variable S11\_una subsecretaría o área interna)”

<table>

<thead>

<tr>

<th style="text-align:left;">

folio\_id

</th>

<th style="text-align:right;">

andrea

</th>

<th style="text-align:right;">

carmen

</th>

<th style="text-align:right;">

edithimg

</th>

<th style="text-align:right;">

jesicatapia

</th>

<th style="text-align:right;">

katia

</th>

<th style="text-align:right;">

sandra

</th>

<th style="text-align:right;">

rater\_var

</th>

<th style="text-align:right;">

num\_coders

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

’0000400209014

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0.33

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’1610100144507

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.33

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’0000400124810

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0.33

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’2510100035511

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0.50

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’0002700067612

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.50

</td>

<td style="text-align:right;">

2

</td>

</tr>

</tbody>

</table>

<table>

<thead>

<tr>

<th style="text-align:left;">

</th>

<th style="text-align:right;">

ICC

</th>

<th style="text-align:right;">

p-value

</th>

<th style="text-align:right;">

lower CI limit

</th>

<th style="text-align:right;">

upper CI limit

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

ICC(1)

</td>

<td style="text-align:right;">

0.3772620

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.3051359

</td>

<td style="text-align:right;">

0.4480859

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(k)

</td>

<td style="text-align:right;">

0.6169620

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.5386478

</td>

<td style="text-align:right;">

0.6834023

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(A,1)

</td>

<td style="text-align:right;">

0.3874211

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.3156745

</td>

<td style="text-align:right;">

0.4577102

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(A,k)

</td>

<td style="text-align:right;">

0.6270762

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.5508736

</td>

<td style="text-align:right;">

0.6917371

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(C,1)

</td>

<td style="text-align:right;">

0.3875222

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.3157111

</td>

<td style="text-align:right;">

0.4578577

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(C,k)

</td>

<td style="text-align:right;">

0.6271757

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.5508997

</td>

<td style="text-align:right;">

0.6918727

</td>

</tr>

</tbody>

</table>

\[1\] “CORRELATION MATRIX”

<table>

<thead>

<tr>

<th style="text-align:left;">

</th>

<th style="text-align:right;">

andrea

</th>

<th style="text-align:right;">

carmen

</th>

<th style="text-align:right;">

edithimg

</th>

<th style="text-align:right;">

jesicatapia

</th>

<th style="text-align:right;">

katia

</th>

<th style="text-align:right;">

sandra

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

andrea

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

0.56

</td>

<td style="text-align:right;">

0.51

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

0.43

</td>

<td style="text-align:right;">

0.29

</td>

</tr>

<tr>

<td style="text-align:left;">

carmen

</td>

<td style="text-align:right;">

0.56

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

0.49

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

0.39

</td>

<td style="text-align:right;">

NA

</td>

</tr>

<tr>

<td style="text-align:left;">

edithimg

</td>

<td style="text-align:right;">

0.51

</td>

<td style="text-align:right;">

0.49

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

0.1

</td>

<td style="text-align:right;">

0.54

</td>

<td style="text-align:right;">

0.21

</td>

</tr>

<tr>

<td style="text-align:left;">

jesicatapia

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

0.10

</td>

<td style="text-align:right;">

1.0

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

0.20

</td>

</tr>

<tr>

<td style="text-align:left;">

katia

</td>

<td style="text-align:right;">

0.43

</td>

<td style="text-align:right;">

0.39

</td>

<td style="text-align:right;">

0.54

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

0.31

</td>

</tr>

<tr>

<td style="text-align:left;">

sandra

</td>

<td style="text-align:right;">

0.29

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

0.21

</td>

<td style="text-align:right;">

0.2

</td>

<td style="text-align:right;">

0.31

</td>

<td style="text-align:right;">

1.00

</td>

</tr>

</tbody>

</table>

\[1\] “Kendalls W is: 0.616677775356448”

``` r
for(i in (c("R5", "R7"))) {

data_wide <-
  reshape2::dcast(to_eval,  folio_id ~ coder_id, value.var= i ) %>%
  mutate_at(vars(contains("Person")),  function(x) factor(x, levels = c("Poca o nada", 
                                                  "Menos de la mitad", 
                                                  "Aproximadamente la mitad",
                                                  "La mayoría",
                                                  "Toda"), ordered = TRUE))

  data_wide$rater_var <- apply(data_wide[, coder_ids], 1, function(x)
                               length(na.omit(unique(x))))
  
  data_wide$num_coders <- apply(data_wide[, coder_ids], 1, function(x)
      length(na.omit(x)))
  
  ratings_out <- data_wide %>%
    filter(rater_var > 1)

#capture the number of coders, get observations with at least 2. 
#output where there is disagreement only
  output <-  ratings_out %>% kable(., escape = TRUE, digit = 2)
  print(output)
  bad_folios <- c(bad_folios, ratings_out$folio_id)

  to_stat <- select(data_wide, -folio_id, -rater_var, -num_coders)
  #print(kable(iccNA(to_stat)$ICCs))


  #kendallW <- kendallNA(to_stat)
  #print(sprintf("Kendalls W is: %s", kendallW$`Kendall's W`))
  print(kripp.alpha(t(to_stat), method = "ordinal"))
  cat("\n\n")
}
```

<table>

<thead>

<tr>

<th style="text-align:left;">

folio\_id

</th>

<th style="text-align:left;">

andrea

</th>

<th style="text-align:left;">

carmen

</th>

<th style="text-align:left;">

edithimg

</th>

<th style="text-align:left;">

jesicatapia

</th>

<th style="text-align:left;">

katia

</th>

<th style="text-align:left;">

sandra

</th>

<th style="text-align:right;">

rater\_var

</th>

<th style="text-align:right;">

num\_coders

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

’0000800101312

</td>

<td style="text-align:left;">

Aproximadamente la mitad

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

La mayoría

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’0064101735111

</td>

<td style="text-align:left;">

Menos de la mitad

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

Menos de la mitad

</td>

<td style="text-align:left;">

La mayoría

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’0210000030806

</td>

<td style="text-align:left;">

Aproximadamente la mitad

</td>

<td style="text-align:left;">

La mayoría

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’1615100031912

</td>

<td style="text-align:left;">

La mayoría

</td>

<td style="text-align:left;">

Aproximadamente la mitad

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

Menos de la mitad

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:right;">

3

</td>

<td style="text-align:right;">

3

</td>

</tr>

</tbody>

</table>

Krippendorff’s alpha

Subjects = 349 Raters = 6 alpha = 0.673

<table>

<thead>

<tr>

<th style="text-align:left;">

folio\_id

</th>

<th style="text-align:left;">

andrea

</th>

<th style="text-align:left;">

carmen

</th>

<th style="text-align:left;">

edithimg

</th>

<th style="text-align:left;">

jesicatapia

</th>

<th style="text-align:left;">

katia

</th>

<th style="text-align:left;">

sandra

</th>

<th style="text-align:right;">

rater\_var

</th>

<th style="text-align:right;">

num\_coders

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

’0000400111912

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

Poca o nada

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

Toda

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’0000400124810

</td>

<td style="text-align:left;">

Toda

</td>

<td style="text-align:left;">

La mayoría

</td>

<td style="text-align:left;">

Toda

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’0000400190015

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

Poca o nada

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

Aproximadamente la mitad

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’0000400209014

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

La mayoría

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

Aproximadamente la mitad

</td>

<td style="text-align:left;">

Menos de la mitad

</td>

<td style="text-align:right;">

3

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’0000500058507

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

La mayoría

</td>

<td style="text-align:left;">

Aproximadamente la mitad

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

Toda

</td>

<td style="text-align:right;">

3

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’0000500102110

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

Poca o nada

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

Aproximadamente la mitad

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’0000600019613

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

La mayoría

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

Toda

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’0000600021806

</td>

<td style="text-align:left;">

La mayoría

</td>

<td style="text-align:left;">

Aproximadamente la mitad

</td>

<td style="text-align:left;">

Toda

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:right;">

3

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’0000600024610

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

Aproximadamente la mitad

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

Toda

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’0000600035210

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

La mayoría

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

Aproximadamente la mitad

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’0000600170709

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

Poca o nada

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

La mayoría

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’0000600247213

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

Poca o nada

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

Toda

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’0000700001309

</td>

<td style="text-align:left;">

La mayoría

</td>

<td style="text-align:left;">

Toda

</td>

<td style="text-align:left;">

La mayoría

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’0000700003014

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

La mayoría

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

Toda

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’0000700012515

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

Poca o nada

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

Toda

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’0000700048104

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

Aproximadamente la mitad

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

Toda

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’0000700049211

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

Menos de la mitad

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

Poca o nada

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’0000700077705

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

Menos de la mitad

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

La mayoría

</td>

<td style="text-align:left;">

La mayoría

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’0000700207914

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

Poca o nada

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

Toda

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’0000800073014

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

Aproximadamente la mitad

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

Toda

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’0000800101312

</td>

<td style="text-align:left;">

Aproximadamente la mitad

</td>

<td style="text-align:left;">

La mayoría

</td>

<td style="text-align:left;">

Aproximadamente la mitad

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’0000900005113

</td>

<td style="text-align:left;">

La mayoría

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

Toda

</td>

<td style="text-align:left;">

Toda

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’0000900130914

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

La mayoría

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

Toda

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’0000900156715

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

Toda

</td>

<td style="text-align:left;">

Menos de la mitad

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

Toda

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’0000900185907

</td>

<td style="text-align:left;">

La mayoría

</td>

<td style="text-align:left;">

La mayoría

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

Toda

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’0000900247913

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

Poca o nada

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

La mayoría

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’0000900248713

</td>

<td style="text-align:left;">

La mayoría

</td>

<td style="text-align:left;">

La mayoría

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

Toda

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’0000900248810

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

Menos de la mitad

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

Aproximadamente la mitad

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’0001000019203

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

Aproximadamente la mitad

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

Toda

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’0001000057812

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

La mayoría

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

Toda

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’0001000087410

</td>

<td style="text-align:left;">

Toda

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

Toda

</td>

<td style="text-align:left;">

Poca o nada

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’0001000146208

</td>

<td style="text-align:left;">

Toda

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

Toda

</td>

<td style="text-align:left;">

Poca o nada

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’0001000162110

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

La mayoría

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

Aproximadamente la mitad

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’0001100081703

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

La mayoría

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

Poca o nada

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’0001100289115

</td>

<td style="text-align:left;">

La mayoría

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

Toda

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’0001100366708

</td>

<td style="text-align:left;">

Aproximadamente la mitad

</td>

<td style="text-align:left;">

Aproximadamente la mitad

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

Toda

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’0001100395711

</td>

<td style="text-align:left;">

Toda

</td>

<td style="text-align:left;">

La mayoría

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’0001100570007

</td>

<td style="text-align:left;">

Toda

</td>

<td style="text-align:left;">

Toda

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

Poca o nada

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’0001200067405

</td>

<td style="text-align:left;">

Menos de la mitad

</td>

<td style="text-align:left;">

Poca o nada

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

Aproximadamente la mitad

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:right;">

3

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’0001200150109

</td>

<td style="text-align:left;">

La mayoría

</td>

<td style="text-align:left;">

La mayoría

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

Toda

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’0001200202407

</td>

<td style="text-align:left;">

Toda

</td>

<td style="text-align:left;">

La mayoría

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

Toda

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’0001200265707

</td>

<td style="text-align:left;">

Toda

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

Poca o nada

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’0001200313314

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

Aproximadamente la mitad

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

Toda

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’0001300010014

</td>

<td style="text-align:left;">

Toda

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

Toda

</td>

<td style="text-align:left;">

Aproximadamente la mitad

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’0001300041011

</td>

<td style="text-align:left;">

La mayoría

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

Toda

</td>

<td style="text-align:left;">

Toda

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’0001400025309

</td>

<td style="text-align:left;">

Aproximadamente la mitad

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

Toda

</td>

<td style="text-align:left;">

La mayoría

</td>

<td style="text-align:right;">

3

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’0001400043706

</td>

<td style="text-align:left;">

La mayoría

</td>

<td style="text-align:left;">

Toda

</td>

<td style="text-align:left;">

Toda

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’0001600284910

</td>

<td style="text-align:left;">

Menos de la mitad

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

Toda

</td>

<td style="text-align:left;">

Aproximadamente la mitad

</td>

<td style="text-align:right;">

3

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’0001700020906

</td>

<td style="text-align:left;">

Aproximadamente la mitad

</td>

<td style="text-align:left;">

Aproximadamente la mitad

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

La mayoría

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’0001700027708

</td>

<td style="text-align:left;">

Toda

</td>

<td style="text-align:left;">

Menos de la mitad

</td>

<td style="text-align:left;">

Toda

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’0001700078810

</td>

<td style="text-align:left;">

Aproximadamente la mitad

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

La mayoría

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’0002000005615

</td>

<td style="text-align:left;">

Toda

</td>

<td style="text-align:left;">

Toda

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

La mayoría

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’0002000120512

</td>

<td style="text-align:left;">

La mayoría

</td>

<td style="text-align:left;">

La mayoría

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

Toda

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’0002200056808

</td>

<td style="text-align:left;">

Toda

</td>

<td style="text-align:left;">

Toda

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

Aproximadamente la mitad

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’0002700053513

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

La mayoría

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

Toda

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’0002700099113

</td>

<td style="text-align:left;">

La mayoría

</td>

<td style="text-align:left;">

La mayoría

</td>

<td style="text-align:left;">

Toda

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’0002700122913

</td>

<td style="text-align:left;">

La mayoría

</td>

<td style="text-align:left;">

Toda

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

Aproximadamente la mitad

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:right;">

3

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’0002700185215

</td>

<td style="text-align:left;">

La mayoría

</td>

<td style="text-align:left;">

Toda

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’0063700030110

</td>

<td style="text-align:left;">

Toda

</td>

<td style="text-align:left;">

Poca o nada

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

Toda

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’0063700238212

</td>

<td style="text-align:left;">

Aproximadamente la mitad

</td>

<td style="text-align:left;">

La mayoría

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

Toda

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:right;">

3

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’0063700381813

</td>

<td style="text-align:left;">

La mayoría

</td>

<td style="text-align:left;">

Aproximadamente la mitad

</td>

<td style="text-align:left;">

La mayoría

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’0064100400810

</td>

<td style="text-align:left;">

La mayoría

</td>

<td style="text-align:left;">

La mayoría

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

Toda

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’0064100522913

</td>

<td style="text-align:left;">

La mayoría

</td>

<td style="text-align:left;">

Poca o nada

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’0064100566114

</td>

<td style="text-align:left;">

La mayoría

</td>

<td style="text-align:left;">

Toda

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

Aproximadamente la mitad

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:right;">

3

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’0064100740610

</td>

<td style="text-align:left;">

La mayoría

</td>

<td style="text-align:left;">

Toda

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

Toda

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’0064100818910

</td>

<td style="text-align:left;">

La mayoría

</td>

<td style="text-align:left;">

Toda

</td>

<td style="text-align:left;">

La mayoría

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’0064101095409

</td>

<td style="text-align:left;">

La mayoría

</td>

<td style="text-align:left;">

Toda

</td>

<td style="text-align:left;">

Menos de la mitad

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:right;">

3

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’0064101735111

</td>

<td style="text-align:left;">

Menos de la mitad

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

Menos de la mitad

</td>

<td style="text-align:left;">

La mayoría

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’0064102525911

</td>

<td style="text-align:left;">

Toda

</td>

<td style="text-align:left;">

Toda

</td>

<td style="text-align:left;">

Aproximadamente la mitad

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’0210000035314

</td>

<td style="text-align:left;">

La mayoría

</td>

<td style="text-align:left;">

La mayoría

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

Toda

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’0210000090507

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

Menos de la mitad

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

Toda

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’0210000140808

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

Toda

</td>

<td style="text-align:left;">

Poca o nada

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’0411100009107

</td>

<td style="text-align:left;">

Toda

</td>

<td style="text-align:left;">

La mayoría

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

Toda

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’0411100030814

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

Aproximadamente la mitad

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

Toda

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’0411100036106

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

La mayoría

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

Toda

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’0411100055106

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

Aproximadamente la mitad

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

Toda

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’0411100072013

</td>

<td style="text-align:left;">

La mayoría

</td>

<td style="text-align:left;">

Aproximadamente la mitad

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

Poca o nada

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:right;">

3

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’0413100024507

</td>

<td style="text-align:left;">

Toda

</td>

<td style="text-align:left;">

Aproximadamente la mitad

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

La mayoría

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:right;">

3

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’0441000013414

</td>

<td style="text-align:left;">

La mayoría

</td>

<td style="text-align:left;">

Toda

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

Toda

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’0610100023908

</td>

<td style="text-align:left;">

La mayoría

</td>

<td style="text-align:left;">

La mayoría

</td>

<td style="text-align:left;">

Toda

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’0610100077906

</td>

<td style="text-align:left;">

Toda

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

Toda

</td>

<td style="text-align:left;">

Poca o nada

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’0673800045808

</td>

<td style="text-align:left;">

La mayoría

</td>

<td style="text-align:left;">

Aproximadamente la mitad

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

Toda

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:right;">

3

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’0673800119808

</td>

<td style="text-align:left;">

La mayoría

</td>

<td style="text-align:left;">

Aproximadamente la mitad

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’0678000010912

</td>

<td style="text-align:left;">

Toda

</td>

<td style="text-align:left;">

La mayoría

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

Toda

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’0678000017911

</td>

<td style="text-align:left;">

Menos de la mitad

</td>

<td style="text-align:left;">

Aproximadamente la mitad

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

Aproximadamente la mitad

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’0681200028512

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

Aproximadamente la mitad

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

Toda

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’0681200037513

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

Toda

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

La mayoría

</td>

<td style="text-align:left;">

Toda

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’0912000029214

</td>

<td style="text-align:left;">

La mayoría

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

Toda

</td>

<td style="text-align:left;">

Toda

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’0912100036110

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

Toda

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

Menos de la mitad

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’0912100048710

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

Poca o nada

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

Toda

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’0917600003408

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

Menos de la mitad

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

Toda

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’0917900001507

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

Toda

</td>

<td style="text-align:left;">

Poca o nada

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’0933800014509

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

Aproximadamente la mitad

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

La mayoría

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’0944800002908

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

Aproximadamente la mitad

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

Aproximadamente la mitad

</td>

<td style="text-align:left;">

Toda

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’1026500043108

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

Aproximadamente la mitad

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

Toda

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’1111200003612

</td>

<td style="text-align:left;">

Toda

</td>

<td style="text-align:left;">

La mayoría

</td>

<td style="text-align:left;">

Toda

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’1111200007709

</td>

<td style="text-align:left;">

Aproximadamente la mitad

</td>

<td style="text-align:left;">

La mayoría

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

Poca o nada

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:right;">

3

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’1111200025508

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

Toda

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

Toda

</td>

<td style="text-align:left;">

La mayoría

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’1111500016413

</td>

<td style="text-align:left;">

La mayoría

</td>

<td style="text-align:left;">

Toda

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

Toda

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’1111500024807

</td>

<td style="text-align:left;">

Toda

</td>

<td style="text-align:left;">

La mayoría

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

Toda

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’1111500050314

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

Poca o nada

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

Toda

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’1112500008310

</td>

<td style="text-align:left;">

La mayoría

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

Toda

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’1113100013613

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

La mayoría

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

Toda

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’1114100042113

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

La mayoría

</td>

<td style="text-align:left;">

La mayoría

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

Toda

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’1114100052713

</td>

<td style="text-align:left;">

La mayoría

</td>

<td style="text-align:left;">

Toda

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

Toda

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’1215100121912

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

La mayoría

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

Toda

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’1215100127013

</td>

<td style="text-align:left;">

La mayoría

</td>

<td style="text-align:left;">

La mayoría

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

Toda

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’1220000000309

</td>

<td style="text-align:left;">

Toda

</td>

<td style="text-align:left;">

La mayoría

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

Toda

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’1220000015610

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

Menos de la mitad

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

La mayoría

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’1224500002815

</td>

<td style="text-align:left;">

La mayoría

</td>

<td style="text-align:left;">

La mayoría

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

Aproximadamente la mitad

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’1411100034409

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

Poca o nada

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

Toda

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’1511100015108

</td>

<td style="text-align:left;">

Menos de la mitad

</td>

<td style="text-align:left;">

Poca o nada

</td>

<td style="text-align:left;">

Menos de la mitad

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’1511100022411

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

Aproximadamente la mitad

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

Toda

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’1511100045309

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

Aproximadamente la mitad

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

Toda

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’1511100067610

</td>

<td style="text-align:left;">

Aproximadamente la mitad

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

Toda

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’1610100091713

</td>

<td style="text-align:left;">

Toda

</td>

<td style="text-align:left;">

La mayoría

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

La mayoría

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’1613100056411

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

Aproximadamente la mitad

</td>

<td style="text-align:left;">

Toda

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’1615100020609

</td>

<td style="text-align:left;">

La mayoría

</td>

<td style="text-align:left;">

Aproximadamente la mitad

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

La mayoría

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’1615100031912

</td>

<td style="text-align:left;">

Aproximadamente la mitad

</td>

<td style="text-align:left;">

Menos de la mitad

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

Menos de la mitad

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’1816400083714

</td>

<td style="text-align:left;">

Toda

</td>

<td style="text-align:left;">

Aproximadamente la mitad

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

Aproximadamente la mitad

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’1816400107212

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

Aproximadamente la mitad

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

Menos de la mitad

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’1850000101409

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

Menos de la mitad

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

Toda

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’1857200027005

</td>

<td style="text-align:left;">

Aproximadamente la mitad

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

La mayoría

</td>

<td style="text-align:left;">

La mayoría

</td>

<td style="text-align:left;">

Aproximadamente la mitad

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

4

</td>

</tr>

<tr>

<td style="text-align:left;">

’1857500010303

</td>

<td style="text-align:left;">

Toda

</td>

<td style="text-align:left;">

La mayoría

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

Toda

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’1857500022812

</td>

<td style="text-align:left;">

La mayoría

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

Aproximadamente la mitad

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’1857600005803

</td>

<td style="text-align:left;">

Toda

</td>

<td style="text-align:left;">

Menos de la mitad

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

Toda

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’1857600008809

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

La mayoría

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

Toda

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’2010000003412

</td>

<td style="text-align:left;">

Aproximadamente la mitad

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

Toda

</td>

<td style="text-align:left;">

Aproximadamente la mitad

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’2015000001314

</td>

<td style="text-align:left;">

La mayoría

</td>

<td style="text-align:left;">

La mayoría

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

Toda

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’2031200003314

</td>

<td style="text-align:left;">

La mayoría

</td>

<td style="text-align:left;">

La mayoría

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

Toda

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’2106800006205

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

Aproximadamente la mitad

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

Aproximadamente la mitad

</td>

<td style="text-align:left;">

La mayoría

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’2135500004809

</td>

<td style="text-align:left;">

Menos de la mitad

</td>

<td style="text-align:left;">

Menos de la mitad

</td>

<td style="text-align:left;">

La mayoría

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’2510100061415

</td>

<td style="text-align:left;">

Toda

</td>

<td style="text-align:left;">

La mayoría

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

Toda

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’252

</td>

<td style="text-align:left;">

La mayoría

</td>

<td style="text-align:left;">

Aproximadamente la mitad

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

Toda

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:right;">

3

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’26625

</td>

<td style="text-align:left;">

Toda

</td>

<td style="text-align:left;">

Poca o nada

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

Toda

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’27705

</td>

<td style="text-align:left;">

Toda

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

Poca o nada

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’27869

</td>

<td style="text-align:left;">

La mayoría

</td>

<td style="text-align:left;">

Toda

</td>

<td style="text-align:left;">

Toda

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’3670000008013

</td>

<td style="text-align:left;">

Menos de la mitad

</td>

<td style="text-align:left;">

La mayoría

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

La mayoría

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’7394

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

La mayoría

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

Toda

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

2

</td>

</tr>

</tbody>

</table>

Krippendorff’s alpha

Subjects = 349 Raters = 6 alpha = 0.397

What ar the folios that have the most issues.

``` r
print(kable(sort(table(bad_folios))))
```

<table>

<thead>

<tr>

<th style="text-align:left;">

bad\_folios

</th>

<th style="text-align:right;">

Freq

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

’0000400020312

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;">

’0000400111912

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;">

’0000400190015

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;">

’0000500058507

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;">

’0000500102110

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;">

’0000600019613

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;">

’0000600021806

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;">

’0000600024610

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;">

’0000600035210

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;">

’0000600149312

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;">

’0000600170709

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;">

’0000600247213

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;">

’0000600291210

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;">

’0000700012515

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;">

’0000700013215

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;">

’0000700049211

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;">

’0000700077705

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;">

’0000700100909

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;">

’0000800073014

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;">

’0000900130914

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;">

’0000900216813

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;">

’0001000057812

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;">

’0001000062110

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;">

’0001000146208

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;">

’0001100081703

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;">

’0001100171309

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;">

’0001100289115

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;">

’0001100366708

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;">

’0001100373615

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;">

’0001100395711

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;">

’0001100502111

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;">

’0001200150109

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;">

’0001200202407

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;">

’0001300003707

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;">

’0001300010014

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;">

’0001400025309

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;">

’0001400043706

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;">

’0001700020906

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;">

’0001700027708

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;">

’0001700203910

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;">

’0002000100913

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;">

’0002000120512

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;">

’0002100030807

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;">

’0002200038205

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;">

’0002200056808

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;">

’0002200158108

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;">

’0002700067612

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;">

’0002700099113

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;">

’0002700185215

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;">

’0063500015815

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;">

’0063700030110

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;">

’0063700057807

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;">

’0063700238212

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;">

’0063700261615

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;">

’0063700381813

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;">

’0064100029408

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;">

’0064100060013

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;">

’0064100332409

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;">

’0064100400810

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;">

’0064100551415

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;">

’0064100566114

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;">

’0064100740610

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;">

’0064100818910

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;">

’0064101519911

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;">

’0064101584110

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;">

’0064101676309

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;">

’0064101996410

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;">

’0064103230113

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;">

’0210000015405

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;">

’0210000035314

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;">

’0210000090507

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;">

’0210000140808

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;">

’0411100009107

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;">

’0411100030814

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;">

’0411100036106

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;">

’0411100055106

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;">

’0411100072013

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;">

’0416000007815

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;">

’0441000013414

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;">

’0610100064610

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;">

’0610100066912

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;">

’0610100077906

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;">

’0610100166512

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;">

’0673800119808

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;">

’0674700008307

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;">

’0678000017911

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;">

’0681200014207

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;">

’0681200028512

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;">

’0681200032313

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;">

’0810000004911

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;">

’0912000029214

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;">

’0912100016013

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;">

’0912100016603

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;">

’0912100036110

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;">

’0917500004813

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;">

’0917600003408

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;">

’0918600000510

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;">

’0933800014509

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;">

’0944800002908

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;">

’1026500013908

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;">

’1111200003612

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;">

’1111200025508

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;">

’1111200046115

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;">

’1111500016413

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;">

’1113100013613

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;">

’1113100032713

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;">

’1114100004905

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;">

’1114100042113

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;">

’1114100052713

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;">

’1116100021513

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;">

’1119500000306

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;">

’1215100061809

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;">

’1215100121912

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;">

’1220000000309

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;">

’1220000015610

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;">

’1222600012909

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;">

’1224500002815

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;">

’1411100030410

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;">

’1412000005508

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;">

’1610100144507

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;">

’1610100168713

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;">

’1615100020609

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;">

’1816400010805

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;">

’1816400083714

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;">

’1850000028006

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;">

’1850000101409

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;">

’1857200027005

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;">

’1857200172714

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;">

’1857500010303

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;">

’1857500022812

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;">

’1857500071010

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;">

’1857600008809

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;">

’1857600018305

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;">

’2010000003412

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;">

’2015000001314

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;">

’2031200003314

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;">

’2106800006205

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;">

’252

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;">

’27869

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;">

’29725

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;">

’3670000008013

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;">

’7394

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;">

’0000400124810

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’0000700001309

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’0000700003014

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’0000700048104

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’0000700207914

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’0000900005113

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’0000900185907

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’0000900248713

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’0000900248810

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’0001000019203

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’0001000087410

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’0001200265707

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’0001200313314

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’0001600284910

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’0002000005615

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’0002700040515

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’0063500110314

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’0063700065013

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’0063700328414

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’0064100319507

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’0064100522913

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’0064101095409

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’0064101735111

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’0064102525911

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’0064103030312

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’0210000030806

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’0413100024507

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’0610100023908

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’0610100092815

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’0612100019614

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’0637000025309

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’0673800045808

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’0678000010912

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’0681200037513

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’0817000010910

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’0912100048710

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’0912100065110

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’1111500024807

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’1111500050314

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’1112500008310

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’1117100000109

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’1215100138911

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’1411100034409

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’1511100015108

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’1511100045309

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’1511100067610

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’1610100027310

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’1613100056411

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’1816400107212

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’1857600005803

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’1857600021710

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’2014300001807

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’2135500004809

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’2136

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’2510100035511

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’26625

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’27705

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’0000400209014

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’0000900156715

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’0001000162110

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’0001100183308

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’0001100570007

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’0001200067405

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’0001200073410

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’0001300041011

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’0001700078810

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’0002000155613

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’0002700053513

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’0002700122913

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’0063700025011

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’0064102522611

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’0821000005107

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’0917900001507

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’1026500043108

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’1111200007709

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’1111200025211

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’1215100127013

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’1511100022411

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’1610100091713

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’2510100061415

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’0000800101312

</td>

<td style="text-align:right;">

4

</td>

</tr>

<tr>

<td style="text-align:left;">

’1615100031912

</td>

<td style="text-align:right;">

4

</td>

</tr>

<tr>

<td style="text-align:left;">

’0000900247913

</td>

<td style="text-align:right;">

5

</td>

</tr>

</tbody>

</table>
