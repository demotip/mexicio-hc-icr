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

icr <- read_csv("./data_raw/Big Data y Acceso a Info en México_June 18, 2019_07.csv")

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

all_dummy <- c(binary_vars, yes_nos)

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

    ## '1215100138911 '0064100551415 '1816400174209 '13434 '0064103030312 '0001100366708 '0064100272013 '0000900060011 '1820000005214 '0210000076309 '1220000015610 '0001200292112 '0064100319507 '0063700328414 '1611100061508 '1511100045309 '0681200014207 '0001100502111 '0411100036106 '0000600019613 '0000600024610 '0000800013207 '0912100048710 '1131800010115 '0001500069106 '1850000060008 '1411100034409 '0063500110314 '1115100056514 '0002000155613 '0064100730107 '0917600003408 '0063700531114 '0001100109006 '1026500043108 '0001300069114 '0002000051413 '0001000062110 '1613100027605 '0001100077115 '0000600247213 '0413100024610 '0000700049211 '1221500003108 '0002700002114 '0912100036110 '0001000019203 '0001100341111 '0210000090507 '0001000057812 '1610100168713 '0610100167711 '0000600170709 '1114100004905 '1031500036414 '1215100121912 '1610100016415 '0000700048104 '0000600291210 '0001100081703 '0000600007209 '0001100099410 '0000400190015 '2136 '0064100033311 '0001200376812 '0001300025410 '0673800130909 '1117100000109 '0411100055106 '0411100030814 '2510100095613 '0817000010910 '0000400111912 '1031500027807 '0610400025214 '1615100011805 '0002700158815 '0002700057714 '0063700279309 '0063300006713 '1113100013613 '1850000101409 '0810000004911 '0002700067612 '1117100008409 '5311000000910 '0064100029408 '0063700179110 '0063700065013 '1111200046115 '0917500004813 '1857600008809 '0064100029003 '1220000006706 '1221500003113 '0001200317608 '0210000078606 '1031500024406 '0000500098606 '0001400015513 '1113100023410 '0001400002203 '1615100021912 '0001600091108 '0610100051615 '0001200365514 '0000900078812

<table>

<tbody>

<tr>

</tr>

</tbody>

</table>

``` r
kable(cat(one_coder$folio_id))
```

    ## '0000400020312 '0000400124810 '0000400155810 '0000400317213 '0000500062605 '0000600021806 '0000600035210 '0000600149312 '0000600199813 '0000600286311 '0000700001309 '0000700003014 '0000700012515 '0000700013215 '0000700069107 '0000700100909 '0000700207914 '0000800073014 '0000800101312 '0000900030811 '0000900130914 '0000900247913 '0000900248713 '0001000162110 '0001100086412 '0001100171606 '0001100183308 '0001100246213 '0001100289115 '0001100373615 '0001100395711 '0001100570007 '0001200061814 '0001200067405 '0001200121815 '0001200150109 '0001200202407 '0001200265707 '0001200313314 '0001300000410 '0001300002110 '0001300003707 '0001400043706 '0001700020906 '0001700027708 '0001700078810 '0001700156608 '0001700198605 '0002000100913 '0002000120512 '0002100021114 '0002200038205 '0002200056808 '0002200158108 '0002700040515 '0002700053513 '0002700099113 '0002700122913 '0002700185215 '0063500015815 '0063700025011 '0063700030110 '0063700057807 '0063700238212 '0063700261615 '0063700381813 '0064100060013 '0064100332409 '0064100522913 '0064100566114 '0064100740610 '0064100818910 '0064101095409 '0064101519911 '0064101676309 '0064102219210 '0064102522611 '0064102525911 '0064103177313 '0210000035314 '0210000132008 '0411100009107 '0413100024507 '0441000013414 '0610100023908 '0610100064610 '0610100066912 '0612100019614 '0632000013606 '0673800045808 '0673800046914 '0673800119808 '0673800138707 '0674700008307 '0678000017911 '0681200028512 '0681200032313 '0681200050914 '0814000011913 '0912100016013 '0912100016603 '0912100031114 '0912100034910 '0918600000510 '0933800014509 '1026500013908 '1110000000112 '1111200003612 '1111200007709 '1111200025508 '1111500016413 '1111500024807 '1111500050314 '1113100032713 '1114100042113 '1114100052713 '1119500000306 '1132100013412 '1215100061809 '1215100127013 '1219700011408 '1220000000309 '1222300007013 '1222600012909 '1224500002815 '1232900010213 '1407500014609 '1411100030410 '1412000005508 '1507500005612 '1511100015108 '1511100022411 '1610100013408 '1610100027310 '1610100032814 '1610100091713 '1613100000607 '1613100039512 '1613100054314 '1615100020609 '17765 '1810000004907 '1816400010805 '1816400083714 '1816400107212 '1816400134811 '1819100004013 '1850000028006 '1857200172714 '1857500022812 '1857500071010 '1857600005803 '1857600021710 '1857700015910 '19213 '2014300001807 '2015000001314 '2015000003612 '2031200003314 '2099900005613 '2099900021410 '2135500004809 '2510100035511 '2510100061415 '252 '26625 '27705 '27869 '2901000010314 '29725 '3670000008013 '4220700001412 '6184 '7394

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

  print( sprintf("Binary variable %s", binary_vars[i]))
  
  ratings_out <- data_wide %>%
    filter(rater_var > 0 | is.na(rater_var))

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

’0000400209014

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

’0000900248810

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

’0064100400810

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

’0411100072013

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

’0610100109913

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

’0908500003207

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

’1219700039614

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

0.4108691

</td>

<td style="text-align:right;">

2.3e-06

</td>

<td style="text-align:right;">

0.2426032

</td>

<td style="text-align:right;">

0.5610342

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(k)

</td>

<td style="text-align:right;">

0.6115822

</td>

<td style="text-align:right;">

2.3e-06

</td>

<td style="text-align:right;">

0.4196723

</td>

<td style="text-align:right;">

0.7426335

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(A,1)

</td>

<td style="text-align:right;">

0.4279950

</td>

<td style="text-align:right;">

9.0e-07

</td>

<td style="text-align:right;">

0.2616378

</td>

<td style="text-align:right;">

0.5753267

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(A,k)

</td>

<td style="text-align:right;">

0.6281539

</td>

<td style="text-align:right;">

9.0e-07

</td>

<td style="text-align:right;">

0.4446325

</td>

<td style="text-align:right;">

0.7535556

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(C,1)

</td>

<td style="text-align:right;">

0.4278552

</td>

<td style="text-align:right;">

9.0e-07

</td>

<td style="text-align:right;">

0.2609081

</td>

<td style="text-align:right;">

0.5754928

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(C,k)

</td>

<td style="text-align:right;">

0.6280206

</td>

<td style="text-align:right;">

9.0e-07

</td>

<td style="text-align:right;">

0.4435140

</td>

<td style="text-align:right;">

0.7537363

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

0.46

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

0.45

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

0.46

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

0.24

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

1

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;">

katia

</td>

<td style="text-align:right;">

0.45

</td>

<td style="text-align:right;">

0.24

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

NA

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

1

</td>

</tr>

</tbody>

</table>

\[1\] “Kendalls W is: 0.672169415037198”

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

’0000500058507

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

’0000500102110

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

’0002100030807

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

’0064100400810

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

’0064100581414

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

’0210000140808

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

’0411100072013

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

’0413000000715

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

’0908500003207

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

’1109000003915

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

0

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

’1132100010608

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

0

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

’1800100011015

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

’1857500010303

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

0.6060262

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.4689557

</td>

<td style="text-align:right;">

0.7180759

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(k)

</td>

<td style="text-align:right;">

0.7764297

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.6659679

</td>

<td style="text-align:right;">

0.8518619

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(A,1)

</td>

<td style="text-align:right;">

0.6057017

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.4699639

</td>

<td style="text-align:right;">

0.7172245

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(A,k)

</td>

<td style="text-align:right;">

0.7761938

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.6669761

</td>

<td style="text-align:right;">

0.8512986

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(C,1)

</td>

<td style="text-align:right;">

0.6091704

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.4723009

</td>

<td style="text-align:right;">

0.7207060

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(C,k)

</td>

<td style="text-align:right;">

0.7787106

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.6689482

</td>

<td style="text-align:right;">

0.8534984

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

0.69

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

0.54

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

0.69

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

0.59

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

0.77

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

NA

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

katia

</td>

<td style="text-align:right;">

0.54

</td>

<td style="text-align:right;">

0.59

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

0.77

</td>

<td style="text-align:right;">

0.41

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

\[1\] “Kendalls W is: 0.769250576783373”

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

’0000500102110

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

’0000900185907

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

’0001100189506

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

’0064101584110

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

’0210000015405

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

’0413000000715

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

’0636300016008

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

’0912000029214

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

’1117100054412

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

’1610100144507

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

’2000100002903

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

0.6352459

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.5047481

</td>

<td style="text-align:right;">

0.7405243

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(k)

</td>

<td style="text-align:right;">

0.7972396

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.6970595

</td>

<td style="text-align:right;">

0.8656505

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(A,1)

</td>

<td style="text-align:right;">

0.6703127

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.5432037

</td>

<td style="text-align:right;">

0.7691640

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(A,k)

</td>

<td style="text-align:right;">

0.8211181

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.7282306

</td>

<td style="text-align:right;">

0.8827746

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(C,1)

</td>

<td style="text-align:right;">

0.6591597

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.5339871

</td>

<td style="text-align:right;">

0.7588788

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(C,k)

</td>

<td style="text-align:right;">

0.8136485

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.7212157

</td>

<td style="text-align:right;">

0.8766286

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

1.00

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

NA

</td>

</tr>

<tr>

<td style="text-align:left;">

carmen

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

NA

</td>

<td style="text-align:right;">

0.46

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

1.0

</td>

<td style="text-align:right;">

NA

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

1.0

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

0.48

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

0.87

</td>

<td style="text-align:right;">

0.7

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

\[1\] “Kendalls W is: 0.789598068028146”

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

’0000500058507

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

0.5

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

NaN

</td>

<td style="text-align:right;">

0.5

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

0.5

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’0064100400810

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

0.5

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’0413100058710

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

NaN

</td>

<td style="text-align:right;">

0.5

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’0636300016008

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

NaN

</td>

<td style="text-align:right;">

0.5

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

0.5

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’0908500003207

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

NaN

</td>

<td style="text-align:right;">

0.5

</td>

<td style="text-align:right;">

2

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

NaN

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.5

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

0.5

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

0.7195885

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.6110607

</td>

<td style="text-align:right;">

0.8038613

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(k)

</td>

<td style="text-align:right;">

0.8528037

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.7800769

</td>

<td style="text-align:right;">

0.9024675

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(A,1)

</td>

<td style="text-align:right;">

0.7335122

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.6297482

</td>

<td style="text-align:right;">

0.8138511

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(A,k)

</td>

<td style="text-align:right;">

0.8613869

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.7934286

</td>

<td style="text-align:right;">

0.9079981

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(C,1)

</td>

<td style="text-align:right;">

0.7351629

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.6308165

</td>

<td style="text-align:right;">

0.8154674

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(C,k)

</td>

<td style="text-align:right;">

0.8623941

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.7941399

</td>

<td style="text-align:right;">

0.9089000

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

1

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

NA

</td>

</tr>

<tr>

<td style="text-align:left;">

carmen

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

1

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

0.79

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

0.44

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

1

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

0.79

</td>

<td style="text-align:right;">

0.44

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

\[1\] “Kendalls W is: 0.85942709454659”

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

’0001100189506

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

’0001400025309

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

’0001700203910

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

’0064100581414

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

’0610100166512

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

’0637000025309

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

’0912000029214

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

’0918900000214

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

’1610100144507

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

’1615100031912

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

0.5948676

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.4554238

</td>

<td style="text-align:right;">

0.7094329

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(k)

</td>

<td style="text-align:right;">

0.7682519

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.6537496

</td>

<td style="text-align:right;">

0.8464432

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(A,1)

</td>

<td style="text-align:right;">

0.6725886

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.5415902

</td>

<td style="text-align:right;">

0.7725729

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(A,k)

</td>

<td style="text-align:right;">

0.8226284

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.7264199

</td>

<td style="text-align:right;">

0.8848964

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(C,1)

</td>

<td style="text-align:right;">

0.6550636

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.5288740

</td>

<td style="text-align:right;">

0.7557798

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(C,k)

</td>

<td style="text-align:right;">

0.8108762

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.7170684

</td>

<td style="text-align:right;">

0.8747933

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

0.68

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

0.66

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

0.68

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

0.46

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

0.55

</td>

</tr>

<tr>

<td style="text-align:left;">

katia

</td>

<td style="text-align:right;">

0.66

</td>

<td style="text-align:right;">

0.46

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

0.68

</td>

<td style="text-align:right;">

0.55

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

\[1\] “Kendalls W is: 0.789580111602076”

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

’0001300010014

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

’0064103230113

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

’0210000030806

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

’0944800002908

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

’1111500045614

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

0.2343653

</td>

<td style="text-align:right;">

0.0051988

</td>

<td style="text-align:right;">

0.0551824

</td>

<td style="text-align:right;">

0.4074133

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(k)

</td>

<td style="text-align:right;">

0.4086661

</td>

<td style="text-align:right;">

0.0051988

</td>

<td style="text-align:right;">

0.1164993

</td>

<td style="text-align:right;">

0.6081809

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(A,1)

</td>

<td style="text-align:right;">

0.2793968

</td>

<td style="text-align:right;">

0.0009773

</td>

<td style="text-align:right;">

0.1044012

</td>

<td style="text-align:right;">

0.4460718

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(A,k)

</td>

<td style="text-align:right;">

0.4667705

</td>

<td style="text-align:right;">

0.0009773

</td>

<td style="text-align:right;">

0.2089828

</td>

<td style="text-align:right;">

0.6449594

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(C,1)

</td>

<td style="text-align:right;">

0.2832749

</td>

<td style="text-align:right;">

0.0009773

</td>

<td style="text-align:right;">

0.1050138

</td>

<td style="text-align:right;">

0.4514874

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(C,k)

</td>

<td style="text-align:right;">

0.4715475

</td>

<td style="text-align:right;">

0.0009773

</td>

<td style="text-align:right;">

0.2094283

</td>

<td style="text-align:right;">

0.6501455

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

0.69

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

carmen

</td>

<td style="text-align:right;">

0.69

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

NA

</td>

<td style="text-align:right;">

NA

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

sandra

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

\[1\] “Kendalls W is: 0.546872444808064”

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

’0000500058507

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

’0000900005113

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

’0000900113814

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

0

</td>

<td style="text-align:right;">

1

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

’0000900248810

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

’0001000146208

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

’0001100189506

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

’0001200073410

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

’0001600284910

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

’0001700203910

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

’0002000005615

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

’0064100581414

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

’0210000015405

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

’0210000030806

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

’0413100058710

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

’0610100077906

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

0

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

’0610100109913

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

’0908500003207

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

’0912100065110

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

’1108300002007

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

’1111500045614

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

’1610100109713

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

’1615100031912

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

’1857500010303

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

’2010000003412

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

’2135500006106

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

’3098

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

0.3864821

</td>

<td style="text-align:right;">

8.70e-06

</td>

<td style="text-align:right;">

0.2157803

</td>

<td style="text-align:right;">

0.5404948

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(k)

</td>

<td style="text-align:right;">

0.5871553

</td>

<td style="text-align:right;">

8.70e-06

</td>

<td style="text-align:right;">

0.3831765

</td>

<td style="text-align:right;">

0.7264482

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(A,1)

</td>

<td style="text-align:right;">

0.3658669

</td>

<td style="text-align:right;">

2.17e-05

</td>

<td style="text-align:right;">

0.1958576

</td>

<td style="text-align:right;">

0.5216173

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(A,k)

</td>

<td style="text-align:right;">

0.5657058

</td>

<td style="text-align:right;">

2.17e-05

</td>

<td style="text-align:right;">

0.3552154

</td>

<td style="text-align:right;">

0.7110001

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(C,1)

</td>

<td style="text-align:right;">

0.3701739

</td>

<td style="text-align:right;">

2.17e-05

</td>

<td style="text-align:right;">

0.1974410

</td>

<td style="text-align:right;">

0.5269383

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(C,k)

</td>

<td style="text-align:right;">

0.5702499

</td>

<td style="text-align:right;">

2.17e-05

</td>

<td style="text-align:right;">

0.3570883

</td>

<td style="text-align:right;">

0.7154900

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

0.37

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

1.0

</td>

<td style="text-align:right;">

0.25

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

0.37

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

0.42

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

0.83

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

NA

</td>

<td style="text-align:right;">

1.0

</td>

<td style="text-align:right;">

NA

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

0.25

</td>

<td style="text-align:right;">

0.42

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

0.83

</td>

<td style="text-align:right;">

0.3

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

\[1\] “Kendalls W is: 0.648782481067849”

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

’0000500047705

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

’0000500102110

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

’0001200073410

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

’0064100581414

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

’0821000005107

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

’0912000029214

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

’0912100065110

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

’1109000003915

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

<tr>

<td style="text-align:left;">

’1114100000412

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

’1615100031912

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

0.5542254

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.4067632

</td>

<td style="text-align:right;">

0.6776181

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(k)

</td>

<td style="text-align:right;">

0.7373228

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.6075389

</td>

<td style="text-align:right;">

0.8259495

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(A,1)

</td>

<td style="text-align:right;">

0.5566097

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.4100309

</td>

<td style="text-align:right;">

0.6793036

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(A,k)

</td>

<td style="text-align:right;">

0.7391886

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.6108739

</td>

<td style="text-align:right;">

0.8270236

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(C,1)

</td>

<td style="text-align:right;">

0.5572632

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.4098549

</td>

<td style="text-align:right;">

0.6802391

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(C,k)

</td>

<td style="text-align:right;">

0.7396988

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.6105861

</td>

<td style="text-align:right;">

0.8276713

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

0.66

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

NA

</td>

</tr>

<tr>

<td style="text-align:left;">

carmen

</td>

<td style="text-align:right;">

0.66

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

0.32

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

\-1.00

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

0.37

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

\-1.00

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

0.68

</td>

<td style="text-align:right;">

0.32

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

0.37

</td>

<td style="text-align:right;">

0.65

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

\[1\] “Kendalls W is: 0.753836241360979”

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

’0000400209014

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

’0001000146208

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

’0001300010014

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

’0002000005615

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

’0411100072013

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

’0610100109913

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

’0636300016008

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

’0908500003207

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

’0912000029214

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

’1026500058813

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

’2135500006106

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

NaN

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

0.4867569

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.3280897

</td>

<td style="text-align:right;">

0.6236157

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(k)

</td>

<td style="text-align:right;">

0.6816482

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.5243566

</td>

<td style="text-align:right;">

0.7890594

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(A,1)

</td>

<td style="text-align:right;">

0.4859304

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.3285627

</td>

<td style="text-align:right;">

0.6222651

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(A,k)

</td>

<td style="text-align:right;">

0.6809299

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.5251075

</td>

<td style="text-align:right;">

0.7880367

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(C,1)

</td>

<td style="text-align:right;">

0.4888210

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.3299161

</td>

<td style="text-align:right;">

0.6255437

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(C,k)

</td>

<td style="text-align:right;">

0.6834382

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.5264196

</td>

<td style="text-align:right;">

0.7904248

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

1.0

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

0.5

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

1.0

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

1.0

</td>

<td style="text-align:right;">

0.68

</td>

</tr>

<tr>

<td style="text-align:left;">

katia

</td>

<td style="text-align:right;">

0.5

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

1.0

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

NA

</td>

<td style="text-align:right;">

0.68

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

\[1\] “Kendalls W is: 0.616039668454384”

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

’0000500047705

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

’0000500102110

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

’0912000029214

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

’1111500045614

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

’1615100031912

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

’3098

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

0.6613839

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.5372119

</td>

<td style="text-align:right;">

0.7603820

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(k)

</td>

<td style="text-align:right;">

0.8151472

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.7238150

</td>

<td style="text-align:right;">

0.8775162

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(A,1)

</td>

<td style="text-align:right;">

0.6605313

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.5370120

</td>

<td style="text-align:right;">

0.7593862

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(A,k)

</td>

<td style="text-align:right;">

0.8145732

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.7237249

</td>

<td style="text-align:right;">

0.8769075

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(C,1)

</td>

<td style="text-align:right;">

0.6625279

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.5381995

</td>

<td style="text-align:right;">

0.7614233

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(C,k)

</td>

<td style="text-align:right;">

0.8159163

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.7246085

</td>

<td style="text-align:right;">

0.8781300

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

0.48

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

0.47

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

0.48

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

0.48

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

1

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

0.77

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

1

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

1.00

</td>

</tr>

<tr>

<td style="text-align:left;">

katia

</td>

<td style="text-align:right;">

0.47

</td>

<td style="text-align:right;">

0.48

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

0.77

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

</tr>

</tbody>

</table>

\[1\] “Kendalls W is: 0.758934905776391”

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

’0000400209014

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

’0001000146208

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

’0001300010014

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

’0002000005615

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

’0411100072013

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

’0610100109913

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

’0636300016008

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

’0908500003207

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

’0912000029214

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

’1026500058813

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

’2135500006106

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

NaN

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

0.4867569

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.3280897

</td>

<td style="text-align:right;">

0.6236157

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(k)

</td>

<td style="text-align:right;">

0.6816482

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.5243566

</td>

<td style="text-align:right;">

0.7890594

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(A,1)

</td>

<td style="text-align:right;">

0.4859304

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.3285627

</td>

<td style="text-align:right;">

0.6222651

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(A,k)

</td>

<td style="text-align:right;">

0.6809299

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.5251075

</td>

<td style="text-align:right;">

0.7880367

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(C,1)

</td>

<td style="text-align:right;">

0.4888210

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.3299161

</td>

<td style="text-align:right;">

0.6255437

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(C,k)

</td>

<td style="text-align:right;">

0.6834382

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.5264196

</td>

<td style="text-align:right;">

0.7904248

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

1.0

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

0.5

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

1.0

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

1.0

</td>

<td style="text-align:right;">

0.68

</td>

</tr>

<tr>

<td style="text-align:left;">

katia

</td>

<td style="text-align:right;">

0.5

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

1.0

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

NA

</td>

<td style="text-align:right;">

0.68

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

\[1\] “Kendalls W is: 0.616039668454384”

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

<tr>

<td style="text-align:left;">

’0064100400810

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

’0064101735111

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

’0410100003506

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

’0413100058710

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

’0944800002908

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

’1116100021513

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

’2135500006106

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

0.6600292

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.5355188

</td>

<td style="text-align:right;">

0.7593579

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(k)

</td>

<td style="text-align:right;">

0.8142349

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.7224518

</td>

<td style="text-align:right;">

0.8769116

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(A,1)

</td>

<td style="text-align:right;">

0.6611402

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.5364174

</td>

<td style="text-align:right;">

0.7603968

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(A,k)

</td>

<td style="text-align:right;">

0.8149832

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.7231950

</td>

<td style="text-align:right;">

0.8775193

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(C,1)

</td>

<td style="text-align:right;">

0.6592960

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.5341574

</td>

<td style="text-align:right;">

0.7589818

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(C,k)

</td>

<td style="text-align:right;">

0.8137404

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.7213533

</td>

<td style="text-align:right;">

0.8766895

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

0.77

</td>

<td style="text-align:right;">

NA

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

</tr>

<tr>

<td style="text-align:left;">

carmen

</td>

<td style="text-align:right;">

0.77

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

0.67

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

0.71

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

1

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

1.00

</td>

</tr>

<tr>

<td style="text-align:left;">

katia

</td>

<td style="text-align:right;">

0.56

</td>

<td style="text-align:right;">

0.67

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

0.71

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

</tr>

</tbody>

</table>

\[1\] “Kendalls W is: 0.810701990942247”

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

’0001300010014

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

’0064103230113

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

’0210000030806

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

’0944800002908

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

’1111500045614

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

0.2343653

</td>

<td style="text-align:right;">

0.0051988

</td>

<td style="text-align:right;">

0.0551824

</td>

<td style="text-align:right;">

0.4074133

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(k)

</td>

<td style="text-align:right;">

0.4086661

</td>

<td style="text-align:right;">

0.0051988

</td>

<td style="text-align:right;">

0.1164993

</td>

<td style="text-align:right;">

0.6081809

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(A,1)

</td>

<td style="text-align:right;">

0.2793968

</td>

<td style="text-align:right;">

0.0009773

</td>

<td style="text-align:right;">

0.1044012

</td>

<td style="text-align:right;">

0.4460718

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(A,k)

</td>

<td style="text-align:right;">

0.4667705

</td>

<td style="text-align:right;">

0.0009773

</td>

<td style="text-align:right;">

0.2089828

</td>

<td style="text-align:right;">

0.6449594

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(C,1)

</td>

<td style="text-align:right;">

0.2832749

</td>

<td style="text-align:right;">

0.0009773

</td>

<td style="text-align:right;">

0.1050138

</td>

<td style="text-align:right;">

0.4514874

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(C,k)

</td>

<td style="text-align:right;">

0.4715475

</td>

<td style="text-align:right;">

0.0009773

</td>

<td style="text-align:right;">

0.2094283

</td>

<td style="text-align:right;">

0.6501455

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

0.69

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

carmen

</td>

<td style="text-align:right;">

0.69

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

NA

</td>

<td style="text-align:right;">

NA

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

sandra

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

\[1\] “Kendalls W is: 0.546872444808064”

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

’0001100189506

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

’0001200073410

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

’0001700203910

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

’0411100072013

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

’0413100058710

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

’0912100065110

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

’1026500058813

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

’1613100056411

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

NaN

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

0.3998514

</td>

<td style="text-align:right;">

4.2e-06

</td>

<td style="text-align:right;">

0.2304467

</td>

<td style="text-align:right;">

0.5517810

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(k)

</td>

<td style="text-align:right;">

0.6006699

</td>

<td style="text-align:right;">

4.2e-06

</td>

<td style="text-align:right;">

0.4033685

</td>

<td style="text-align:right;">

0.7354030

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(A,1)

</td>

<td style="text-align:right;">

0.5184753

</td>

<td style="text-align:right;">

0.0e+00

</td>

<td style="text-align:right;">

0.3565253

</td>

<td style="text-align:right;">

0.6529095

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(A,k)

</td>

<td style="text-align:right;">

0.7085343

</td>

<td style="text-align:right;">

0.0e+00

</td>

<td style="text-align:right;">

0.5545442

</td>

<td style="text-align:right;">

0.8097406

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(C,1)

</td>

<td style="text-align:right;">

0.5039882

</td>

<td style="text-align:right;">

0.0e+00

</td>

<td style="text-align:right;">

0.3474017

</td>

<td style="text-align:right;">

0.6377986

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(C,k)

</td>

<td style="text-align:right;">

0.6964172

</td>

<td style="text-align:right;">

0.0e+00

</td>

<td style="text-align:right;">

0.5458363

</td>

<td style="text-align:right;">

0.7990173

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

0.46

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

1

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

carmen

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

edithimg

</td>

<td style="text-align:right;">

NA

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

NA

</td>

<td style="text-align:right;">

1

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

NA

</td>

<td style="text-align:right;">

1

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

katia

</td>

<td style="text-align:right;">

0.39

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

1

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

1

</td>

</tr>

</tbody>

</table>

\[1\] “Kendalls W is: 0.738430975263384”

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

’0000500047705

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

’0000500102110

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

’0912000029214

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

’1111500045614

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

’1615100031912

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

’3098

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

0.6613839

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.5372119

</td>

<td style="text-align:right;">

0.7603820

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(k)

</td>

<td style="text-align:right;">

0.8151472

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.7238150

</td>

<td style="text-align:right;">

0.8775162

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(A,1)

</td>

<td style="text-align:right;">

0.6605313

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.5370120

</td>

<td style="text-align:right;">

0.7593862

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(A,k)

</td>

<td style="text-align:right;">

0.8145732

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.7237249

</td>

<td style="text-align:right;">

0.8769075

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(C,1)

</td>

<td style="text-align:right;">

0.6625279

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.5381995

</td>

<td style="text-align:right;">

0.7614233

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(C,k)

</td>

<td style="text-align:right;">

0.8159163

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.7246085

</td>

<td style="text-align:right;">

0.8781300

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

0.48

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

0.47

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

0.48

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

0.48

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

1

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

0.77

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

1

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

1.00

</td>

</tr>

<tr>

<td style="text-align:left;">

katia

</td>

<td style="text-align:right;">

0.47

</td>

<td style="text-align:right;">

0.48

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

0.77

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

</tr>

</tbody>

</table>

\[1\] “Kendalls W is: 0.758934905776391”

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

<tr>

<td style="text-align:left;">

’0064100400810

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

’0064101735111

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

’0410100003506

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

’0413100058710

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

’0944800002908

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

’1116100021513

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

’2135500006106

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

0.6600292

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.5355188

</td>

<td style="text-align:right;">

0.7593579

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(k)

</td>

<td style="text-align:right;">

0.8142349

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.7224518

</td>

<td style="text-align:right;">

0.8769116

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(A,1)

</td>

<td style="text-align:right;">

0.6611402

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.5364174

</td>

<td style="text-align:right;">

0.7603968

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(A,k)

</td>

<td style="text-align:right;">

0.8149832

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.7231950

</td>

<td style="text-align:right;">

0.8775193

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(C,1)

</td>

<td style="text-align:right;">

0.6592960

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.5341574

</td>

<td style="text-align:right;">

0.7589818

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(C,k)

</td>

<td style="text-align:right;">

0.8137404

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.7213533

</td>

<td style="text-align:right;">

0.8766895

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

0.77

</td>

<td style="text-align:right;">

NA

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

</tr>

<tr>

<td style="text-align:left;">

carmen

</td>

<td style="text-align:right;">

0.77

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

0.67

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

0.71

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

1

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

1.00

</td>

</tr>

<tr>

<td style="text-align:left;">

katia

</td>

<td style="text-align:right;">

0.56

</td>

<td style="text-align:right;">

0.67

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

0.71

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

</tr>

</tbody>

</table>

\[1\] “Kendalls W is: 0.810701990942247”

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

’0000500047705

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

’0000500102110

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

’0912000029214

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

’1111500045614

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

’1615100031912

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

’3098

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

0.6613839

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.5372119

</td>

<td style="text-align:right;">

0.7603820

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(k)

</td>

<td style="text-align:right;">

0.8151472

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.7238150

</td>

<td style="text-align:right;">

0.8775162

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(A,1)

</td>

<td style="text-align:right;">

0.6605313

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.5370120

</td>

<td style="text-align:right;">

0.7593862

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(A,k)

</td>

<td style="text-align:right;">

0.8145732

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.7237249

</td>

<td style="text-align:right;">

0.8769075

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(C,1)

</td>

<td style="text-align:right;">

0.6625279

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.5381995

</td>

<td style="text-align:right;">

0.7614233

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(C,k)

</td>

<td style="text-align:right;">

0.8159163

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.7246085

</td>

<td style="text-align:right;">

0.8781300

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

0.48

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

0.47

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

0.48

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

0.48

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

1

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

0.77

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

1

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

1.00

</td>

</tr>

<tr>

<td style="text-align:left;">

katia

</td>

<td style="text-align:right;">

0.47

</td>

<td style="text-align:right;">

0.48

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

0.77

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

</tr>

</tbody>

</table>

\[1\] “Kendalls W is: 0.758934905776391”

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

’0001100189506

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

’0001200073410

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

’0001700203910

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

’0411100072013

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

’0413100058710

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

’0912100065110

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

’1026500058813

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

’1613100056411

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

NaN

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

0.3998514

</td>

<td style="text-align:right;">

4.2e-06

</td>

<td style="text-align:right;">

0.2304467

</td>

<td style="text-align:right;">

0.5517810

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(k)

</td>

<td style="text-align:right;">

0.6006699

</td>

<td style="text-align:right;">

4.2e-06

</td>

<td style="text-align:right;">

0.4033685

</td>

<td style="text-align:right;">

0.7354030

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(A,1)

</td>

<td style="text-align:right;">

0.5184753

</td>

<td style="text-align:right;">

0.0e+00

</td>

<td style="text-align:right;">

0.3565253

</td>

<td style="text-align:right;">

0.6529095

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(A,k)

</td>

<td style="text-align:right;">

0.7085343

</td>

<td style="text-align:right;">

0.0e+00

</td>

<td style="text-align:right;">

0.5545442

</td>

<td style="text-align:right;">

0.8097406

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(C,1)

</td>

<td style="text-align:right;">

0.5039882

</td>

<td style="text-align:right;">

0.0e+00

</td>

<td style="text-align:right;">

0.3474017

</td>

<td style="text-align:right;">

0.6377986

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(C,k)

</td>

<td style="text-align:right;">

0.6964172

</td>

<td style="text-align:right;">

0.0e+00

</td>

<td style="text-align:right;">

0.5458363

</td>

<td style="text-align:right;">

0.7990173

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

0.46

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

1

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

carmen

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

edithimg

</td>

<td style="text-align:right;">

NA

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

NA

</td>

<td style="text-align:right;">

1

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

NA

</td>

<td style="text-align:right;">

1

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

katia

</td>

<td style="text-align:right;">

0.39

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

1

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

1

</td>

</tr>

</tbody>

</table>

\[1\] “Kendalls W is: 0.738430975263384”

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

’0001000146208

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

’0210000030806

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

’0610100166512

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

’1108300002007

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

’1610100144507

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

0.5563651

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.4093008

</td>

<td style="text-align:right;">

0.6793062

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(k)

</td>

<td style="text-align:right;">

0.7389975

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.6100411

</td>

<td style="text-align:right;">

0.8270592

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(A,1)

</td>

<td style="text-align:right;">

0.5736177

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.4297441

</td>

<td style="text-align:right;">

0.6929163

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(A,k)

</td>

<td style="text-align:right;">

0.7523096

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.6298936

</td>

<td style="text-align:right;">

0.8358919

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(C,1)

</td>

<td style="text-align:right;">

0.5728990

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.4284966

</td>

<td style="text-align:right;">

0.6925192

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(C,k)

</td>

<td style="text-align:right;">

0.7517617

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.6286323

</td>

<td style="text-align:right;">

0.8356573

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

NA

</td>

<td style="text-align:right;">

NA

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

carmen

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

edithimg

</td>

<td style="text-align:right;">

NA

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

NA

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

0.83

</td>

</tr>

<tr>

<td style="text-align:left;">

katia

</td>

<td style="text-align:right;">

0.44

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

1

</td>

<td style="text-align:right;">

0.83

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

\[1\] “Kendalls W is: 0.670360939476897”

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

’0000900113814

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

0

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

’0064101735111

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

’0064103230113

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

’0210000030806

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

’0821000005107

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

’1613100056411

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

’1857500010303

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

0.7129659

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.6025475

</td>

<td style="text-align:right;">

0.7989649

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(k)

</td>

<td style="text-align:right;">

0.8486657

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.7738943

</td>

<td style="text-align:right;">

0.8997256

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(A,1)

</td>

<td style="text-align:right;">

0.7737940

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.6563662

</td>

<td style="text-align:right;">

0.8517984

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(A,k)

</td>

<td style="text-align:right;">

0.8853604

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.8097883

</td>

<td style="text-align:right;">

0.9288987

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(C,1)

</td>

<td style="text-align:right;">

0.7429750

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.6409844

</td>

<td style="text-align:right;">

0.8211875

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(C,k)

</td>

<td style="text-align:right;">

0.8671315

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.8012270

</td>

<td style="text-align:right;">

0.9120363

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

0.66

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

0.60

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

0.66

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

0.79

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

1

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

1

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

0.78

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

0.79

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

1

</td>

<td style="text-align:right;">

0.78

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

\[1\] “Kendalls W is: 0.833525252657699”

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

’0001000146208

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

’0210000030806

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

’0610100166512

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

’1108300002007

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

’1610100144507

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

0.5563651

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.4093008

</td>

<td style="text-align:right;">

0.6793062

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(k)

</td>

<td style="text-align:right;">

0.7389975

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.6100411

</td>

<td style="text-align:right;">

0.8270592

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(A,1)

</td>

<td style="text-align:right;">

0.5736177

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.4297441

</td>

<td style="text-align:right;">

0.6929163

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(A,k)

</td>

<td style="text-align:right;">

0.7523096

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.6298936

</td>

<td style="text-align:right;">

0.8358919

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(C,1)

</td>

<td style="text-align:right;">

0.5728990

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.4284966

</td>

<td style="text-align:right;">

0.6925192

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(C,k)

</td>

<td style="text-align:right;">

0.7517617

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.6286323

</td>

<td style="text-align:right;">

0.8356573

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

NA

</td>

<td style="text-align:right;">

NA

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

carmen

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

edithimg

</td>

<td style="text-align:right;">

NA

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

NA

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

0.83

</td>

</tr>

<tr>

<td style="text-align:left;">

katia

</td>

<td style="text-align:right;">

0.44

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

1

</td>

<td style="text-align:right;">

0.83

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

\[1\] “Kendalls W is: 0.670360939476897”

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

’0000500102110

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

’0001000146208

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

’0064100400810

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

’0064101584110

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

’0210000030806

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

’0610100166512

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

’0912000029214

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

’0912100065110

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

’1108300002007

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

’1117100054412

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

0

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

’1857200044606

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

’1857500010303

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

’2901000001406

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

0.5512250

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.4032092

</td>

<td style="text-align:right;">

0.6752483

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(k)

</td>

<td style="text-align:right;">

0.7349654

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.6040168

</td>

<td style="text-align:right;">

0.8243875

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(A,1)

</td>

<td style="text-align:right;">

0.5884161

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.4484542

</td>

<td style="text-align:right;">

0.7040623

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(A,k)

</td>

<td style="text-align:right;">

0.7634636

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.6474627

</td>

<td style="text-align:right;">

0.8430123

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(C,1)

</td>

<td style="text-align:right;">

0.5900936

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.4491639

</td>

<td style="text-align:right;">

0.7059333

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(C,k)

</td>

<td style="text-align:right;">

0.7647128

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.6480074

</td>

<td style="text-align:right;">

0.8442315

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

0.35

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

0.55

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

0.35

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

NA

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

0.83

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

0.75

</td>

</tr>

<tr>

<td style="text-align:left;">

katia

</td>

<td style="text-align:right;">

0.55

</td>

<td style="text-align:right;">

0.39

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

0.83

</td>

<td style="text-align:right;">

0.75

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

\[1\] “Kendalls W is: 0.744055071135618”

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

’0000900113814

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

0

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

’0001300010014

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

’0610100109913

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

’0637000025309

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

’1111500045614

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

0

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

’1615100031912

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

’1800100011015

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

’2901000001406

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

0.6478136

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.5203042

</td>

<td style="text-align:right;">

0.7500984

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(k)

</td>

<td style="text-align:right;">

0.8059309

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.7100451

</td>

<td style="text-align:right;">

0.8714094

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(A,1)

</td>

<td style="text-align:right;">

0.6773983

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.5547012

</td>

<td style="text-align:right;">

0.7734779

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(A,k)

</td>

<td style="text-align:right;">

0.8258049

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.7375655

</td>

<td style="text-align:right;">

0.8852144

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(C,1)

</td>

<td style="text-align:right;">

0.6707969

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.5485713

</td>

<td style="text-align:right;">

0.7676553

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(C,k)

</td>

<td style="text-align:right;">

0.8214398

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.7328717

</td>

<td style="text-align:right;">

0.8817867

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

NA

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

0.53

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

0.54

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

1.00

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

1

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

0.83

</td>

</tr>

<tr>

<td style="text-align:left;">

katia

</td>

<td style="text-align:right;">

0.53

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

1

</td>

<td style="text-align:right;">

0.83

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

\[1\] “Kendalls W is: 0.821403260188337”

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

’0001000146208

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

’0210000030806

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

’0610100166512

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

’1108300002007

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

’1610100144507

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

0.5563651

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.4093008

</td>

<td style="text-align:right;">

0.6793062

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(k)

</td>

<td style="text-align:right;">

0.7389975

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.6100411

</td>

<td style="text-align:right;">

0.8270592

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(A,1)

</td>

<td style="text-align:right;">

0.5736177

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.4297441

</td>

<td style="text-align:right;">

0.6929163

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(A,k)

</td>

<td style="text-align:right;">

0.7523096

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.6298936

</td>

<td style="text-align:right;">

0.8358919

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(C,1)

</td>

<td style="text-align:right;">

0.5728990

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.4284966

</td>

<td style="text-align:right;">

0.6925192

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(C,k)

</td>

<td style="text-align:right;">

0.7517617

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.6286323

</td>

<td style="text-align:right;">

0.8356573

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

NA

</td>

<td style="text-align:right;">

NA

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

carmen

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

edithimg

</td>

<td style="text-align:right;">

NA

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

NA

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

0.83

</td>

</tr>

<tr>

<td style="text-align:left;">

katia

</td>

<td style="text-align:right;">

0.44

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

1

</td>

<td style="text-align:right;">

0.83

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

\[1\] “Kendalls W is: 0.670360939476897”

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

’0001000146208

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

’0210000030806

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

’0610100166512

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

’1108300002007

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

’1610100144507

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

0.5563651

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.4093008

</td>

<td style="text-align:right;">

0.6793062

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(k)

</td>

<td style="text-align:right;">

0.7389975

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.6100411

</td>

<td style="text-align:right;">

0.8270592

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(A,1)

</td>

<td style="text-align:right;">

0.5736177

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.4297441

</td>

<td style="text-align:right;">

0.6929163

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(A,k)

</td>

<td style="text-align:right;">

0.7523096

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.6298936

</td>

<td style="text-align:right;">

0.8358919

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(C,1)

</td>

<td style="text-align:right;">

0.5728990

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.4284966

</td>

<td style="text-align:right;">

0.6925192

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(C,k)

</td>

<td style="text-align:right;">

0.7517617

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.6286323

</td>

<td style="text-align:right;">

0.8356573

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

NA

</td>

<td style="text-align:right;">

NA

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

carmen

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

edithimg

</td>

<td style="text-align:right;">

NA

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

NA

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

0.83

</td>

</tr>

<tr>

<td style="text-align:left;">

katia

</td>

<td style="text-align:right;">

0.44

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

1

</td>

<td style="text-align:right;">

0.83

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

\[1\] “Kendalls W is: 0.670360939476897”

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

0.5

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

0.5

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

0.8127725

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.7339757

</td>

<td style="text-align:right;">

0.8714179

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(k)

</td>

<td style="text-align:right;">

0.9074146

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.8616700

</td>

<td style="text-align:right;">

0.9386527

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(A,1)

</td>

<td style="text-align:right;">

0.8372161

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.7598033

</td>

<td style="text-align:right;">

0.8911679

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(A,k)

</td>

<td style="text-align:right;">

0.9207076

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.8768956

</td>

<td style="text-align:right;">

0.9487582

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(C,1)

</td>

<td style="text-align:right;">

0.8241701

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.7491439

</td>

<td style="text-align:right;">

0.8796090

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(C,k)

</td>

<td style="text-align:right;">

0.9136629

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.8708385

</td>

<td style="text-align:right;">

0.9428417

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

1.0

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

0.7

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

edithimg

</td>

<td style="text-align:right;">

NA

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

NA

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

0.68

</td>

</tr>

<tr>

<td style="text-align:left;">

katia

</td>

<td style="text-align:right;">

0.7

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

1.0

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

1

</td>

<td style="text-align:right;">

0.68

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

\[1\] “Kendalls W is: 0.726698278281849”

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

’0000900113814

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

0

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

’0064101735111

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

’0064103230113

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

’0210000030806

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

’0821000005107

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

’1613100056411

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

’1857500010303

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

0.7129659

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.6025475

</td>

<td style="text-align:right;">

0.7989649

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(k)

</td>

<td style="text-align:right;">

0.8486657

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.7738943

</td>

<td style="text-align:right;">

0.8997256

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(A,1)

</td>

<td style="text-align:right;">

0.7737940

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.6563662

</td>

<td style="text-align:right;">

0.8517984

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(A,k)

</td>

<td style="text-align:right;">

0.8853604

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.8097883

</td>

<td style="text-align:right;">

0.9288987

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(C,1)

</td>

<td style="text-align:right;">

0.7429750

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.6409844

</td>

<td style="text-align:right;">

0.8211875

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(C,k)

</td>

<td style="text-align:right;">

0.8671315

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.8012270

</td>

<td style="text-align:right;">

0.9120363

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

0.66

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

0.60

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

0.66

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

0.79

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

1

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

1

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

0.78

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

0.79

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

1

</td>

<td style="text-align:right;">

0.78

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

\[1\] “Kendalls W is: 0.833525252657699”

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

’0000500102110

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

’0064100400810

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

’0064101584110

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

’0210000030806

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

’0610100166512

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

’0912000029214

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

’0912100065110

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

’1108300002007

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

’1116100021513

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

’1117100054412

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

0

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

’1610100109713

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

’1610100144507

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

’1857200044606

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

’1857500010303

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

’2901000001406

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

0.3702401

</td>

<td style="text-align:right;">

2.03e-05

</td>

<td style="text-align:right;">

0.1980862

</td>

<td style="text-align:right;">

0.5266969

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(k)

</td>

<td style="text-align:right;">

0.5703195

</td>

<td style="text-align:right;">

2.03e-05

</td>

<td style="text-align:right;">

0.3580225

</td>

<td style="text-align:right;">

0.7152928

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(A,1)

</td>

<td style="text-align:right;">

0.4673558

</td>

<td style="text-align:right;">

1.00e-07

</td>

<td style="text-align:right;">

0.3009290

</td>

<td style="text-align:right;">

0.6102027

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(A,k)

</td>

<td style="text-align:right;">

0.6645369

</td>

<td style="text-align:right;">

1.00e-07

</td>

<td style="text-align:right;">

0.4922572

</td>

<td style="text-align:right;">

0.7796296

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(C,1)

</td>

<td style="text-align:right;">

0.4583216

</td>

<td style="text-align:right;">

1.00e-07

</td>

<td style="text-align:right;">

0.2951406

</td>

<td style="text-align:right;">

0.6006647

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(C,k)

</td>

<td style="text-align:right;">

0.6563883

</td>

<td style="text-align:right;">

1.00e-07

</td>

<td style="text-align:right;">

0.4859525

</td>

<td style="text-align:right;">

0.7725167

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

0.39

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

</tr>

<tr>

<td style="text-align:left;">

carmen

</td>

<td style="text-align:right;">

0.39

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

0.54

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

0.59

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

0.37

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

0.54

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

0.59

</td>

<td style="text-align:right;">

0.37

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

\[1\] “Kendalls W is: 0.690396846406345”

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

0.5

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

0.5

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

0.8127725

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.7339757

</td>

<td style="text-align:right;">

0.8714179

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(k)

</td>

<td style="text-align:right;">

0.9074146

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.8616700

</td>

<td style="text-align:right;">

0.9386527

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(A,1)

</td>

<td style="text-align:right;">

0.8372161

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.7598033

</td>

<td style="text-align:right;">

0.8911679

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(A,k)

</td>

<td style="text-align:right;">

0.9207076

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.8768956

</td>

<td style="text-align:right;">

0.9487582

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(C,1)

</td>

<td style="text-align:right;">

0.8241701

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.7491439

</td>

<td style="text-align:right;">

0.8796090

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(C,k)

</td>

<td style="text-align:right;">

0.9136629

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.8708385

</td>

<td style="text-align:right;">

0.9428417

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

1.0

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

0.7

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

edithimg

</td>

<td style="text-align:right;">

NA

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

NA

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

0.68

</td>

</tr>

<tr>

<td style="text-align:left;">

katia

</td>

<td style="text-align:right;">

0.7

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

1.0

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

1

</td>

<td style="text-align:right;">

0.68

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

\[1\] “Kendalls W is: 0.726698278281849”

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

NaN

</td>

<td style="text-align:right;">

0.5

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

0.5

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

0.9184438

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.8808236

</td>

<td style="text-align:right;">

0.9451194

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(k)

</td>

<td style="text-align:right;">

0.9621569

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.9434594

</td>

<td style="text-align:right;">

0.9749251

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(A,1)

</td>

<td style="text-align:right;">

0.9224615

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.8862027

</td>

<td style="text-align:right;">

0.9479793

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(A,k)

</td>

<td style="text-align:right;">

0.9641054

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.9461819

</td>

<td style="text-align:right;">

0.9762714

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(C,1)

</td>

<td style="text-align:right;">

0.9210370

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.8843927

</td>

<td style="text-align:right;">

0.9469347

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(C,k)

</td>

<td style="text-align:right;">

0.9634156

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.9452692

</td>

<td style="text-align:right;">

0.9757798

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

1.0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

0.9

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

1.0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

1.0

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

1

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

1

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

0.85

</td>

</tr>

<tr>

<td style="text-align:left;">

katia

</td>

<td style="text-align:right;">

0.9

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

1.0

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

1

</td>

<td style="text-align:right;">

0.85

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

\[1\] “Kendalls W is: 0.95776768192637”

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

’0000500102110

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

’0064100400810

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

’0064101584110

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

’0210000030806

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

’0610100166512

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

’0912000029214

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

’0912100065110

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

’1108300002007

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

’1116100021513

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

’1117100054412

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

0

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

’1610100109713

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

’1610100144507

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

’1857200044606

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

’1857500010303

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

’2901000001406

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

0.3702401

</td>

<td style="text-align:right;">

2.03e-05

</td>

<td style="text-align:right;">

0.1980862

</td>

<td style="text-align:right;">

0.5266969

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(k)

</td>

<td style="text-align:right;">

0.5703195

</td>

<td style="text-align:right;">

2.03e-05

</td>

<td style="text-align:right;">

0.3580225

</td>

<td style="text-align:right;">

0.7152928

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(A,1)

</td>

<td style="text-align:right;">

0.4673558

</td>

<td style="text-align:right;">

1.00e-07

</td>

<td style="text-align:right;">

0.3009290

</td>

<td style="text-align:right;">

0.6102027

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(A,k)

</td>

<td style="text-align:right;">

0.6645369

</td>

<td style="text-align:right;">

1.00e-07

</td>

<td style="text-align:right;">

0.4922572

</td>

<td style="text-align:right;">

0.7796296

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(C,1)

</td>

<td style="text-align:right;">

0.4583216

</td>

<td style="text-align:right;">

1.00e-07

</td>

<td style="text-align:right;">

0.2951406

</td>

<td style="text-align:right;">

0.6006647

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(C,k)

</td>

<td style="text-align:right;">

0.6563883

</td>

<td style="text-align:right;">

1.00e-07

</td>

<td style="text-align:right;">

0.4859525

</td>

<td style="text-align:right;">

0.7725167

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

0.39

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

</tr>

<tr>

<td style="text-align:left;">

carmen

</td>

<td style="text-align:right;">

0.39

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

0.54

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

0.59

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

0.37

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

0.54

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

0.59

</td>

<td style="text-align:right;">

0.37

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

\[1\] “Kendalls W is: 0.690396846406345”

\[1\] “Binary variable NA”

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

’0821000005107

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

’1117100054412

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

’2901000001406

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

’3098

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

0.1850492

</td>

<td style="text-align:right;">

0.0216876

</td>

<td style="text-align:right;">

0.0054744

</td>

<td style="text-align:right;">

0.3623394

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(k)

</td>

<td style="text-align:right;">

0.3389080

</td>

<td style="text-align:right;">

0.0216876

</td>

<td style="text-align:right;">

0.0122751

</td>

<td style="text-align:right;">

0.5619591

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(A,1)

</td>

<td style="text-align:right;">

0.2015621

</td>

<td style="text-align:right;">

0.0138764

</td>

<td style="text-align:right;">

0.0224875

</td>

<td style="text-align:right;">

0.3772330

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(A,k)

</td>

<td style="text-align:right;">

0.3630344

</td>

<td style="text-align:right;">

0.0138764

</td>

<td style="text-align:right;">

0.0499189

</td>

<td style="text-align:right;">

0.5774643

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(C,1)

</td>

<td style="text-align:right;">

0.2021920

</td>

<td style="text-align:right;">

0.0138764

</td>

<td style="text-align:right;">

0.0220527

</td>

<td style="text-align:right;">

0.3784664

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(C,k)

</td>

<td style="text-align:right;">

0.3639389

</td>

<td style="text-align:right;">

0.0138764

</td>

<td style="text-align:right;">

0.0484446

</td>

<td style="text-align:right;">

0.5789048

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

NA

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

0.32

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

edithimg

</td>

<td style="text-align:right;">

NA

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

1

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

0.32

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

</tbody>

</table>

\[1\] “Kendalls W is: 0.522199849423256”

\[1\] “Binary variable NA”

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

’0000900113814

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

1

</td>

<td style="text-align:right;">

1

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

’0002100030807

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

’0064100581414

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

’0413100058710

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

’0610100109913

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

’0637000025309

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

’0912100065110

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

’1117100054412

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

’1800100011015

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

’1857200044606

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

0.4563337

</td>

<td style="text-align:right;">

1e-07

</td>

<td style="text-align:right;">

0.2934454

</td>

<td style="text-align:right;">

0.5987658

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(k)

</td>

<td style="text-align:right;">

0.6545794

</td>

<td style="text-align:right;">

1e-07

</td>

<td style="text-align:right;">

0.4839137

</td>

<td style="text-align:right;">

0.7711236

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(A,1)

</td>

<td style="text-align:right;">

0.4795277

</td>

<td style="text-align:right;">

0e+00

</td>

<td style="text-align:right;">

0.3245046

</td>

<td style="text-align:right;">

0.6154512

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(A,k)

</td>

<td style="text-align:right;">

0.6753332

</td>

<td style="text-align:right;">

0e+00

</td>

<td style="text-align:right;">

0.5203002

</td>

<td style="text-align:right;">

0.7832324

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(C,1)

</td>

<td style="text-align:right;">

0.4916736

</td>

<td style="text-align:right;">

0e+00

</td>

<td style="text-align:right;">

0.3331949

</td>

<td style="text-align:right;">

0.6278545

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(C,k)

</td>

<td style="text-align:right;">

0.6859027

</td>

<td style="text-align:right;">

0e+00

</td>

<td style="text-align:right;">

0.5301064

</td>

<td style="text-align:right;">

0.7920563

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

NA

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0.63

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

edithimg

</td>

<td style="text-align:right;">

NA

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

1.00

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

1

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

katia

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

1

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

</tbody>

</table>

\[1\] “Kendalls W is: 0.607953729258254”

\[1\] “Binary variable NA”

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

’0000500047705

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

’0000500102110

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

’0000900113814

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

1

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

4

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

’0000900248810

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

’0001000146208

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

’0001300010014

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

’0064100581414

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

’0210000030806

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

’0410100003506

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

’0413100058710

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

’0610100016512

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

’1117100054412

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

’1857200027005

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

’1857500010303

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

’2010000003412

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

0.4092035

</td>

<td style="text-align:right;">

2.5e-06

</td>

<td style="text-align:right;">

0.2407614

</td>

<td style="text-align:right;">

0.5596381

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(k)

</td>

<td style="text-align:right;">

0.6099453

</td>

<td style="text-align:right;">

2.5e-06

</td>

<td style="text-align:right;">

0.4172268

</td>

<td style="text-align:right;">

0.7415490

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(A,1)

</td>

<td style="text-align:right;">

0.4688249

</td>

<td style="text-align:right;">

0.0e+00

</td>

<td style="text-align:right;">

0.3132025

</td>

<td style="text-align:right;">

0.6062426

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(A,k)

</td>

<td style="text-align:right;">

0.6658510

</td>

<td style="text-align:right;">

0.0e+00

</td>

<td style="text-align:right;">

0.5071134

</td>

<td style="text-align:right;">

0.7766402

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(C,1)

</td>

<td style="text-align:right;">

0.4838538

</td>

<td style="text-align:right;">

0.0e+00

</td>

<td style="text-align:right;">

0.3242176

</td>

<td style="text-align:right;">

0.6215135

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(C,k)

</td>

<td style="text-align:right;">

0.6791208

</td>

<td style="text-align:right;">

0.0e+00

</td>

<td style="text-align:right;">

0.5199606

</td>

<td style="text-align:right;">

0.7875665

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

0.69

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

1.00

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

carmen

</td>

<td style="text-align:right;">

0.69

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

0.22

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

1.00

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

0.83

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

\-1.00

</td>

<td style="text-align:right;">

0.53

</td>

</tr>

<tr>

<td style="text-align:left;">

katia

</td>

<td style="text-align:right;">

0.34

</td>

<td style="text-align:right;">

0.22

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

\-1.00

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

0.83

</td>

<td style="text-align:right;">

0.53

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

\[1\] “Kendalls W is: 0.697806201049”

\[1\] “Binary variable NA”

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

’0000700077705

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

’0064100581414

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

’0064101735111

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

’0610100166512

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

’1610100144507

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

0.3872709

</td>

<td style="text-align:right;">

8.4e-06

</td>

<td style="text-align:right;">

0.2166431

</td>

<td style="text-align:right;">

0.5411625

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(k)

</td>

<td style="text-align:right;">

0.5879611

</td>

<td style="text-align:right;">

8.4e-06

</td>

<td style="text-align:right;">

0.3843806

</td>

<td style="text-align:right;">

0.7269822

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(A,1)

</td>

<td style="text-align:right;">

0.4149717

</td>

<td style="text-align:right;">

2.3e-06

</td>

<td style="text-align:right;">

0.2456025

</td>

<td style="text-align:right;">

0.5652524

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(A,k)

</td>

<td style="text-align:right;">

0.6155948

</td>

<td style="text-align:right;">

2.3e-06

</td>

<td style="text-align:right;">

0.4236385

</td>

<td style="text-align:right;">

0.7458964

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(C,1)

</td>

<td style="text-align:right;">

0.4121268

</td>

<td style="text-align:right;">

2.3e-06

</td>

<td style="text-align:right;">

0.2434295

</td>

<td style="text-align:right;">

0.5623707

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(C,k)

</td>

<td style="text-align:right;">

0.6128152

</td>

<td style="text-align:right;">

2.3e-06

</td>

<td style="text-align:right;">

0.4207666

</td>

<td style="text-align:right;">

0.7436698

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

0.69

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

0.24

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

0.69

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

1.00

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

0.68

</td>

</tr>

<tr>

<td style="text-align:left;">

katia

</td>

<td style="text-align:right;">

0.24

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

0.68

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

\[1\] “Kendalls W is: 0.630543149618403”

\[1\] “Binary variable NA”

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

’0000900113814

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

0

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

’0000900185907

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

’0001100189506

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

’0064100400810

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

’0413100058710

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

0

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

’0610100109913

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

’0637000025309

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

’0908500003207

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

’0912000029214

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

’1800100011015

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

’2000100002903

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

’2106800006205

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

0.2986650

</td>

<td style="text-align:right;">

0.0005172

</td>

<td style="text-align:right;">

0.1216916

</td>

<td style="text-align:right;">

0.4647362

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(k)

</td>

<td style="text-align:right;">

0.4901708

</td>

<td style="text-align:right;">

0.0005172

</td>

<td style="text-align:right;">

0.2382739

</td>

<td style="text-align:right;">

0.6621861

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(A,1)

</td>

<td style="text-align:right;">

0.4038413

</td>

<td style="text-align:right;">

0.0000053

</td>

<td style="text-align:right;">

0.2308044

</td>

<td style="text-align:right;">

0.5571934

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(A,k)

</td>

<td style="text-align:right;">

0.6046448

</td>

<td style="text-align:right;">

0.0000053

</td>

<td style="text-align:right;">

0.4033976

</td>

<td style="text-align:right;">

0.7397745

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(C,1)

</td>

<td style="text-align:right;">

0.3971779

</td>

<td style="text-align:right;">

0.0000053

</td>

<td style="text-align:right;">

0.2269377

</td>

<td style="text-align:right;">

0.5498176

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(C,k)

</td>

<td style="text-align:right;">

0.5979916

</td>

<td style="text-align:right;">

0.0000053

</td>

<td style="text-align:right;">

0.3985903

</td>

<td style="text-align:right;">

0.7338560

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

NA

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

0.35

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

0.48

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

\-0.07

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

1

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

katia

</td>

<td style="text-align:right;">

0.35

</td>

<td style="text-align:right;">

0.48

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

1

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

\-0.07

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

\[1\] “Kendalls W is: 0.571030216471417”

\[1\] “Binary variable NA”

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

’0064100940708

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

0.5

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’0210000140808

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

NaN

</td>

<td style="text-align:right;">

0.5

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

0.5

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’0944800002908

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

0.5

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

0.6250438

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.4921918

</td>

<td style="text-align:right;">

0.7327166

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(k)

</td>

<td style="text-align:right;">

0.7900711

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.6863492

</td>

<td style="text-align:right;">

0.8609007

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(A,1)

</td>

<td style="text-align:right;">

0.6345942

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.5025852

</td>

<td style="text-align:right;">

0.7405889

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(A,k)

</td>

<td style="text-align:right;">

0.7967847

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.6951954

</td>

<td style="text-align:right;">

0.8656995

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(C,1)

</td>

<td style="text-align:right;">

0.6307918

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.4987916

</td>

<td style="text-align:right;">

0.7373112

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(C,k)

</td>

<td style="text-align:right;">

0.7941225

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.6920047

</td>

<td style="text-align:right;">

0.8637017

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

NA

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

NA

</td>

</tr>

<tr>

<td style="text-align:left;">

carmen

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

\-0.07

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

1

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

0.86

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

\-0.07

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

\[1\] “Kendalls W is: 0.651409557495314”

\[1\] “Binary variable NA”

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

’0000500058507

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

’1857200027005

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

’2901000001406

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

0.2957315

</td>

<td style="text-align:right;">

0.0005814

</td>

<td style="text-align:right;">

0.1186145

</td>

<td style="text-align:right;">

0.4621558

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(k)

</td>

<td style="text-align:right;">

0.4866617

</td>

<td style="text-align:right;">

0.0005814

</td>

<td style="text-align:right;">

0.2330309

</td>

<td style="text-align:right;">

0.6598610

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(A,1)

</td>

<td style="text-align:right;">

0.6576870

</td>

<td style="text-align:right;">

0.0000000

</td>

<td style="text-align:right;">

0.3481085

</td>

<td style="text-align:right;">

0.8058828

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(A,k)

</td>

<td style="text-align:right;">

0.8126535

</td>

<td style="text-align:right;">

0.0000000

</td>

<td style="text-align:right;">

0.4855064

</td>

<td style="text-align:right;">

0.9083880

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(C,1)

</td>

<td style="text-align:right;">

0.5554793

</td>

<td style="text-align:right;">

0.0000000

</td>

<td style="text-align:right;">

0.4077373

</td>

<td style="text-align:right;">

0.6788330

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(C,k)

</td>

<td style="text-align:right;">

0.7383048

</td>

<td style="text-align:right;">

0.0000000

</td>

<td style="text-align:right;">

0.6085007

</td>

<td style="text-align:right;">

0.8267484

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

NA

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

NA

</td>

</tr>

<tr>

<td style="text-align:left;">

carmen

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

edithimg

</td>

<td style="text-align:right;">

NA

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

NA

</td>

<td style="text-align:right;">

1

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

1

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

\-0.03

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

1

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

1

</td>

</tr>

</tbody>

</table>

\[1\] “Kendalls W is: 0.50366477969447”

\[1\] “Binary variable NA”

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

’0637000025309

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

’0908500003207

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

’1816400033115

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

0.0300728

</td>

<td style="text-align:right;">

0.3684254

</td>

<td style="text-align:right;">

\-0.1438007

</td>

<td style="text-align:right;">

0.2140429

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(k)

</td>

<td style="text-align:right;">

0.0654206

</td>

<td style="text-align:right;">

0.3684254

</td>

<td style="text-align:right;">

\-0.3963373

</td>

<td style="text-align:right;">

0.3807457

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(A,1)

</td>

<td style="text-align:right;">

0.2425694

</td>

<td style="text-align:right;">

0.0067380

</td>

<td style="text-align:right;">

0.0465557

</td>

<td style="text-align:right;">

0.4246141

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(A,k)

</td>

<td style="text-align:right;">

0.4196276

</td>

<td style="text-align:right;">

0.0067380

</td>

<td style="text-align:right;">

0.0930570

</td>

<td style="text-align:right;">

0.6265340

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(C,1)

</td>

<td style="text-align:right;">

0.2268277

</td>

<td style="text-align:right;">

0.0067380

</td>

<td style="text-align:right;">

0.0469351

</td>

<td style="text-align:right;">

0.4009285

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(C,k)

</td>

<td style="text-align:right;">

0.3984399

</td>

<td style="text-align:right;">

0.0067380

</td>

<td style="text-align:right;">

0.1000584

</td>

<td style="text-align:right;">

0.6017456

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

NA

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

\-0.02

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

NA

</td>

<td style="text-align:right;">

1

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

1

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

\-0.02

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

NA

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

1

</td>

</tr>

</tbody>

</table>

\[1\] “Kendalls W is: 0.43810261707989”

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

Subjects = 85 Raters = 6 alpha = 0.637

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

’0000500058507

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

’0000900113814

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

’0000900156715

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

’0064100400810

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

</tbody>

</table>

Krippendorff’s alpha

Subjects = 85 Raters = 6 alpha = 0.439

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

’0064100940708

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;">

’0610100016512

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

’0918900000214

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;">

’1114100000412

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;">

’1132100010608

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;">

’1219700039614

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;">

’1816400033115

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

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’0000900156715

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

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’0001400025309

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’0210000140808

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’0413000000715

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

’1109000003915

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

’2000100002903

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’2106800006205

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

’0002100030807

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’0410100003506

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’1610100109713

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’1857200027005

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’1857600018305

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’2010000003412

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

’0000400209014

</td>

<td style="text-align:right;">

4

</td>

</tr>

<tr>

<td style="text-align:left;">

’0001100171309

</td>

<td style="text-align:right;">

4

</td>

</tr>

<tr>

<td style="text-align:left;">

’0064103230113

</td>

<td style="text-align:right;">

4

</td>

</tr>

<tr>

<td style="text-align:left;">

’0610100092815

</td>

<td style="text-align:right;">

4

</td>

</tr>

<tr>

<td style="text-align:left;">

’0636300016008

</td>

<td style="text-align:right;">

4

</td>

</tr>

<tr>

<td style="text-align:left;">

’1026500058813

</td>

<td style="text-align:right;">

4

</td>

</tr>

<tr>

<td style="text-align:left;">

’1800100011015

</td>

<td style="text-align:right;">

4

</td>

</tr>

<tr>

<td style="text-align:left;">

’1857200044606

</td>

<td style="text-align:right;">

4

</td>

</tr>

<tr>

<td style="text-align:left;">

’0000500047705

</td>

<td style="text-align:right;">

5

</td>

</tr>

<tr>

<td style="text-align:left;">

’0000500058507

</td>

<td style="text-align:right;">

5

</td>

</tr>

<tr>

<td style="text-align:left;">

’0000900005113

</td>

<td style="text-align:right;">

5

</td>

</tr>

<tr>

<td style="text-align:left;">

’0001700203910

</td>

<td style="text-align:right;">

5

</td>

</tr>

<tr>

<td style="text-align:left;">

’0064101735111

</td>

<td style="text-align:right;">

5

</td>

</tr>

<tr>

<td style="text-align:left;">

’0064101996410

</td>

<td style="text-align:right;">

5

</td>

</tr>

<tr>

<td style="text-align:left;">

’0821000005107

</td>

<td style="text-align:right;">

5

</td>

</tr>

<tr>

<td style="text-align:left;">

’1613100056411

</td>

<td style="text-align:right;">

5

</td>

</tr>

<tr>

<td style="text-align:left;">

’2135500006106

</td>

<td style="text-align:right;">

5

</td>

</tr>

<tr>

<td style="text-align:left;">

’3098

</td>

<td style="text-align:right;">

5

</td>

</tr>

<tr>

<td style="text-align:left;">

’0001000087410

</td>

<td style="text-align:right;">

6

</td>

</tr>

<tr>

<td style="text-align:left;">

’0001100189506

</td>

<td style="text-align:right;">

6

</td>

</tr>

<tr>

<td style="text-align:left;">

’0001300010014

</td>

<td style="text-align:right;">

6

</td>

</tr>

<tr>

<td style="text-align:left;">

’0944800002908

</td>

<td style="text-align:right;">

6

</td>

</tr>

<tr>

<td style="text-align:left;">

’2901000001406

</td>

<td style="text-align:right;">

6

</td>

</tr>

<tr>

<td style="text-align:left;">

’0001300041011

</td>

<td style="text-align:right;">

7

</td>

</tr>

<tr>

<td style="text-align:left;">

’0001600284910

</td>

<td style="text-align:right;">

7

</td>

</tr>

<tr>

<td style="text-align:left;">

’0002000005615

</td>

<td style="text-align:right;">

7

</td>

</tr>

<tr>

<td style="text-align:left;">

’0064100581414

</td>

<td style="text-align:right;">

7

</td>

</tr>

<tr>

<td style="text-align:left;">

’0411100072013

</td>

<td style="text-align:right;">

7

</td>

</tr>

<tr>

<td style="text-align:left;">

’0416000007815

</td>

<td style="text-align:right;">

7

</td>

</tr>

<tr>

<td style="text-align:left;">

’0610100109913

</td>

<td style="text-align:right;">

7

</td>

</tr>

<tr>

<td style="text-align:left;">

’0917900001507

</td>

<td style="text-align:right;">

7

</td>

</tr>

<tr>

<td style="text-align:left;">

’1111200025211

</td>

<td style="text-align:right;">

7

</td>

</tr>

<tr>

<td style="text-align:left;">

’1111500045614

</td>

<td style="text-align:right;">

7

</td>

</tr>

<tr>

<td style="text-align:left;">

’1117100054412

</td>

<td style="text-align:right;">

7

</td>

</tr>

<tr>

<td style="text-align:left;">

’1511100067610

</td>

<td style="text-align:right;">

7

</td>

</tr>

<tr>

<td style="text-align:left;">

’0000900113814

</td>

<td style="text-align:right;">

8

</td>

</tr>

<tr>

<td style="text-align:left;">

’0001200073410

</td>

<td style="text-align:right;">

8

</td>

</tr>

<tr>

<td style="text-align:left;">

’0908500003207

</td>

<td style="text-align:right;">

8

</td>

</tr>

<tr>

<td style="text-align:left;">

’0912100065110

</td>

<td style="text-align:right;">

8

</td>

</tr>

<tr>

<td style="text-align:left;">

’1108300002007

</td>

<td style="text-align:right;">

8

</td>

</tr>

<tr>

<td style="text-align:left;">

’1857500010303

</td>

<td style="text-align:right;">

8

</td>

</tr>

<tr>

<td style="text-align:left;">

’0001000146208

</td>

<td style="text-align:right;">

9

</td>

</tr>

<tr>

<td style="text-align:left;">

’0064101584110

</td>

<td style="text-align:right;">

9

</td>

</tr>

<tr>

<td style="text-align:left;">

’0413100058710

</td>

<td style="text-align:right;">

9

</td>

</tr>

<tr>

<td style="text-align:left;">

’0637000025309

</td>

<td style="text-align:right;">

9

</td>

</tr>

<tr>

<td style="text-align:left;">

’1610100144507

</td>

<td style="text-align:right;">

9

</td>

</tr>

<tr>

<td style="text-align:left;">

’0000900248810

</td>

<td style="text-align:right;">

10

</td>

</tr>

<tr>

<td style="text-align:left;">

’0064100400810

</td>

<td style="text-align:right;">

10

</td>

</tr>

<tr>

<td style="text-align:left;">

’0000500102110

</td>

<td style="text-align:right;">

11

</td>

</tr>

<tr>

<td style="text-align:left;">

’0000900185907

</td>

<td style="text-align:right;">

11

</td>

</tr>

<tr>

<td style="text-align:left;">

’0610100166512

</td>

<td style="text-align:right;">

12

</td>

</tr>

<tr>

<td style="text-align:left;">

’1615100031912

</td>

<td style="text-align:right;">

12

</td>

</tr>

<tr>

<td style="text-align:left;">

’0912000029214

</td>

<td style="text-align:right;">

13

</td>

</tr>

<tr>

<td style="text-align:left;">

’1116100021513

</td>

<td style="text-align:right;">

13

</td>

</tr>

<tr>

<td style="text-align:left;">

’0210000015405

</td>

<td style="text-align:right;">

16

</td>

</tr>

<tr>

<td style="text-align:left;">

’0210000030806

</td>

<td style="text-align:right;">

17

</td>

</tr>

</tbody>

</table>
