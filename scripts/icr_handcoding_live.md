Examine Pilot Data
================
Aaron Erlich
03/06/2019

## Inter Coder Reliability

I need to get the same 10 which were coded by everyone. As far as I can
tell, it appears that only **five** of the coders coded these. I believe
Dan used a different system.

``` r
ROUND <- "full_pilot1"
pacman::p_load(tidyverse, tidyselect, irr, kableExtra, irrNA)

icr <- read_csv("./data_raw/Big Data y Acceso a Info en México_June 18, 2019_07.36.csv",)
fs_ps1 <- readRDS("./data_raw/full_sample_post_p1.rds") %>%
  filter(round == ROUND)

#remove gardage rows
icr2 <- icr %>% slice(-(1:2)) %>%
  filter(!(is.na(RecipientEmail)), Progress==100)

#table(icr)

#deal with person who coded from a differet location (uggh)


icr2 <- mutate(icr, folio_id = ifelse(grepl("'", S1),
                                    trimws(S1),
                                    trimws(str_c("'", S1))))

icr2 <- mutate(icr2, coder_id = str_extract(RecipientEmail, "^[[:alnum:]]+"))


 to_eval <- filter(icr2, folio_id %in% fs_ps1$FOLIO)

#get rid of Dan???

binary_vars <- vars_select(names(to_eval), matches("S6_|S10_"))

to_eval<- to_eval %>% mutate_at(vars(binary_vars),
                                list(~dplyr::recode(., `Sí` = 1, `No` = 0, .default = NA_real_)))

# to_eval <- mutate(icr2, Q14 = factor(Q14, levels = c("Poca o nada", 
#                                                   "Menos de la mitad", 
#                                                   "Aproximadamente la mitad",
#                                                   "La mayoría",
#                                                   "Toda")))

#table(icr$unique_coder)

#output where a second coder is missing ----
number_times_coded <- to_eval %>%
  group_by(folio_id) %>%
  summarise(count = n())


n_coder <- filter(number_times_coded, count > 1)
one_coder <-  filter(number_times_coded, count == 1)
```

These have only bee coded once

``` r
kable(cat(one_coder$folio_id))
```

    ## '0000400020312 '0000400124810 '0000400155810 '0000400317213 '0000500062605 '0000600021806 '0000600035210 '0000600149312 '0000600199813 '0000600286311 '0000700001309 '0000700003014 '0000700012515 '0000700013215 '0000700069107 '0000700100909 '0000700207914 '0000800073014 '0000800101312 '0000900030811 '0000900060011 '0000900130914 '0000900247913 '0000900248713 '0001000162110 '0001100086412 '0001100171606 '0001100183308 '0001100246213 '0001100289115 '0001100373615 '0001100395711 '0001100570007 '0001200061814 '0001200067405 '0001200121815 '0001200150109 '0001200202407 '0001200265707 '0001200313314 '0001300000410 '0001300002110 '0001400043706 '0001700020906 '0001700027708 '0001700078810 '0001700156608 '0001700198605 '0002000100913 '0002000120512 '0002100021114 '0002200038205 '0002200056808 '0002200158108 '0002700040515 '0002700053513 '0002700099113 '0002700122913 '0002700185215 '0063500015815 '0063700025011 '0063700030110 '0063700057807 '0063700238212 '0063700261615 '0063700381813 '0064100060013 '0064100332409 '0064100522913 '0064100566114 '0064100740610 '0064100818910 '0064101519911 '0064101676309 '0064102219210 '0064102522611 '0064102525911 '0064103177313 '0210000035314 '0210000132008 '0411100009107 '0413100024507 '0441000013414 '0610100023908 '0610100064610 '0610100066912 '0612100019614 '0632000013606 '0673800045808 '0673800046914 '0673800119808 '0673800138707 '0674700008307 '0678000017911 '0681200028512 '0681200032313 '0681200050914 '0814000011913 '0912100016013 '0912100016603 '0912100031114 '0912100034910 '0918600000510 '0933800014509 '1026500013908 '1110000000112 '1111200003612 '1111200007709 '1111200025508 '1111500016413 '1111500024807 '1111500050314 '1113100032713 '1114100042113 '1114100052713 '1119500000306 '1132100013412 '1215100061809 '1215100127013 '1219700011408 '1220000000309 '1222300007013 '1222600012909 '1224500002815 '1232900010213 '1407500014609 '1411100030410 '1412000005508 '1507500005612 '1511100015108 '1511100022411 '1610100013408 '1610100027310 '1610100032814 '1610100091713 '1613100000607 '1613100039512 '1613100054314 '1615100020609 '17765 '1810000004907 '1816400010805 '1816400083714 '1816400107212 '1816400134811 '1819100004013 '1850000028006 '1857200172714 '1857500022812 '1857500071010 '1857600005803 '1857600021710 '1857700015910 '19213 '2014300001807 '2015000001314 '2015000003612 '2031200003314 '2099900005613 '2099900021410 '2135500004809 '2510100035511 '2510100061415 '252 '26625 '27705 '27869 '2901000010314 '29725 '3670000008013 '4220700001412 '6184 '7394

<table>

<tbody>

<tr>

</tr>

</tbody>

</table>

``` r
to_eval <- filter(to_eval, folio_id %in% n_coder$folio_id)
coder_ids <- unique(to_eval$coder_id)

for(i in 1:length(binary_vars)) {
  data_wide <- reshape2::dcast(to_eval,  folio_id ~ coder_id , value.var= binary_vars[i], mean)
  #data_wide <- data_wide %>% select(-folio_id) %>%
  data_wide$rater_var <- apply(data_wide[, coder_ids], 1, var, na.rm = TRUE)
  
 print( sprintf("Binary variable %s", binary_vars[i]))
  
  ratings_out <- data_wide %>%
    filter(rater_var > 0 | is.na(rater_var))

#capture the number of coders, get observations with at least 2. 
#output where there is disagreement only
 output <-  ratings_out %>% kable(., escape = TRUE, digit = 2, )
print(output)

to_stat <- select(data_wide, -folio_id, -rater_var)
print(kable(iccNA(to_stat)$ICCs))

cat("\n\n")


kendallW <- kendallNA(to_stat)
print(sprintf("Kendalls W is: %s", kendallW$`Kendall's W`))
#iprint(kripp.alpha(t(to_stat), method = "nominal"))
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

</tr>

<tr>

<td style="text-align:left;">

’0001300003707

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

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

NA

</td>

</tr>

<tr>

<td style="text-align:left;">

’0064101095409

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

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

NA

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

0.1769131

</td>

<td style="text-align:right;">

0.0273732

</td>

<td style="text-align:right;">

\-0.0035828

</td>

<td style="text-align:right;">

0.3546852

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(k)

</td>

<td style="text-align:right;">

0.3238732

</td>

<td style="text-align:right;">

0.0273732

</td>

<td style="text-align:right;">

\-0.0080200

</td>

<td style="text-align:right;">

0.5505435

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(A,1)

</td>

<td style="text-align:right;">

0.1946427

</td>

<td style="text-align:right;">

0.0172987

</td>

<td style="text-align:right;">

0.0145500

</td>

<td style="text-align:right;">

0.3707755

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(A,k)

</td>

<td style="text-align:right;">

0.3500668

</td>

<td style="text-align:right;">

0.0172987

</td>

<td style="text-align:right;">

0.0323811

</td>

<td style="text-align:right;">

0.5675466

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(C,1)

</td>

<td style="text-align:right;">

0.1951577

</td>

<td style="text-align:right;">

0.0172987

</td>

<td style="text-align:right;">

0.0141130

</td>

<td style="text-align:right;">

0.3718345

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(C,k)

</td>

<td style="text-align:right;">

0.3508139

</td>

<td style="text-align:right;">

0.0172987

</td>

<td style="text-align:right;">

0.0309161

</td>

<td style="text-align:right;">

0.5688155

</td>

</tr>

</tbody>

</table>

\[1\] “Kendalls W is: 0.527125624171469”

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

</tr>

</thead>

<tbody>

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

0.5

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0.12

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

1.0

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0.25

</td>

</tr>

<tr>

<td style="text-align:left;">

’0001300003707

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0.0

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NA

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

0.0

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0.50

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

1.0

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0.33

</td>

</tr>

<tr>

<td style="text-align:left;">

’0064101095409

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

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

NA

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

0.0

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0.50

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

0.0

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0.50

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

0.0

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0.50

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

0.5

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0.12

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

0.0

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0.50

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

0.3935193

</td>

<td style="text-align:right;">

6.4e-06

</td>

<td style="text-align:right;">

0.2232887

</td>

<td style="text-align:right;">

0.5459465

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(k)

</td>

<td style="text-align:right;">

0.5911766

</td>

<td style="text-align:right;">

6.4e-06

</td>

<td style="text-align:right;">

0.3904958

</td>

<td style="text-align:right;">

0.7282339

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(A,1)

</td>

<td style="text-align:right;">

0.4214261

</td>

<td style="text-align:right;">

7.0e-07

</td>

<td style="text-align:right;">

0.2595997

</td>

<td style="text-align:right;">

0.5665636

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(A,k)

</td>

<td style="text-align:right;">

0.6187988

</td>

<td style="text-align:right;">

7.0e-07

</td>

<td style="text-align:right;">

0.4387315

</td>

<td style="text-align:right;">

0.7444208

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(C,1)

</td>

<td style="text-align:right;">

0.4333572

</td>

<td style="text-align:right;">

7.0e-07

</td>

<td style="text-align:right;">

0.2669711

</td>

<td style="text-align:right;">

0.5795329

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(C,k)

</td>

<td style="text-align:right;">

0.6302310

</td>

<td style="text-align:right;">

7.0e-07

</td>

<td style="text-align:right;">

0.4480209

</td>

<td style="text-align:right;">

0.7544022

</td>

</tr>

</tbody>

</table>

\[1\] “Kendalls W is: 0.610485350984833”

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

1.00

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0.33

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

1.00

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0.33

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

0.00

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0.33

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

1.00

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0.33

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

0.33

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0.22

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

1.00

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0.50

</td>

</tr>

<tr>

<td style="text-align:left;">

’0001300003707

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

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

NaN

</td>

<td style="text-align:right;">

NA

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

1.00

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0.50

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

1.00

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0.33

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

1.00

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0.33

</td>

</tr>

<tr>

<td style="text-align:left;">

’0064101095409

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

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

NA

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

1.00

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0.50

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

1.00

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0.33

</td>

</tr>

<tr>

<td style="text-align:left;">

’0210000140808

</td>

<td style="text-align:right;">

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

NaN

</td>

<td style="text-align:right;">

0.06

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

1.00

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0.33

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

0.50

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0.12

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

0.50

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0.12

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

1.00

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0.33

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

1.00

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0.50

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

1.00

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0.33

</td>

</tr>

<tr>

<td style="text-align:left;">

’1112500008310

</td>

<td style="text-align:right;">

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

0.50

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0.12

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

0.50

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0.12

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

0.50

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0.12

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

1.00

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0.33

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

1.00

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0.50

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

0.50

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0.12

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

1.00

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0.50

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

0.50

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0.12

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

0.3835837

</td>

<td style="text-align:right;">

1.09e-05

</td>

<td style="text-align:right;">

0.2123835

</td>

<td style="text-align:right;">

0.5375517

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(k)

</td>

<td style="text-align:right;">

0.5810317

</td>

<td style="text-align:right;">

1.09e-05

</td>

<td style="text-align:right;">

0.3753710

</td>

<td style="text-align:right;">

0.7214900

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(A,1)

</td>

<td style="text-align:right;">

0.4631632

</td>

<td style="text-align:right;">

0.00e+00

</td>

<td style="text-align:right;">

0.3084149

</td>

<td style="text-align:right;">

0.6002082

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(A,k)

</td>

<td style="text-align:right;">

0.6578573

</td>

<td style="text-align:right;">

0.00e+00

</td>

<td style="text-align:right;">

0.4975599

</td>

<td style="text-align:right;">

0.7701734

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(C,1)

</td>

<td style="text-align:right;">

0.4854814

</td>

<td style="text-align:right;">

0.00e+00

</td>

<td style="text-align:right;">

0.3261317

</td>

<td style="text-align:right;">

0.6222826

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(C,k)

</td>

<td style="text-align:right;">

0.6777136

</td>

<td style="text-align:right;">

0.00e+00

</td>

<td style="text-align:right;">

0.5189013

</td>

<td style="text-align:right;">

0.7859398

</td>

</tr>

</tbody>

</table>

\[1\] “Kendalls W is: 0.698502760734474”

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

</tr>

<tr>

<td style="text-align:left;">

’0001300003707

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

1.0

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NA

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

0.0

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0.50

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

0.0

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0.33

</td>

</tr>

<tr>

<td style="text-align:left;">

’0064101095409

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

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

NA

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

0.0

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0.50

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

0.5

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0.12

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

0.0

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0.33

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

1.0

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0.50

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

0.0

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0.50

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

0.4592315

</td>

<td style="text-align:right;">

1e-07

</td>

<td style="text-align:right;">

0.2967021

</td>

<td style="text-align:right;">

0.6006061

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(k)

</td>

<td style="text-align:right;">

0.6542871

</td>

<td style="text-align:right;">

1e-07

</td>

<td style="text-align:right;">

0.4845855

</td>

<td style="text-align:right;">

0.7701867

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(A,1)

</td>

<td style="text-align:right;">

0.4863226

</td>

<td style="text-align:right;">

0e+00

</td>

<td style="text-align:right;">

0.3256677

</td>

<td style="text-align:right;">

0.6236462

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(A,k)

</td>

<td style="text-align:right;">

0.6784486

</td>

<td style="text-align:right;">

0e+00

</td>

<td style="text-align:right;">

0.5182913

</td>

<td style="text-align:right;">

0.7869390

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(C,1)

</td>

<td style="text-align:right;">

0.4822255

</td>

<td style="text-align:right;">

0e+00

</td>

<td style="text-align:right;">

0.3223933

</td>

<td style="text-align:right;">

0.6196390

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(C,k)

</td>

<td style="text-align:right;">

0.6748594

</td>

<td style="text-align:right;">

0e+00

</td>

<td style="text-align:right;">

0.5146408

</td>

<td style="text-align:right;">

0.7840441

</td>

</tr>

</tbody>

</table>

\[1\] “Kendalls W is: 0.678950854278712”

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

0.00

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0.25

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

0.00

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0.33

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

1.00

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0.33

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

0.67

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0.06

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

0.00

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0.33

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

0.00

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0.33

</td>

</tr>

<tr>

<td style="text-align:left;">

’0001300003707

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

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

NaN

</td>

<td style="text-align:right;">

NA

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

1.00

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0.50

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

0.00

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0.50

</td>

</tr>

<tr>

<td style="text-align:left;">

’0064101095409

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

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

NA

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

0.50

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0.12

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

0.00

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0.50

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

0.00

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0.50

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

0.00

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0.50

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

0.00

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0.50

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

0.50

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0.12

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

0.50

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0.12

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

0.3251485

</td>

<td style="text-align:right;">

0.0001804

</td>

<td style="text-align:right;">

0.1492487

</td>

<td style="text-align:right;">

0.4874654

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(k)

</td>

<td style="text-align:right;">

0.5177835

</td>

<td style="text-align:right;">

0.0001804

</td>

<td style="text-align:right;">

0.2810758

</td>

<td style="text-align:right;">

0.6794457

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(A,1)

</td>

<td style="text-align:right;">

0.4256084

</td>

<td style="text-align:right;">

0.0000018

</td>

<td style="text-align:right;">

0.2539932

</td>

<td style="text-align:right;">

0.5752409

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(A,k)

</td>

<td style="text-align:right;">

0.6228313

</td>

<td style="text-align:right;">

0.0000018

</td>

<td style="text-align:right;">

0.4307998

</td>

<td style="text-align:right;">

0.7513066

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(C,1)

</td>

<td style="text-align:right;">

0.4174774

</td>

<td style="text-align:right;">

0.0000018

</td>

<td style="text-align:right;">

0.2492351

</td>

<td style="text-align:right;">

0.5663252

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(C,k)

</td>

<td style="text-align:right;">

0.6149664

</td>

<td style="text-align:right;">

0.0000018

</td>

<td style="text-align:right;">

0.4252345

</td>

<td style="text-align:right;">

0.7442636

</td>

</tr>

</tbody>

</table>

\[1\] “Kendalls W is: 0.579546964886993”

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

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

’0001300003707

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NA

</td>

</tr>

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

0.50

</td>

</tr>

<tr>

<td style="text-align:left;">

’0064101095409

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

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

NA

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

0.33

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0.22

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

0.6549829

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.5295473

</td>

<td style="text-align:right;">

0.7550285

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(k)

</td>

<td style="text-align:right;">

0.8088245

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.7149813

</td>

<td style="text-align:right;">

0.8729157

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(A,1)

</td>

<td style="text-align:right;">

0.6650627

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.5406825

</td>

<td style="text-align:right;">

0.7632483

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(A,k)

</td>

<td style="text-align:right;">

0.8156746

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.7239703

</td>

<td style="text-align:right;">

0.8778330

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(C,1)

</td>

<td style="text-align:right;">

0.6609423

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.5365527

</td>

<td style="text-align:right;">

0.7597183

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(C,k)

</td>

<td style="text-align:right;">

0.8128857

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.7206819

</td>

<td style="text-align:right;">

0.8757201

</td>

</tr>

</tbody>

</table>

\[1\] “Kendalls W is: 0.659027630737807”

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

</tr>

<tr>

<td style="text-align:left;">

’0001300003707

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

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

NA

</td>

</tr>

<tr>

<td style="text-align:left;">

’0064101095409

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

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

NA

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

0.2885683

</td>

<td style="text-align:right;">

0.0008079

</td>

<td style="text-align:right;">

0.1105787

</td>

<td style="text-align:right;">

0.4554776

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(k)

</td>

<td style="text-align:right;">

0.4747782

</td>

<td style="text-align:right;">

0.0008079

</td>

<td style="text-align:right;">

0.2169603

</td>

<td style="text-align:right;">

0.6508578

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(A,1)

</td>

<td style="text-align:right;">

0.6529212

</td>

<td style="text-align:right;">

0.0000000

</td>

<td style="text-align:right;">

0.3482969

</td>

<td style="text-align:right;">

0.8012220

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(A,k)

</td>

<td style="text-align:right;">

0.8074117

</td>

<td style="text-align:right;">

0.0000000

</td>

<td style="text-align:right;">

0.4844935

</td>

<td style="text-align:right;">

0.9047205

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(C,1)

</td>

<td style="text-align:right;">

0.5522340

</td>

<td style="text-align:right;">

0.0000000

</td>

<td style="text-align:right;">

0.4040769

</td>

<td style="text-align:right;">

0.6757162

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(C,k)

</td>

<td style="text-align:right;">

0.7332306

</td>

<td style="text-align:right;">

0.0000000

</td>

<td style="text-align:right;">

0.6017754

</td>

<td style="text-align:right;">

0.8228138

</td>

</tr>

</tbody>

</table>

\[1\] “Kendalls W is: 0.508781637635764”

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

1.0

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0.33

</td>

</tr>

<tr>

<td style="text-align:left;">

’0001300003707

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0.5

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

NA

</td>

</tr>

<tr>

<td style="text-align:left;">

’0064101095409

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

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

NA

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

0.0

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0.50

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

1.0

</td>

<td style="text-align:right;">

NaN

</td>

<td style="text-align:right;">

0.50

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

0.0462598

</td>

<td style="text-align:right;">

0.3056858

</td>

<td style="text-align:right;">

\-0.1302064

</td>

<td style="text-align:right;">

0.2303309

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(k)

</td>

<td style="text-align:right;">

0.0975504

</td>

<td style="text-align:right;">

0.3056858

</td>

<td style="text-align:right;">

\-0.3454388

</td>

<td style="text-align:right;">

0.4000950

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(A,1)

</td>

<td style="text-align:right;">

0.2529987

</td>

<td style="text-align:right;">

0.0050481

</td>

<td style="text-align:right;">

0.0569608

</td>

<td style="text-align:right;">

0.4333395

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(A,k)

</td>

<td style="text-align:right;">

0.4301331

</td>

<td style="text-align:right;">

0.0050481

</td>

<td style="text-align:right;">

0.1127894

</td>

<td style="text-align:right;">

0.6317483

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(C,1)

</td>

<td style="text-align:right;">

0.2371663

</td>

<td style="text-align:right;">

0.0050481

</td>

<td style="text-align:right;">

0.0567380

</td>

<td style="text-align:right;">

0.4100131

</td>

</tr>

<tr>

<td style="text-align:left;">

ICC(C,k)

</td>

<td style="text-align:right;">

0.4092893

</td>

<td style="text-align:right;">

0.0050481

</td>

<td style="text-align:right;">

0.1182064

</td>

<td style="text-align:right;">

0.6076544

</td>

</tr>

</tbody>

</table>

\[1\] “Kendalls W is: 0.443895373604839”

Okay, now let’s look at agreement. At least for these c coders, it lokes
like, mainly for *Toda* folks are in agreement. For the middle
categories, it is challenging. If we condensed the categories it would
look better. I won’t do more until I figure how who the unique coders
are

``` r
# ratings <- select(data_wide, folio_id, contains("Person")) 
# 
# ratings %>% kable(., escape = TRUE)
# 
# kripp.alpha(t(select(ratings, contains("Person"))), method = "ordinal")

# %>%
#   column_spec(1:5, width = "10em")
```

It’s hard to get agreement out of all the coders of the 8, where we have
all observations we get 3 thare the same across the board.

``` r
# agree(select(ratings, contains("@")))
```

This is not a sufficient Kripp alpha, but it is getting there.

``` r
# kripp.alpha(t(select(ratings, contains("Person"))), method = "ordinal")
```
