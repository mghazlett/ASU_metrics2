*******************************************************************************
* Table 1 - Populist Government Episodes, 1900-2020
* Generates tables/Table1.tex via file write
* Part of replication for Funke, Schularick, and Trebesch (2023, AER)
*******************************************************************************

capture {

file open t1 using "tables/Table1.tex", write replace text

* ---- Preamble / Panel A header ----
file write t1 `"\def\sym#1{\ifmmode^{#1}\else\(\^{#1}\)\fi}"' _n
file write t1 `"\setlength{\tabcolsep}{5pt}"' _n
file write t1 `"{\footnotesize"' _n
file write t1 `"\begin{longtable}{rp{2.2cm}p{2.4cm}p{3.2cm}l}"' _n
file write t1 `"\caption*{\textbf{Table 1---Populist Government Episodes, 1900--2020}} \\"' _n
file write t1 `"\hline\hline"' _n
file write t1 `"\multicolumn{5}{l}{\textit{Panel A. Populist leader spell (coded dataset)}} \\"' _n
file write t1 `"\hline"' _n
file write t1 `"No. & Country & Years & Leader & Left/Right \\"' _n
file write t1 `"\hline"' _n
file write t1 `"\endfirsthead"' _n
file write t1 `"\hline\hline"' _n
file write t1 `"\multicolumn{5}{l}{\textit{Panel A. Populist leader spell (coded dataset) -- continued}} \\"' _n
file write t1 `"\hline"' _n
file write t1 `"No. & Country & Years & Leader & Left/Right \\"' _n
file write t1 `"\hline"' _n
file write t1 `"\endhead"' _n
file write t1 `"\hline"' _n
file write t1 `"\endfoot"' _n

* ---- Panel A rows ----
file write t1 `"1. & Argentina & 1916--1922 & Yrigoyen & Left-wing \\"' _n
file write t1 `"2. & Argentina & 1928--1930 & Yrigoyen & Left-wing \\"' _n
file write t1 `"3. & Argentina & 1946--1955 & Per\'on & Left-wing \\"' _n
file write t1 `"4. & Argentina & 1973--1974 & Per\'on & Left-wing \\"' _n
file write t1 `"5. & Argentina & 1974--1976 & Mart\'inez de Per\'on & Left-wing \\"' _n
file write t1 `"6. & Argentina & 1989--1999 & Menem & Right-wing \\"' _n
file write t1 `"7. & Argentina & 2003--2007 & Kirchner & Left-wing \\"' _n
file write t1 `"8. & Argentina & 2007--2015 & Fern\'andez & Left-wing \\"' _n
file write t1 `"9. & Bolivia & 1952--1956 & Estenssoro & Left-wing \\"' _n
file write t1 `"10. & Bolivia & 1956--1960 & Zuazo & Left-wing \\"' _n
file write t1 `"11. & Bolivia & 1960--1964 & Estenssoro & Left-wing \\"' _n
file write t1 `"12. & Bolivia & 2006--2019 & Morales & Left-wing \\"' _n
file write t1 `"13. & Brazil & 1930--1945 & Vargas & Left-wing \\"' _n
file write t1 `"14. & Brazil & 1951--1954 & Vargas & Left-wing \\"' _n
file write t1 `"15. & Brazil & 1990--1992 & Collor & Right-wing \\"' _n
file write t1 `"16. & Brazil & 2019-- & Bolsonaro & Right-wing \\"' _n
file write t1 `"17. & Bulgaria & 2009--2013 & Borisov & Right-wing \\"' _n
file write t1 `"18. & Bulgaria & 2014--2017 & Borisov & Right-wing \\"' _n
file write t1 `"19. & Bulgaria & 2017-- & Borisov & Right-wing \\"' _n
file write t1 `"20. & Chile & 1920--1924 & Alessandri & Left-wing \\"' _n
file write t1 `"21. & Chile & within 1925 & Ib\'a\~nez & Left-wing \\"' _n
file write t1 `"22. & Chile & within 1925 & Alessandri & Left-wing \\"' _n
file write t1 `"23. & Chile & 1927--1931 & Ib\'a\~nez & Left-wing \\"' _n
file write t1 `"24. & Chile & 1932--1938 & Alessandri & Left-wing \\"' _n
file write t1 `"25. & Chile & 1952--1958 & Ib\'a\~nez & Left-wing \\"' _n
file write t1 `"26. & Ecuador & 1934--1935 & Velasco & Right-wing \\"' _n
file write t1 `"27. & Ecuador & 1944--1947 & Velasco & Right-wing \\"' _n
file write t1 `"28. & Ecuador & 1952--1956 & Velasco & Right-wing \\"' _n
file write t1 `"29. & Ecuador & 1960--1961 & Velasco & Right-wing \\"' _n
file write t1 `"30. & Ecuador & 1968--1972 & Velasco & Right-wing \\"' _n
file write t1 `"31. & Ecuador & 1996--1997 & Bucaram & Right-wing \\"' _n
file write t1 `"32. & Ecuador & 2007--2017 & Correa & Left-wing \\"' _n
file write t1 `"33. & Germany & 1933--1945 & Hitler & Right-wing \\"' _n
file write t1 `"34. & Greece & 2015--2019 & Tsipras & Left-wing \\"' _n
file write t1 `"35. & Hungary & 2010-- & Orb\'an & Right-wing \\"' _n
file write t1 `"36. & India & 1966--1977 & Gandhi & Left-wing \\"' _n
file write t1 `"37. & India & 2014-- & Modi & Right-wing \\"' _n
file write t1 `"38. & Indonesia & 1945--1948 & Sukarno & Left-wing \\"' _n
file write t1 `"39. & Indonesia & 1949--1966 & Sukarno & Left-wing \\"' _n
file write t1 `"40. & Indonesia & 2014-- & Widodo & Left-wing \\"' _n
file write t1 `"41. & Israel & 1996--1999 & Netanyahu & Right-wing \\"' _n
file write t1 `"42. & Israel & 2009-- & Netanyahu & Right-wing \\"' _n
file write t1 `"43. & Italy & 1922--1943 & Mussolini & Right-wing \\"' _n
file write t1 `"44. & Italy & 1994--1995 & Berlusconi & Right-wing \\"' _n
file write t1 `"45. & Italy & 2001--2006 & Berlusconi & Right-wing \\"' _n
file write t1 `"46. & Italy & 2008--2011 & Berlusconi & Right-wing \\"' _n
file write t1 `"47. & Italy & 2018-- & Lega/M5S & Right-wing \\"' _n
file write t1 `"48. & Japan & 2001--2006 & Koizumi & Right-wing \\"' _n
file write t1 `"49. & Mexico & 1934--1940 & C\'ardenas & Left-wing \\"' _n
file write t1 `"50. & Mexico & 1970--1976 & Echeverr\'ia & Left-wing \\"' _n
file write t1 `"51. & Mexico & 2018-- & L\'opez Obrador & Left-wing \\"' _n
file write t1 `"52. & New Zealand & 1975--1984 & Muldoon & Right-wing \\"' _n
file write t1 `"53. & Peru & 1985--1990 & Garc\'ia & Left-wing \\"' _n
file write t1 `"54. & Peru & 1990--2000 & Fujimori & Right-wing \\"' _n
file write t1 `"55. & Philippines & 1998--2001 & Estrada & Left-wing \\"' _n
file write t1 `"56. & Philippines & 2016-- & Duterte & Right-wing \\"' _n
file write t1 `"57. & Poland & 2005--2007 & Kaczy\'nski/PiS & Right-wing \\"' _n
file write t1 `"58. & Poland & 2015-- & PiS (J.\ Kaczy\'nski) & Right-wing \\"' _n
file write t1 `"59. & Slovakia & 1990--1991 & Me\v{c}iar & Right-wing \\"' _n
file write t1 `"60. & Slovakia & 1992--1994 & Me\v{c}iar & Right-wing \\"' _n
file write t1 `"61. & Slovakia & 1994--1998 & Me\v{c}iar & Right-wing \\"' _n
file write t1 `"62. & Slovakia & 2006--2010 & Fico & Left-wing \\"' _n
file write t1 `"63. & Slovakia & 2012--2018 & Fico & Left-wing \\"' _n
file write t1 `"64. & South Africa & 2009--2018 & Zuma & Left-wing \\"' _n
file write t1 `"65. & South Korea & 2003--2008 & Roh & Right-wing \\"' _n
file write t1 `"66. & Taiwan & 2000--2008 & Chen & Right-wing \\"' _n
file write t1 `"67. & Thailand & 2001--2006 & Shinawatra & Right-wing \\"' _n
file write t1 `"68. & Turkey & 2003-- & Erdo\u{g}an & Right-wing \\"' _n
file write t1 `"69. & United Kingdom & 2019-- & Johnson & Right-wing \\"' _n
file write t1 `"70. & United States & 2017-- & Trump & Right-wing \\"' _n
file write t1 `"71. & Venezuela & 1999--2013 & Ch\'avez & Left-wing \\"' _n
file write t1 `"72. & Venezuela & 2013-- & Maduro & Left-wing \\"' _n

* ---- Panel B header ----
file write t1 `"\hline\hline"' _n
file write t1 `"\multicolumn{5}{l}{\textit{Panel B. Populist episodes (for econometric analysis)}} \\"' _n
file write t1 `"\hline"' _n
file write t1 `"No. & Leader & Episode & Sample & \\"' _n
file write t1 `"\hline"' _n

* ---- Panel B rows ----
file write t1 `"1. & Yrigoyen & 1928--1930 & Extended & \\"' _n
file write t1 `"2. & Per\'on & 1946--1955 & Core & \\"' _n
file write t1 `"3. & Per\'on-Mart\'inez & 1973--1976 & Core & \\"' _n
file write t1 `"4. & Menem & 1989--1999 & Core & \\"' _n
file write t1 `"5. & Kirchner-Fern\'andez & 2003--2015 & Core & \\"' _n
file write t1 `"6. & Estenssoro-Zuazo & 1952--1964 & Core & \\"' _n
file write t1 `"7. & Morales & 2006--2019 & Extended & \\"' _n
file write t1 `"8. & Vargas & 1930--1945 & Extended & \\"' _n
file write t1 `"9. & Vargas & 1951--1954 & Core & \\"' _n
file write t1 `"10. & Collor & 1990--1992 & Core & \\"' _n
file write t1 `"11. & Bolsonaro & 2019-- & Extended & \\"' _n
file write t1 `"12. & Borisov & 2009-- & Extended & \\"' _n
file write t1 `"13. & Alessandri-Ib\'a\~nez & 1920--1938 & Extended & \\"' _n
file write t1 `"14. & Ib\'a\~nez & 1952--1958 & Core & \\"' _n
file write t1 `"15. & Velasco & 1934--1935 & Extended & \\"' _n
file write t1 `"16. & Velasco & 1952--1956 & Core & \\"' _n
file write t1 `"17. & Velasco & 1960--1961 & Core & \\"' _n
file write t1 `"18. & Velasco & 1968--1972 & Core & \\"' _n
file write t1 `"19. & Bucaram & 1996--1997 & Core & \\"' _n
file write t1 `"20. & Correa & 2007--2017 & Extended & \\"' _n
file write t1 `"21. & Hitler & 1933--1945 & Extended & \\"' _n
file write t1 `"22. & Tsipras & 2015--2019 & Extended & \\"' _n
file write t1 `"23. & Orb\'an & 2010-- & Extended & \\"' _n
file write t1 `"24. & Gandhi & 1966--1977 & Core & \\"' _n
file write t1 `"25. & Modi & 2014-- & Extended & \\"' _n
file write t1 `"26. & Widodo & 2014-- & Extended & \\"' _n
file write t1 `"27. & Netanyahu & 1996--1999 & Core & \\"' _n
file write t1 `"28. & Netanyahu & 2009-- & Extended & \\"' _n
file write t1 `"29. & Mussolini & 1922--1943 & Extended & \\"' _n
file write t1 `"30. & Berlusconi & 1994--1995 & Core & \\"' _n
file write t1 `"31. & Berlusconi & 2001--2011 & Core & \\"' _n
file write t1 `"32. & Lega/M5S & 2018-- & Extended & \\"' _n
file write t1 `"33. & Koizumi & 2001--2006 & Core & \\"' _n
file write t1 `"34. & C\'ardenas & 1934--1940 & Extended & \\"' _n
file write t1 `"35. & Echeverr\'ia & 1970--1976 & Core & \\"' _n
file write t1 `"36. & L\'opez Obrador & 2018-- & Extended & \\"' _n
file write t1 `"37. & Muldoon & 1975--1984 & Core & \\"' _n
file write t1 `"38. & Garc\'ia & 1985--1990 & Core & \\"' _n
file write t1 `"39. & Fujimori & 1990--2000 & Core & \\"' _n
file write t1 `"40. & Estrada & 1998--2001 & Core & \\"' _n
file write t1 `"41. & Duterte & 2016-- & Extended & \\"' _n
file write t1 `"42. & Kaczy\'nski/PiS & 2005--2007 & Extended & \\"' _n
file write t1 `"43. & PiS (J.\ Kaczy\'nski) & 2015-- & Extended & \\"' _n
file write t1 `"44. & Me\v{c}iar & 1990--1998 & Core & \\"' _n
file write t1 `"45. & Fico & 2006--2018 & Extended & \\"' _n
file write t1 `"46. & Zuma & 2009--2018 & Extended & \\"' _n
file write t1 `"47. & Roh & 2003--2008 & Core & \\"' _n
file write t1 `"48. & Chen & 2000--2008 & Core & \\"' _n
file write t1 `"49. & Shinawatra & 2001--2006 & Core & \\"' _n
file write t1 `"50. & Erdo\u{g}an & 2003-- & Core & \\"' _n
file write t1 `"51. & Johnson & 2019-- & Extended & \\"' _n
file write t1 `"52. & Trump & 2017-- & Extended & \\"' _n
file write t1 `"53. & Ch\'avez-Maduro & 1999-- & Core & \\"' _n

* ---- Close table and notes ----
file write t1 `"\hline\hline"' _n
file write t1 `"\multicolumn{5}{p{14cm}}{\textit{Notes:} Panel A: Dates/names from Archigos (Goemans, Gleditsch, and Chiozza 2009) until December 2015 and own coding based on Wikipedia from January 2016 to December 2020. Panel B: Spells two years or closer together by the same populist (or two populists with similar ideology) are connected. ``$-$'' = episode excluded because it starts during world war (1914--1918 or 1939--1945). Core sample: starting years 1946--2004. Extended sample: all other episodes.} \\"' _n
file write t1 `"\end{longtable}"' _n
file write t1 `"}"' _n  // close \footnotesize

file close t1

}
