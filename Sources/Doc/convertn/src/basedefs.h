#define HTMLNAV_NONE    0
#define HTMLNAV_TOCONLY 1
#define HTMLNAV_FULL    2

/* Primitive Tokens */

#define t_unknowncs  (-1)
#define t_unknowncw  (-2)
#define t_ignorable  (-3)

#define t_eof          0
#define t_plain        1
#define t_lbrace       2
#define t_rbrace       3
#define t_macropar     4
#define t_math         5
#define t_superscript  6
#define t_subscript    7
#define t_tab          8
#define t_tie          9
#define t_lbracket    10
#define t_rbracket    11


/* control Symbols */

#define t_firstcs                100
#define t_wordspace              100
#define t_negthinspace           101
#define t_accent                 102
#define t_beginformula           103
#define t_endformula             104
#define t_tabp                   105
#define t_thinspace              106
#define t_hyphenation            107
#define t_italicsspace           108
#define t_mediumspace            109
#define t_thickspace             110
#define t_tabl                   111
#define t_tabe                   112
#define t_tabg                   113
#define t_eosspace               114
#define t_begindispformula       115
#define t_enddispformula         116
#define t_backaccent             117

/* Control Words */

#define t_firstcw               1000
#define t_input                 1000
#define t_include               1001
#define t_includeonly           1002

#define t_begin                 1010
#define t_end                   1011
#define t_par                   1012
#define t_newline               1013
#define t_tabularnewline        1014

// Headings

#define t_part                  1020
#define t_chapter               1021
#define t_section               1022
#define t_subsection            1023
#define t_subsubsection         1024
#define t_paragraph             1025
#define t_subparagraph          1026

#define t_appendix              1030
#define t_caption               1031

#define t_title                 1050

// Index and cross-referencing

#define t_label                 1060
#define t_ref                   1061
#define t_index                 1062
#define t_public                1070    // LaTeX OnLine specific
#define t_extern                1071    // LaTeX OnLine specific

#define t_verb                  1080

#define t_mbox                  1090

#define t_footnote              1100

#define t_includegraphics       1110   

#define t_nominitoc             1210    // LaTeX OnLine specific

#define t_url                   1220    // url.sty (also loaded by hyperref.sty)
#define t_href                  1221    // hyperref.sty

// Typefaces

#define t_rm                    1400  // Roman
#define t_bf                    1401  // Bold
#define t_sf                    1402  // Sans Serif
#define t_it                    1403  // Italic
#define t_sl                    1404  // Slanted
#define t_sc                    1405  // Small Caps
#define t_tt                    1406  // Typewriter
#define t_em                    1407  // Emphatic

#define t_mdseries              1410  // Normal 
#define t_bfseries              1411  // Bold

// Font sizes

#define t_tiny                  1490
#define t_scriptsize            1491
#define t_footnotesize          1492
#define t_small                 1493
#define t_normalsize            1494
#define t_large                 1495
#define t_Large                 1496
#define t_LARGE                 1497
#define t_huge                  1498
#define t_Huge                  1499


// Environment-specific commands

#define t_item                  1500
#define t_hline                 1510
#define t_cline                 1511
#define t_vline                 1512
#define t_multicolumn           1513

// Definitions

#define t_newcommand            1600
#define t_renewcommand          1601
#define t_usercommand           1602
#define t_newenvironment        1605
#define t_renewenvironment      1606
#define t_userenvironment       1607

// Conditional sections

#define t_newif                 1700
#define t_if                    1701
#define t_else                  1702
#define t_fi                    1703
#define t_true                  1704
#define t_false                 1705

// Accents

#define t_u                     1800
#define t_v                     1801
#define t_H                     1802
#define t_t                     1803
#define t_c                     1804
#define t_d                     1805
#define t_b                     1806

#define firstspecsymbol         1900

// Foreign-language symbols

#define t_oe                    1910
#define t_OE                    1911
#define t_ae                    1912
#define t_AE                    1913
#define t_aa                    1914
#define t_AA                    1915
#define t_o                     1916
#define t_O                     1917
#define t_l                     1918
#define t_L                     1919
#define t_ss                    1920

// Special symbols

#define t_dag                   1930
#define t_ddag                  1931
#define t_S                     1932
#define t_P                     1933
#define t_copyright             1934
#define t_pounds                1935

// punctuation

#define t_hyphen                1940
#define t_endash                1941
#define t_emdash                1942
#define t_cdots                 1943
#define t_ldots                 1944
#define t_lsquo                 1945
#define t_rsquo                 1946
#define t_ldquo                 1947
#define t_rdquo                 1948


#define t_textbackslash         1950

// TeX and LaTeX logos

#define t_TeX                   1990
#define t_LaTeX                 1991

//
// Math mode symbols
//

#define firstmathsymbol         2000

// Greek Letters

#define t_alpha                 2000
#define t_beta                  2001
#define t_gamma                 2002
#define t_delta                 2003
#define t_epsilon               2004
#define t_varepsilon            2005
#define t_zeta                  2006
#define t_eta                   2007
#define t_theta                 2008
#define t_vartheta              2009
#define t_iota                  2010
#define t_kappa                 2011
#define t_lambda                2012
#define t_mu                    2013
#define t_nu                    2014
#define t_xi                    2015
#define t_pi                    2016
#define t_varpi                 2017
#define t_rho                   2018
#define t_varrho                2019
#define t_sigma                 2020
#define t_varsigma              2021
#define t_tau                   2022
#define t_upsilon               2023
#define t_phi                   2024
#define t_varphi                2025
#define t_chi                   2026
#define t_psi                   2027
#define t_omega                 2028

#define t_Gamma                 2050
#define t_Delta                 2051
#define t_Theta                 2052
#define t_Lambda                2053
#define t_Xi                    2054
#define t_Pi                    2055
#define t_Sigma                 2056
#define t_Upsilon               2057
#define t_Phi                   2058
#define t_Psi                   2059
#define t_Omega                 2060

// Binary Operation Symbols

#define t_pm                    2100
#define t_mp                    2101
#define t_times                 2102
#define t_div                   2103
#define t_ast                   2104
#define t_star                  2105
#define t_circ                  2106
#define t_bullet                2107
#define t_cdot                  2108

#define t_cap                   2110
#define t_cup                   2111
#define t_uplus                 2112
#define t_sqcap                 2113
#define t_sqcup                 2114
#define t_vee                   2115
#define t_wedge                 2116
#define t_setminus              2117
#define t_wr                    2118

#define t_diamond               2120
#define t_bigtriangleup         2121
#define t_bigtriangledown       2122
#define t_triangleleft          2123
#define t_triangleright         2124
#define t_lhd                   2125
#define t_rhd                   2126
#define t_unlhd                 2127
#define t_unrhd                 2128

#define t_oplus                 2130
#define t_ominus                2131
#define t_otimes                2132
#define t_oslash                2133
#define t_odot                  2134
#define t_bigcirc               2135
#define t_dagger                2136
#define t_ddagger               2137
#define t_amalg                 2138

// Relation Symbols

#define t_leq                   2200
#define t_prec                  2201
#define t_preceq                2202
#define t_ll                    2203
#define t_subset                2204
#define t_subseteq              2205
#define t_sqsubset              2206
#define t_sqsubseteq            2207
#define t_in                    2208
#define t_vdash                 2209

#define t_geq                   2210
#define t_succ                  2211
#define t_succeq                2212
#define t_gg                    2213
#define t_supset                2214
#define t_supseteq              2215
#define t_sqsupset              2216
#define t_sqsupseteq            2217
#define t_ni                    2218
#define t_dashv                 2219

#define t_equiv                 2220
#define t_sim                   2221
#define t_simeq                 2222
#define t_asymp                 2223
#define t_approx                2224
#define t_cong                  2225
#define t_neq                   2226
#define t_doteq                 2227
#define t_propto                2228

#define t_models                2230
#define t_perp                  2231
#define t_mid                   2232
#define t_parallel              2233
#define t_bowtie                2234
#define t_Join                  2235
#define t_smile                 2236
#define t_frown                 2237

#define t_leftarrow             2300
#define t_Leftarrow             2301
#define t_rightarrow            2302
#define t_Rightarrow            2303
#define t_leftrightarrow        2304
#define t_Leftrightarrow        2305
#define t_mapsto                2306
#define t_hookleftarrow         2307
#define t_leftharpoonup         2308
#define t_leftharpoondown       2309

#define t_longleftarrow         2310
#define t_Longleftarrow         2311
#define t_longrightarrow        2312
#define t_Longrightarrow        2313
#define t_longleftrightarrow    2314
#define t_Longleftrightarrow    2315
#define t_longmapsto            2316
#define t_hookrightarrow        2317
#define t_rightharpoonup        2318
#define t_rightharpoondown      2319

#define t_uparrow               2320
#define t_Uparrow               2321
#define t_downarrow             2322
#define t_Downarrow             2323
#define t_updownarrow           2324
#define t_Updownarrow           2325
#define t_nearrow               2326
#define t_searrow               2327
#define t_swarrow               2328
#define t_nwarrow               2329

#define t_rightleftharpoons     2330
#define t_leadsto               2331

// Miscellanious Symbols

#define t_aleph                 2400
#define t_hbar                  2401
#define t_imath                 2402
#define t_jmath                 2403
#define t_eli                   2404
#define t_wp                    2405
#define t_Re                    2406
#define t_Im                    2407
#define t_mho                   2408

#define t_prime                 2410
#define t_emptyset              2411
#define t_nabla                 2412
#define t_surd                  2413
#define t_top                   2414
#define t_bot                   2415
#define t_angle                 2416

#define t_forall                2420
#define t_exists                2421
#define t_neg                   2422
#define t_flat                  2423
#define t_natural               2424
#define t_sharp                 2425
#define t_backslash             2426
#define t_partial               2427

#define t_infty                 2430
#define t_Box                   2431
#define t_Diamond               2432
#define t_triangle              2433
#define t_clubsuit              2434
#define t_diamondsuit           2435
#define t_heartsuit             2436
#define t_spadesuit             2437

// Variable-sized Symbols

#define t_sum                   2500
#define t_prod                  2501
#define t_coprod                2502
#define t_int                   2503
#define t_oint                  2504

#define t_bigcap                2510
#define t_bigcup                2511
#define t_bigsqcap              2512
#define t_bigsqcup              2513
#define t_bigvee                2514
#define t_bigwedge              2515

#define t_bigodot               2520
#define t_bigotimes             2521
#define t_bigoplus              2522
#define t_biguplus              2523

// Log-like Functions

#define t_arccos                2600
#define t_arcsin                2601
#define t_arctan                2602
#define t_arg                   2603
#define t_cos                   2604
#define t_cosh                  2605
#define t_cot                   2606
#define t_coth                  2607

#define t_csc                   2610
#define t_deg                   2611
#define t_det                   2612
#define t_dim                   2613
#define t_exp                   2614
#define t_gcd                   2615
#define t_hom                   2616
#define t_inf                   2617

#define t_ker                   2620
#define t_lg                    2621
#define t_lim                   2622
#define t_liminf                2623
#define t_limsup                2624
#define t_ln                    2625
#define t_log                   2626
#define t_max                   2627

#define t_min                   2630
#define t_Pr                    2631
#define t_sec                   2632
#define t_sin                   2633
#define t_sinh                  2634
#define t_sup                   2635
#define t_tan                   2636
#define t_tanh                  2637

// Environments

#define e_unknown     (-1)
#define e_document    0
#define e_verbatim    1
#define e_itemize     2
#define e_enumerate   3
#define e_description 4
#define e_center      5
#define e_flushleft   6
#define e_flushright  7
#define e_table       10
#define e_figure      11
#define e_popup       12    // LaTeX OnLine extension
#define e_tabbing     20
#define e_tabular     21
#define e_tabularw    22

// Fonts

#define f_rm     t_rm
#define f_bf     t_bf
#define f_sf     t_sf
#define f_it     t_it
#define f_sl     t_sl
#define f_sc     t_sc
#define f_tt     t_tt
#define f_math   t_math
#define f_tab    t_tab

#define BUFLEN 1024
