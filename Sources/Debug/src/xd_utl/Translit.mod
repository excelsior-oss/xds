IMPLEMENTATION MODULE Translit;

IMPORT SYSTEM;

TYPE
  TABLE = ARRAY [CHR(ORD(MAX(CHAR)) DIV 2+1)..MAX(CHAR)] OF CHAR;

  TABLES = ARRAY TRANSLITERATE_FROM_TO OF TABLE;


CONST
  Tables = TABLES {

--  NN =
       TABLE { CHR(0), CHR(0), CHR(0), CHR(0), CHR(0), CHR(0), CHR(0), CHR(0)
             , CHR(0), CHR(0), CHR(0), CHR(0), CHR(0), CHR(0), CHR(0), CHR(0)
             , CHR(0), CHR(0), CHR(0), CHR(0), CHR(0), CHR(0), CHR(0), CHR(0)
             , CHR(0), CHR(0), CHR(0), CHR(0), CHR(0), CHR(0), CHR(0), CHR(0)
             , CHR(0), CHR(0), CHR(0), CHR(0), CHR(0), CHR(0), CHR(0), CHR(0)
             , CHR(0), CHR(0), CHR(0), CHR(0), CHR(0), CHR(0), CHR(0), CHR(0)
             , CHR(0), CHR(0), CHR(0), CHR(0), CHR(0), CHR(0), CHR(0), CHR(0)
             , CHR(0), CHR(0), CHR(0), CHR(0), CHR(0), CHR(0), CHR(0), CHR(0)
             , CHR(0), CHR(0), CHR(0), CHR(0), CHR(0), CHR(0), CHR(0), CHR(0)
             , CHR(0), CHR(0), CHR(0), CHR(0), CHR(0), CHR(0), CHR(0), CHR(0)
             , CHR(0), CHR(0), CHR(0), CHR(0), CHR(0), CHR(0), CHR(0), CHR(0)
             , CHR(0), CHR(0), CHR(0), CHR(0), CHR(0), CHR(0), CHR(0), CHR(0)
             , CHR(0), CHR(0), CHR(0), CHR(0), CHR(0), CHR(0), CHR(0), CHR(0)
             , CHR(0), CHR(0), CHR(0), CHR(0), CHR(0), CHR(0), CHR(0), CHR(0)
             , CHR(0), CHR(0), CHR(0), CHR(0), CHR(0), CHR(0), CHR(0), CHR(0)
             , CHR(0), CHR(0), CHR(0), CHR(0), CHR(0), CHR(0), CHR(0), CHR(0)
             },
--  AI =
       TABLE { CHR(176), CHR(177), CHR(178), CHR(179), CHR(180), CHR(181), CHR(182), CHR(183)
             , CHR(184), CHR(185), CHR(186), CHR(187), CHR(188), CHR(189), CHR(190), CHR(191)
             , CHR(192), CHR(193), CHR(194), CHR(195), CHR(196), CHR(197), CHR(198), CHR(199)
             , CHR(200), CHR(201), CHR(202), CHR(203), CHR(204), CHR(205), CHR(206), CHR(207)
             , CHR(208), CHR(209), CHR(210), CHR(211), CHR(212), CHR(213), CHR(214), CHR(215)
             , CHR(216), CHR(217), CHR(218), CHR(219), CHR(220), CHR(221), CHR(222), CHR(223)
             , CHR(136), CHR(137), CHR(138), CHR(139), CHR(140), CHR(141), CHR(142), CHR(143)
             , CHR(144), CHR(145), CHR(146), CHR(147), CHR(148), CHR(149), CHR(150), CHR(151)
             , CHR(152), CHR(153), CHR(154), CHR(129), CHR(156), CHR(157), CHR(158), CHR(159)
             , CHR(160), CHR(161), CHR(162), CHR(163), CHR(164), CHR(165), CHR(166), CHR(167)
             , CHR(168), CHR(169), CHR(170), CHR(171), CHR(172), CHR(173), CHR(174), CHR(175)
             , CHR(240), CHR(241), CHR(242), CHR(243), CHR(244), CHR(245), CHR(246), CHR(247)
             , CHR(224), CHR(225), CHR(226), CHR(227), CHR(228), CHR(229), CHR(230), CHR(231)
             , CHR(232), CHR(233), CHR(234), CHR(235), CHR(236), CHR(237), CHR(238), CHR(239)
             , CHR(130), CHR(132), CHR(135), CHR(134), CHR(128), CHR(133), CHR(131), CHR(155)
             , CHR(248), CHR(249), CHR(250), CHR(251), CHR(252), CHR(253), CHR(254), CHR(255)
             },
  
--  AK =
       TABLE { CHR(225), CHR(226), CHR(247), CHR(231), CHR(228), CHR(229), CHR(246), CHR(250)
             , CHR(233), CHR(234), CHR(235), CHR(236), CHR(237), CHR(238), CHR(239), CHR(240)
             , CHR(242), CHR(243), CHR(244), CHR(245), CHR(230), CHR(232), CHR(227), CHR(254)
             , CHR(251), CHR(253), CHR(255), CHR(249), CHR(248), CHR(252), CHR(224), CHR(241)
             , CHR(193), CHR(194), CHR(215), CHR(199), CHR(196), CHR(197), CHR(214), CHR(218)
             , CHR(201), CHR(202), CHR(203), CHR(204), CHR(205), CHR(206), CHR(207), CHR(208)
             , CHR(136), CHR(137), CHR(138), CHR(139), CHR(140), CHR(141), CHR(142), CHR(143)
             , CHR(144), CHR(145), CHR(146), CHR(147), CHR(148), CHR(149), CHR(150), CHR(151)
             , CHR(152), CHR(153), CHR(154), CHR(129), CHR(156), CHR(157), CHR(158), CHR(159)
             , CHR(160), CHR(161), CHR(162), CHR(163), CHR(164), CHR(165), CHR(166), CHR(167)
             , CHR(168), CHR(169), CHR(170), CHR(171), CHR(172), CHR(173), CHR(174), CHR(175)
             , CHR(176), CHR(177), CHR(178), CHR(179), CHR(180), CHR(181), CHR(182), CHR(183)
             , CHR(210), CHR(211), CHR(212), CHR(213), CHR(198), CHR(200), CHR(195), CHR(222)
             , CHR(219), CHR(221), CHR(223), CHR(217), CHR(216), CHR(220), CHR(192), CHR(209)
             , CHR(130), CHR(132), CHR(135), CHR(134), CHR(128), CHR(133), CHR(131), CHR(155)
             , CHR(184), CHR(185), CHR(186), CHR(187), CHR(188), CHR(189), CHR(190), CHR(191)
             },
  
--  AW =
       TABLE { CHR(192), CHR(193), CHR(194), CHR(195), CHR(196), CHR(197), CHR(198), CHR(199)
             , CHR(200), CHR(201), CHR(202), CHR(203), CHR(204), CHR(205), CHR(206), CHR(207)
             , CHR(208), CHR(209), CHR(210), CHR(211), CHR(212), CHR(213), CHR(214), CHR(215)
             , CHR(216), CHR(217), CHR(218), CHR(219), CHR(220), CHR(221), CHR(222), CHR(223)
             , CHR(224), CHR(225), CHR(226), CHR(227), CHR(228), CHR(229), CHR(230), CHR(231)
             , CHR(232), CHR(233), CHR(234), CHR(235), CHR(236), CHR(237), CHR(238), CHR(239)
             , CHR(136), CHR(137), CHR(138), CHR(139), CHR(140), CHR(141), CHR(142), CHR(143)
             , CHR(144), CHR(145), CHR(146), CHR(147), CHR(148), CHR(149), CHR(150), CHR(151)
             , CHR(152), CHR(153), CHR(154), CHR(129), CHR(156), CHR(157), CHR(158), CHR(159)
             , CHR(160), CHR(161), CHR(162), CHR(163), CHR(164), CHR(165), CHR(166), CHR(167)
             , CHR(168), CHR(169), CHR(170), CHR(171), CHR(172), CHR(173), CHR(174), CHR(175)
             , CHR(176), CHR(177), CHR(178), CHR(179), CHR(180), CHR(181), CHR(182), CHR(183)
             , CHR(240), CHR(241), CHR(242), CHR(243), CHR(244), CHR(245), CHR(246), CHR(247)
             , CHR(248), CHR(249), CHR(250), CHR(251), CHR(252), CHR(253), CHR(254), CHR(255)
             , CHR(130), CHR(132), CHR(135), CHR(134), CHR(128), CHR(133), CHR(131), CHR(155)
             , CHR(184), CHR(185), CHR(186), CHR(187), CHR(188), CHR(189), CHR(190), CHR(191)
             },
  
--  IA =
       TABLE { CHR(244), CHR(195), CHR(240), CHR(246), CHR(241), CHR(245), CHR(243), CHR(242)
             , CHR(176), CHR(177), CHR(178), CHR(179), CHR(180), CHR(181), CHR(182), CHR(183)
             , CHR(184), CHR(185), CHR(186), CHR(187), CHR(188), CHR(189), CHR(190), CHR(191)
             , CHR(192), CHR(193), CHR(194), CHR(247), CHR(196), CHR(197), CHR(198), CHR(199)
             , CHR(200), CHR(201), CHR(202), CHR(203), CHR(204), CHR(205), CHR(206), CHR(207)
             , CHR(208), CHR(209), CHR(210), CHR(211), CHR(212), CHR(213), CHR(214), CHR(215)
             , CHR(128), CHR(129), CHR(130), CHR(131), CHR(132), CHR(133), CHR(134), CHR(135)
             , CHR(136), CHR(137), CHR(138), CHR(139), CHR(140), CHR(141), CHR(142), CHR(143)
             , CHR(144), CHR(145), CHR(146), CHR(147), CHR(148), CHR(149), CHR(150), CHR(151)
             , CHR(152), CHR(153), CHR(154), CHR(155), CHR(156), CHR(157), CHR(158), CHR(159)
             , CHR(160), CHR(161), CHR(162), CHR(163), CHR(164), CHR(165), CHR(166), CHR(167)
             , CHR(168), CHR(169), CHR(170), CHR(171), CHR(172), CHR(173), CHR(174), CHR(175)
             , CHR(224), CHR(225), CHR(226), CHR(227), CHR(228), CHR(229), CHR(230), CHR(231)
             , CHR(232), CHR(233), CHR(234), CHR(235), CHR(236), CHR(237), CHR(238), CHR(239)
             , CHR(216), CHR(217), CHR(218), CHR(219), CHR(220), CHR(221), CHR(222), CHR(223)
             , CHR(248), CHR(249), CHR(250), CHR(251), CHR(252), CHR(253), CHR(254), CHR(255)
             },
  
--  IK =
       TABLE { CHR(128), CHR(129), CHR(130), CHR(131), CHR(132), CHR(133), CHR(134), CHR(135)
             , CHR(136), CHR(137), CHR(138), CHR(139), CHR(140), CHR(141), CHR(142), CHR(143)
             , CHR(144), CHR(145), CHR(146), CHR(147), CHR(148), CHR(149), CHR(150), CHR(151)
             , CHR(152), CHR(153), CHR(154), CHR(155), CHR(156), CHR(157), CHR(158), CHR(159)
             , CHR(160), CHR(161), CHR(162), CHR(163), CHR(164), CHR(165), CHR(166), CHR(167)
             , CHR(168), CHR(169), CHR(170), CHR(171), CHR(172), CHR(173), CHR(174), CHR(175)
             , CHR(225), CHR(226), CHR(247), CHR(231), CHR(228), CHR(229), CHR(246), CHR(250)
             , CHR(233), CHR(234), CHR(235), CHR(236), CHR(237), CHR(238), CHR(239), CHR(240)
             , CHR(242), CHR(243), CHR(244), CHR(245), CHR(230), CHR(232), CHR(227), CHR(254)
             , CHR(251), CHR(253), CHR(255), CHR(249), CHR(248), CHR(252), CHR(224), CHR(241)
             , CHR(193), CHR(194), CHR(215), CHR(199), CHR(196), CHR(197), CHR(214), CHR(218)
             , CHR(201), CHR(202), CHR(203), CHR(204), CHR(205), CHR(206), CHR(207), CHR(208)
             , CHR(210), CHR(211), CHR(212), CHR(213), CHR(198), CHR(200), CHR(195), CHR(222)
             , CHR(219), CHR(221), CHR(223), CHR(217), CHR(216), CHR(220), CHR(192), CHR(209)
             , CHR(176), CHR(177), CHR(178), CHR(179), CHR(180), CHR(181), CHR(182), CHR(183)
             , CHR(184), CHR(185), CHR(186), CHR(187), CHR(188), CHR(189), CHR(190), CHR(191)
             },
  
--  IW =
       TABLE { CHR(128), CHR(129), CHR(130), CHR(131), CHR(132), CHR(133), CHR(134), CHR(135)
             , CHR(136), CHR(137), CHR(138), CHR(139), CHR(140), CHR(141), CHR(142), CHR(143)
             , CHR(144), CHR(145), CHR(146), CHR(147), CHR(148), CHR(149), CHR(150), CHR(151)
             , CHR(152), CHR(153), CHR(154), CHR(155), CHR(156), CHR(157), CHR(158), CHR(159)
             , CHR(160), CHR(161), CHR(162), CHR(163), CHR(164), CHR(165), CHR(166), CHR(167)
             , CHR(168), CHR(169), CHR(170), CHR(171), CHR(172), CHR(173), CHR(174), CHR(175)
             , CHR(192), CHR(193), CHR(194), CHR(195), CHR(196), CHR(197), CHR(198), CHR(199)
             , CHR(200), CHR(201), CHR(202), CHR(203), CHR(204), CHR(205), CHR(206), CHR(207)
             , CHR(208), CHR(209), CHR(210), CHR(211), CHR(212), CHR(213), CHR(214), CHR(215)
             , CHR(216), CHR(217), CHR(218), CHR(219), CHR(220), CHR(221), CHR(222), CHR(223)
             , CHR(224), CHR(225), CHR(226), CHR(227), CHR(228), CHR(229), CHR(230), CHR(231)
             , CHR(232), CHR(233), CHR(234), CHR(235), CHR(236), CHR(237), CHR(238), CHR(239)
             , CHR(240), CHR(241), CHR(242), CHR(243), CHR(244), CHR(245), CHR(246), CHR(247)
             , CHR(248), CHR(249), CHR(250), CHR(251), CHR(252), CHR(253), CHR(254), CHR(255)
             , CHR(176), CHR(177), CHR(178), CHR(179), CHR(180), CHR(181), CHR(182), CHR(183)
             , CHR(184), CHR(185), CHR(186), CHR(187), CHR(188), CHR(189), CHR(190), CHR(191)
             },
  
--  KA =
       TABLE { CHR(244), CHR(195), CHR(240), CHR(246), CHR(241), CHR(245), CHR(243), CHR(242)
             , CHR(176), CHR(177), CHR(178), CHR(179), CHR(180), CHR(181), CHR(182), CHR(183)
             , CHR(184), CHR(185), CHR(186), CHR(187), CHR(188), CHR(189), CHR(190), CHR(191)
             , CHR(192), CHR(193), CHR(194), CHR(247), CHR(196), CHR(197), CHR(198), CHR(199)
             , CHR(200), CHR(201), CHR(202), CHR(203), CHR(204), CHR(205), CHR(206), CHR(207)
             , CHR(208), CHR(209), CHR(210), CHR(211), CHR(212), CHR(213), CHR(214), CHR(215)
             , CHR(216), CHR(217), CHR(218), CHR(219), CHR(220), CHR(221), CHR(222), CHR(223)
             , CHR(248), CHR(249), CHR(250), CHR(251), CHR(252), CHR(253), CHR(254), CHR(255)
             , CHR(238), CHR(160), CHR(161), CHR(230), CHR(164), CHR(165), CHR(228), CHR(163)
             , CHR(229), CHR(168), CHR(169), CHR(170), CHR(171), CHR(172), CHR(173), CHR(174)
             , CHR(175), CHR(239), CHR(224), CHR(225), CHR(226), CHR(227), CHR(166), CHR(162)
             , CHR(236), CHR(235), CHR(167), CHR(232), CHR(237), CHR(233), CHR(231), CHR(234)
             , CHR(158), CHR(128), CHR(129), CHR(150), CHR(132), CHR(133), CHR(148), CHR(131)
             , CHR(149), CHR(136), CHR(137), CHR(138), CHR(139), CHR(140), CHR(141), CHR(142)
             , CHR(143), CHR(159), CHR(144), CHR(145), CHR(146), CHR(147), CHR(134), CHR(130)
             , CHR(156), CHR(155), CHR(135), CHR(152), CHR(157), CHR(153), CHR(151), CHR(154)
             },
  
--  KI =
       TABLE { CHR(128), CHR(129), CHR(130), CHR(131), CHR(132), CHR(133), CHR(134), CHR(135)
             , CHR(136), CHR(137), CHR(138), CHR(139), CHR(140), CHR(141), CHR(142), CHR(143)
             , CHR(144), CHR(145), CHR(146), CHR(147), CHR(148), CHR(149), CHR(150), CHR(151)
             , CHR(152), CHR(153), CHR(154), CHR(155), CHR(156), CHR(157), CHR(158), CHR(159)
             , CHR(160), CHR(161), CHR(162), CHR(163), CHR(164), CHR(165), CHR(166), CHR(167)
             , CHR(168), CHR(169), CHR(170), CHR(171), CHR(172), CHR(173), CHR(174), CHR(175)
             , CHR(240), CHR(241), CHR(242), CHR(243), CHR(244), CHR(245), CHR(246), CHR(247)
             , CHR(248), CHR(249), CHR(250), CHR(251), CHR(252), CHR(253), CHR(254), CHR(255)
             , CHR(238), CHR(208), CHR(209), CHR(230), CHR(212), CHR(213), CHR(228), CHR(211)
             , CHR(229), CHR(216), CHR(217), CHR(218), CHR(219), CHR(220), CHR(221), CHR(222)
             , CHR(223), CHR(239), CHR(224), CHR(225), CHR(226), CHR(227), CHR(214), CHR(210)
             , CHR(236), CHR(235), CHR(215), CHR(232), CHR(237), CHR(233), CHR(231), CHR(234)
             , CHR(206), CHR(176), CHR(177), CHR(198), CHR(180), CHR(181), CHR(196), CHR(179)
             , CHR(197), CHR(184), CHR(185), CHR(186), CHR(187), CHR(188), CHR(189), CHR(190)
             , CHR(191), CHR(207), CHR(192), CHR(193), CHR(194), CHR(195), CHR(182), CHR(178)
             , CHR(204), CHR(203), CHR(183), CHR(200), CHR(205), CHR(201), CHR(199), CHR(202)
             },
  
--  KW =
       TABLE { CHR(128), CHR(129), CHR(130), CHR(131), CHR(132), CHR(133), CHR(134), CHR(135)
             , CHR(136), CHR(137), CHR(138), CHR(139), CHR(140), CHR(141), CHR(142), CHR(143)
             , CHR(144), CHR(145), CHR(146), CHR(147), CHR(148), CHR(149), CHR(150), CHR(151)
             , CHR(152), CHR(153), CHR(154), CHR(155), CHR(156), CHR(157), CHR(158), CHR(159)
             , CHR(160), CHR(161), CHR(162), CHR(163), CHR(164), CHR(165), CHR(166), CHR(167)
             , CHR(168), CHR(169), CHR(170), CHR(171), CHR(172), CHR(173), CHR(174), CHR(175)
             , CHR(176), CHR(177), CHR(178), CHR(179), CHR(180), CHR(181), CHR(182), CHR(183)
             , CHR(184), CHR(185), CHR(186), CHR(187), CHR(188), CHR(189), CHR(190), CHR(191)
             , CHR(254), CHR(224), CHR(225), CHR(246), CHR(228), CHR(229), CHR(244), CHR(227)
             , CHR(245), CHR(232), CHR(233), CHR(234), CHR(235), CHR(236), CHR(237), CHR(238)
             , CHR(239), CHR(255), CHR(240), CHR(241), CHR(242), CHR(243), CHR(230), CHR(226)
             , CHR(252), CHR(251), CHR(231), CHR(248), CHR(253), CHR(249), CHR(247), CHR(250)
             , CHR(222), CHR(192), CHR(193), CHR(214), CHR(196), CHR(197), CHR(212), CHR(195)
             , CHR(213), CHR(200), CHR(201), CHR(202), CHR(203), CHR(204), CHR(205), CHR(206)
             , CHR(207), CHR(223), CHR(208), CHR(209), CHR(210), CHR(211), CHR(198), CHR(194)
             , CHR(220), CHR(219), CHR(199), CHR(216), CHR(221), CHR(217), CHR(215), CHR(218)
             },
  
--  WA =
       TABLE { CHR(244), CHR(195), CHR(240), CHR(246), CHR(241), CHR(245), CHR(243), CHR(242)
             , CHR(176), CHR(177), CHR(178), CHR(179), CHR(180), CHR(181), CHR(182), CHR(183)
             , CHR(184), CHR(185), CHR(186), CHR(187), CHR(188), CHR(189), CHR(190), CHR(191)
             , CHR(192), CHR(193), CHR(194), CHR(247), CHR(196), CHR(197), CHR(198), CHR(199)
             , CHR(200), CHR(201), CHR(202), CHR(203), CHR(204), CHR(205), CHR(206), CHR(207)
             , CHR(208), CHR(209), CHR(210), CHR(211), CHR(212), CHR(213), CHR(214), CHR(215)
             , CHR(216), CHR(217), CHR(218), CHR(219), CHR(220), CHR(221), CHR(222), CHR(223)
             , CHR(248), CHR(249), CHR(250), CHR(251), CHR(252), CHR(253), CHR(254), CHR(255)
             , CHR(128), CHR(129), CHR(130), CHR(131), CHR(132), CHR(133), CHR(134), CHR(135)
             , CHR(136), CHR(137), CHR(138), CHR(139), CHR(140), CHR(141), CHR(142), CHR(143)
             , CHR(144), CHR(145), CHR(146), CHR(147), CHR(148), CHR(149), CHR(150), CHR(151)
             , CHR(152), CHR(153), CHR(154), CHR(155), CHR(156), CHR(157), CHR(158), CHR(159)
             , CHR(160), CHR(161), CHR(162), CHR(163), CHR(164), CHR(165), CHR(166), CHR(167)
             , CHR(168), CHR(169), CHR(170), CHR(171), CHR(172), CHR(173), CHR(174), CHR(175)
             , CHR(224), CHR(225), CHR(226), CHR(227), CHR(228), CHR(229), CHR(230), CHR(231)
             , CHR(232), CHR(233), CHR(234), CHR(235), CHR(236), CHR(237), CHR(238), CHR(239)
             },
  
--  WI =
       TABLE { CHR(128), CHR(129), CHR(130), CHR(131), CHR(132), CHR(133), CHR(134), CHR(135)
             , CHR(136), CHR(137), CHR(138), CHR(139), CHR(140), CHR(141), CHR(142), CHR(143)
             , CHR(144), CHR(145), CHR(146), CHR(147), CHR(148), CHR(149), CHR(150), CHR(151)
             , CHR(152), CHR(153), CHR(154), CHR(155), CHR(156), CHR(157), CHR(158), CHR(159)
             , CHR(160), CHR(161), CHR(162), CHR(163), CHR(164), CHR(165), CHR(166), CHR(167)
             , CHR(168), CHR(169), CHR(170), CHR(171), CHR(172), CHR(173), CHR(174), CHR(175)
             , CHR(240), CHR(241), CHR(242), CHR(243), CHR(244), CHR(245), CHR(246), CHR(247)
             , CHR(248), CHR(249), CHR(250), CHR(251), CHR(252), CHR(253), CHR(254), CHR(255)
             , CHR(176), CHR(177), CHR(178), CHR(179), CHR(180), CHR(181), CHR(182), CHR(183)
             , CHR(184), CHR(185), CHR(186), CHR(187), CHR(188), CHR(189), CHR(190), CHR(191)
             , CHR(192), CHR(193), CHR(194), CHR(195), CHR(196), CHR(197), CHR(198), CHR(199)
             , CHR(200), CHR(201), CHR(202), CHR(203), CHR(204), CHR(205), CHR(206), CHR(207)
             , CHR(208), CHR(209), CHR(210), CHR(211), CHR(212), CHR(213), CHR(214), CHR(215)
             , CHR(216), CHR(217), CHR(218), CHR(219), CHR(220), CHR(221), CHR(222), CHR(223)
             , CHR(224), CHR(225), CHR(226), CHR(227), CHR(228), CHR(229), CHR(230), CHR(231)
             , CHR(232), CHR(233), CHR(234), CHR(235), CHR(236), CHR(237), CHR(238), CHR(239)
             },
  
--  WK =
       TABLE { CHR(128), CHR(129), CHR(130), CHR(131), CHR(132), CHR(133), CHR(134), CHR(135)
             , CHR(136), CHR(137), CHR(138), CHR(139), CHR(140), CHR(141), CHR(142), CHR(143)
             , CHR(144), CHR(145), CHR(146), CHR(147), CHR(148), CHR(149), CHR(150), CHR(151)
             , CHR(152), CHR(153), CHR(154), CHR(155), CHR(156), CHR(157), CHR(158), CHR(159)
             , CHR(160), CHR(161), CHR(162), CHR(163), CHR(164), CHR(165), CHR(166), CHR(167)
             , CHR(168), CHR(169), CHR(170), CHR(171), CHR(172), CHR(173), CHR(174), CHR(175)
             , CHR(176), CHR(177), CHR(178), CHR(179), CHR(180), CHR(181), CHR(182), CHR(183)
             , CHR(184), CHR(185), CHR(186), CHR(187), CHR(188), CHR(189), CHR(190), CHR(191)
             , CHR(225), CHR(226), CHR(247), CHR(231), CHR(228), CHR(229), CHR(246), CHR(250)
             , CHR(233), CHR(234), CHR(235), CHR(236), CHR(237), CHR(238), CHR(239), CHR(240)
             , CHR(242), CHR(243), CHR(244), CHR(245), CHR(230), CHR(232), CHR(227), CHR(254)
             , CHR(251), CHR(253), CHR(255), CHR(249), CHR(248), CHR(252), CHR(224), CHR(241)
             , CHR(193), CHR(194), CHR(215), CHR(199), CHR(196), CHR(197), CHR(214), CHR(218)
             , CHR(201), CHR(202), CHR(203), CHR(204), CHR(205), CHR(206), CHR(207), CHR(208)
             , CHR(210), CHR(211), CHR(212), CHR(213), CHR(198), CHR(200), CHR(195), CHR(222)
             , CHR(219), CHR(221), CHR(223), CHR(217), CHR(216), CHR(220), CHR(192), CHR(209)
             }
                  };

 

PROCEDURE TransliterateStr (from_to: TRANSLITERATE_FROM_TO; VAR str: ARRAY OF CHAR);
VAR
  l, i: CARDINAL;
BEGIN
  IF from_to = nn THEN
    RETURN;
  END;
  l := LENGTH(str);
  IF l = 0 THEN
    RETURN;
  END;
  FOR i := 0 TO l-1 DO
    IF str[i] > CHR(ORD(MAX(CHAR)) DIV 2) THEN
      str[i] := Tables[from_to][str[i]];
    END;
  END;
END TransliterateStr; 


PROCEDURE TransliterateBuffer (from_to: TRANSLITERATE_FROM_TO; buffer: SYSTEM.ADDRESS; size: CARDINAL);
VAR
  i: CARDINAL;
  p: POINTER TO CHAR;
BEGIN
  IF from_to = nn THEN
    RETURN;
  END;
  IF size = 0 THEN
    RETURN;
  END;
  FOR i := 1 TO size DO
    p := SYSTEM.ADDADR(buffer, i-1);
    IF p^ > CHR(ORD(MAX(CHAR)) DIV 2) THEN
      p^ := Tables[from_to][p^];
    END;
  END;
END TransliterateBuffer; 

                  
END Translit.
