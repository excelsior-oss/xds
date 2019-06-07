//
// Tables of predefined commands, control symbols and environments
// Included into 'scanner.cpp'.
//

#define CW(text) { #text, t_##text }

const struct {
  char text[24];
  int  token;
} CWTable[1024] =
{   CW(input),            CW(include),          CW(includeonly),

    CW(begin),            CW(end),              CW(item),

    CW(part),             CW(chapter),          CW(section),
    CW(subsection),       CW(subsubsection),    CW(paragraph),
    CW(subparagraph),     CW(appendix),

    CW(par),

    CW(title),

    CW(label),            CW(ref),              CW(index),
    CW(public),           CW(extern),

    CW(caption),

    CW(verb),

    CW(newline),          CW(tabularnewline),

    CW(mbox),

    CW(footnote),

    CW(includegraphics),

    CW(nominitoc),

    CW(url),              CW(href),

    CW(rm),               CW(bf),               CW(sf),
    CW(it),               CW(sl),               CW(sc),
    CW(tt),               CW(em),

    CW(mdseries),         CW(bfseries),

    CW(tiny),             CW(scriptsize),       CW(footnotesize),
    CW(small),            CW(normalsize),       CW(large),
    CW(Large),            CW(LARGE),            CW(huge),
    CW(Huge),

    CW(newcommand),       CW(renewcommand),
    CW(newenvironment),   CW(renewenvironment),

    CW(newif),            CW(else),             CW(fi),

    CW(hline),            CW(cline),            CW(vline),
    CW(multicolumn),

    CW(u),                CW(v),                CW(H),
    CW(t),                CW(c),                CW(d),
    CW(b),

    CW(oe),               CW(OE),               CW(ae),
    CW(AE),               CW(aa),               CW(AA),
    CW(o),                CW(O),                CW(l),
    CW(L),                CW(ss),

    CW(dag),              CW(ddag),             CW(S),
    CW(P),                CW(copyright),        CW(pounds),

    CW(hyphen),           CW(endash),           CW(emdash),
    CW(cdots),            CW(ldots),

    CW(textbackslash),

    CW(TeX),              CW(LaTeX),

    // Greek Letters

    CW(alpha),            CW(beta),             CW(gamma),
    CW(delta),            CW(epsilon),          CW(varepsilon),
    CW(zeta),             CW(eta),              CW(theta),
    CW(vartheta),         CW(iota),             CW(kappa),
    CW(lambda),           CW(mu),               CW(nu),
    CW(xi),               CW(pi),               CW(varpi),
    CW(rho),              CW(varrho),           CW(sigma),
    CW(varsigma),         CW(tau),              CW(upsilon),
    CW(phi),              CW(varphi),           CW(chi),
    CW(psi),              CW(omega),

    CW(Gamma),            CW(Delta),            CW(Theta),
    CW(Lambda),           CW(Xi),               CW(Pi),
    CW(Sigma),            CW(Upsilon),          CW(Phi),
    CW(Psi),              CW(Omega),

    // Binary Operation Symbols

    CW(pm),               CW(mp),               CW(times),
    CW(div),              CW(ast),              CW(star),
    CW(circ),             CW(bullet),           CW(cdot),

    CW(cap),              CW(cup),              CW(uplus),
    CW(sqcap),            CW(sqcup),            CW(vee),
    CW(wedge),            CW(setminus),         CW(wr),

    CW(diamond),          CW(bigtriangleup),    CW(bigtriangledown),
    CW(triangleleft),     CW(triangleright),    CW(lhd),
    CW(rhd),              CW(unlhd),            CW(unrhd),

    CW(oplus),            CW(ominus),           CW(otimes),
    CW(oslash),           CW(odot),             CW(bigcirc),
    CW(dagger),           CW(ddagger),          CW(amalg),

    // Relation Symbols

    CW(leq),              CW(prec),             CW(preceq),
    CW(ll),               CW(subset),           CW(subseteq),
    CW(sqsubset),         CW(sqsubseteq),       CW(in),
    CW(vdash),

    CW(geq),              CW(succ),             CW(succeq),
    CW(gg),               CW(supset),           CW(supseteq),
    CW(sqsupset),         CW(sqsupseteq),       CW(ni),
    CW(dashv),

    CW(equiv),            CW(sim),              CW(simeq),
    CW(asymp),            CW(approx),           CW(cong),
    CW(neq),              CW(doteq),            CW(propto),

    CW(models),           CW(perp),             CW(mid),
    CW(parallel),         CW(bowtie),           CW(Join),
    CW(smile),            CW(frown),

    CW(leftarrow),        CW(Leftarrow),
    CW(rightarrow),       CW(Rightarrow),
    CW(leftrightarrow),   CW(Leftrightarrow),
    CW(mapsto),           CW(hookleftarrow),
    CW(leftharpoonup),    CW(leftharpoondown),

    CW(longleftarrow),      CW(Longleftarrow),
    CW(longrightarrow),     CW(Longrightarrow),
    CW(longleftrightarrow), CW(Longleftrightarrow),
    CW(longmapsto),         CW(hookrightarrow),
    CW(rightharpoonup),     CW(rightharpoondown),

    CW(uparrow),            CW(Uparrow),
    CW(downarrow),          CW(Downarrow),
    CW(updownarrow),        CW(Updownarrow),
    CW(nearrow),            CW(searrow),
    CW(swarrow),            CW(nwarrow),

    CW(rightleftharpoons),  CW(leadsto),

    // Miscellanious Symbols

    CW(aleph),              CW(hbar),           CW(imath),
    CW(jmath),              CW(eli),            CW(wp),
    CW(Re),                 CW(Im),             CW(mho),

    CW(prime),              CW(emptyset),       CW(nabla),
    CW(surd),               CW(top),            CW(bot),
    CW(angle),

    CW(forall),             CW(exists),         CW(neg),
    CW(flat),               CW(natural),        CW(sharp),
    CW(backslash),          CW(partial),

    CW(infty),              CW(Box),            CW(Diamond),
    CW(triangle),           CW(clubsuit),       CW(diamondsuit),
    CW(heartsuit),          CW(spadesuit),

    // Variable-sized Symbols

    CW(sum),                CW(prod),           CW(coprod),
    CW(int),                CW(oint),

    CW(bigcap),             CW(bigcup),         CW(bigsqcap),
    CW(bigsqcup),           CW(bigvee),         CW(bigwedge),

    CW(bigodot),            CW(bigotimes),      CW(bigoplus),
    CW(biguplus),

    // Log-like Functions

    CW(arccos),             CW(arcsin),         CW(arctan),
    CW(arg),                CW(cos),            CW(cosh),
    CW(cot),                CW(coth),

    CW(csc),                CW(deg),            CW(det),
    CW(dim),                CW(exp),            CW(gcd),
    CW(hom),                CW(inf),

    CW(ker),                CW(lg),             CW(lim),
    CW(liminf),             CW(limsup),         CW(ln),
    CW(log),                CW(max),

    CW(min),                CW(Pr),             CW(sec),
    CW(sin),                CW(sinh),           CW(sup),
    CW(tan),                CW(tanh),

    // That's all

    CW(unknowncw)
};

struct {
    char symbol;
    int  token;
} CSTable [32] =
{ { '\\', t_newline },
  { ' ',  t_wordspace },
  { '!',  t_negthinspace },
  { '\'', t_accent },
  { '(',  t_beginformula },
  { ')',  t_endformula },
  { '+',  t_tabp },
  { ',',  t_thinspace },
  { '-',  t_hyphenation },
  { '/',  t_italicsspace },
  { ':',  t_mediumspace },
  { ';',  t_thickspace },
  { '<',  t_tabl },
  { '=',  t_tabe },
  { '>',  t_tabg },
  { '@',  t_eosspace },
  { '[',  t_begindispformula },
  { ']',  t_enddispformula },
  { '`',  t_backaccent },
// !!! \| !!!
  { ' ',   t_unknowncs }
};

#define EN(text) { #text, e_##text }

struct {
  char text[16];
  int  code;
} ENTable[32] =
{   EN(document),
    EN(verbatim),
    EN(itemize),    EN(enumerate),  EN(description),
    EN(center),     EN(flushleft),  EN(flushright),
    EN(table),      EN(figure),     EN(popup),
    EN(tabbing),    EN(tabular),
    { "tabular*",     e_tabularw },
    { "",e_unknown }
};

int FindEN(const char text[]) {
    int i;

    for (i = 0; ENTable[i].code != e_unknown; i++)
        if (strcmp(ENTable[i].text,text)==0) return ENTable[i].code;
    return e_unknown;
}
