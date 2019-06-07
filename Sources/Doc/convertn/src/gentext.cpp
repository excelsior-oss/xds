#include <string.h>
#include <stdio.h>

#include "basedefs.h"
#include "scanner.h"
#include "ir.h"

void IRNode::gentext(Write w) {
    IRNode *scan;
    for (scan = this->son; scan; scan = scan->next)
        scan->gentext(w);
}

void IRPlain::gentext(Write w) {
    w(text);
}

void IRCommand::gentext(Write w) {
    switch (code) {
    case t_hyphen:
    case t_endash:
    case t_emdash:
        w("-"); break;
    case t_lsquo:
        w("`"); break;
    case t_rsquo:
        w("'"); break;
    case t_ldquo:
    case t_rdquo:
        w("\""); break;
    case t_copyright:
        w("(c)"); break;
    case t_TeX:
        w("TeX"); break;
    case t_LaTeX:
        w("LaTeX"); break;
    case t_times:
        w("*"); break;
    case t_textbackslash:
    case t_backslash:
        w("\\"); break;
    case t_ldots:
        w("..."); break;
    case t_surd:
        w("V"); break;
    case t_subset:
        w("<"); break;
    case t_subseteq:
        w("<="); break;
    case t_supset:
        w(">"); break;
    case t_supseteq:
        w(">="); break;
    case t_leq:
        w("<="); break;
    case t_geq:
        w(">="); break;
    case t_neq:
        w("<>"); break;
    case t_pi:
        w("&pi;"); break;
    case t_exp:
        w("exp "); break;
    case t_ln:
        w("ln "); break;
    default:
        w("?");
        break;
    }
}

void IRItem::gentext(Write w) {
    if (son) {
        this->IRNode::gentext(w);
        w(" ");
    }
    else
        w("$ ");
}

void IRVerbatim::gentext(Write w) {
    String *scan = list.first;
    for (scan = list.first; scan; scan = scan->next)
        w(scan->string);
}

void IRTabular::gentext(Write w) {
    IRNode *row,*cell;
    for (row = this->son; row; row = row->next) {
        for (cell = row->son; cell; cell = cell->next) {
            cell->gentext(w);
            w("|");
        }
    }
}

void IRRef::gentext(Write w) {
//    char s[64];
    IRHeading* h = IRHeading::find(this->label);
    if (h) {
        if (son) this->IRNode::gentext(w); else h->IRNode::gentext(w);
    }
    else
        w("???");
}

void IRGraphics::gentext(Write w) {
    w("[");
    w(this->name);
    w("]");
}

