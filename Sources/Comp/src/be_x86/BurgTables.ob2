-- source grammar = e386.b
<*+WOFF*>
MODULE BurgTables;

IMPORT ir, RD := RDefs, BurgNT, SYSTEM;
TYPE Index *= INTEGER;
TYPE NTnts_nType = ARRAY OF BurgNT.NT;
CONST NTnts_n*= NTnts_nType {
	BurgNT.NTreg, BurgNT.NTnowhere , (*0*)
	BurgNT.NTnowhere , (*1*)
	BurgNT.NTrc, BurgNT.NTnowhere , (*2*)
	BurgNT.NTmem, BurgNT.NTnowhere , (*3*)
	BurgNT.NTbased, BurgNT.NTnowhere , (*4*)
	BurgNT.NTscaled, BurgNT.NTnowhere , (*5*)
	BurgNT.NTbased, BurgNT.NTbased, BurgNT.NTnowhere , (*6*)
	BurgNT.NTbased, BurgNT.NTscaled, BurgNT.NTnowhere , (*7*)
	BurgNT.NTscaled, BurgNT.NTbased, BurgNT.NTnowhere , (*8*)
	BurgNT.NTaddr, BurgNT.NTnowhere , (*9*)
	BurgNT.NTreg, BurgNT.NTmrc, BurgNT.NTnowhere , (*10*)
	BurgNT.NTmrc, BurgNT.NTreg, BurgNT.NTnowhere , (*11*)
	BurgNT.NTmrc, BurgNT.NTnowhere , (*12*)
	BurgNT.NTmem, BurgNT.NTrc, BurgNT.NTnowhere , (*13*)
	BurgNT.NTrc, BurgNT.NTmem, BurgNT.NTnowhere , (*14*)
	BurgNT.NTaddr, BurgNT.NTmem, BurgNT.NTrc, BurgNT.NTnowhere , (*15*)
	BurgNT.NTaddr, BurgNT.NTrc, BurgNT.NTmem, BurgNT.NTnowhere , (*16*)
	BurgNT.NTaddr, BurgNT.NTmem, BurgNT.NTnowhere , (*17*)
	BurgNT.NTrc, BurgNT.NTmrc, BurgNT.NTnowhere , (*18*)
	BurgNT.NTmrc, BurgNT.NTrc, BurgNT.NTnowhere , (*19*)
	BurgNT.NTreg, BurgNT.NTreg, BurgNT.NTnowhere , (*20*)
	BurgNT.NTmem, BurgNT.NTreg, BurgNT.NTnowhere , (*21*)
	BurgNT.NTaddr, BurgNT.NTmem, BurgNT.NTreg, BurgNT.NTnowhere , (*22*)
	BurgNT.NTaddr, BurgNT.NTrc, BurgNT.NTnowhere , (*23*)
	BurgNT.NTtos, BurgNT.NTnowhere , (*24*)
	BurgNT.NTrc, BurgNT.NTrc, BurgNT.NTrc, BurgNT.NTnowhere , (*25*)
	BurgNT.NTrc, BurgNT.NTrc, BurgNT.NTnowhere , (*26*)
	BurgNT.NTreg, BurgNT.NTmem, BurgNT.NTnowhere , (*27*)
	BurgNT.NTreg, BurgNT.NTaddr, BurgNT.NTnowhere , (*28*)
	BurgNT.NTlocal, BurgNT.NTnowhere , (*29*)
	BurgNT.NTtos, BurgNT.NTtos, BurgNT.NTnowhere , (*30*)
	BurgNT.NTtos, BurgNT.NTmem, BurgNT.NTnowhere , (*31*)
	BurgNT.NTmem, BurgNT.NTtos, BurgNT.NTnowhere , (*32*)
	BurgNT.NTaddr, BurgNT.NTtos, BurgNT.NTnowhere , (*33*)
	BurgNT.NTaddr, BurgNT.NTreg, BurgNT.NTmrc, BurgNT.NTnowhere , (*34*)
	BurgNT.NTaddr, BurgNT.NTmrc, BurgNT.NTreg, BurgNT.NTnowhere , (*35*)
	BurgNT.NTaddr, BurgNT.NTreg, BurgNT.NTnowhere  (*36*)
};
CONST nRules = BurgNT.Rule{ 263 };
TYPE RuleRange   = BurgNT.Rule[BurgNT.Rule{0}..nRules];
     NTntsType = ARRAY RuleRange OF Index;
CONST NTnts*= NTntsType {
	Index{ 0 }, (*0*)
	Index{ 0 }, (*1->0*) 
	Index{ 2 }, (*2->1*) 
	Index{ 3 }, (*3->2*) 
	Index{ 5 }, (*4->3*) 
	Index{ 0 }, (*5->0*) 
	Index{ 2 }, (*6->1*) 
	Index{ 2 }, (*7->1*) 
	Index{ 7 }, (*8->4*) 
	Index{ 7 }, (*9->4*) 
	Index{ 2 }, (*10->1*) 
	Index{ 0 }, (*11->0*) 
	Index{ 0 }, (*12->0*) 
	Index{ 9 }, (*13->5*) 
	Index{ 9 }, (*14->5*) 
	Index{ 7 }, (*15->4*) 
	Index{ 11 }, (*16->6*) 
	Index{ 9 }, (*17->5*) 
	Index{ 14 }, (*18->7*) 
	Index{ 17 }, (*19->8*) 
	Index{ 2 }, (*20->1*) 
	Index{ 2 }, (*21->1*) 
	Index{ 20 }, (*22->9*) 
	Index{ 20 }, (*23->9*) 
	Index{ 0 }, (*24->0*) 
	Index{ 0 }, (*25->0*) 
	Index{ 0 }, (*26->0*) 
	Index{ 0 }, (*27->0*) 
	Index{ 7 }, (*28->4*) 
	Index{ 7 }, (*29->4*) 
	Index{ 7 }, (*30->4*) 
	Index{ 9 }, (*31->5*) 
	Index{ 9 }, (*32->5*) 
	Index{ 9 }, (*33->5*) 
	Index{ 20 }, (*34->9*) 
	Index{ 20 }, (*35->9*) 
	Index{ 20 }, (*36->9*) 
	Index{ 11 }, (*37->6*) 
	Index{ 14 }, (*38->7*) 
	Index{ 17 }, (*39->8*) 
	Index{ 20 }, (*40->9*) 
	Index{ 2 }, (*41->1*) 
	Index{ 2 }, (*42->1*) 
	Index{ 5 }, (*43->3*) 
	Index{ 20 }, (*44->9*) 
	Index{ 2 }, (*45->1*) 
	Index{ 20 }, (*46->9*) 
	Index{ 0 }, (*47->0*) 
	Index{ 2 }, (*48->1*) 
	Index{ 2 }, (*49->1*) 
	Index{ 2 }, (*50->1*) 
	Index{ 22 }, (*51->10*) 
	Index{ 25 }, (*52->11*) 
	Index{ 28 }, (*53->12*) 
	Index{ 30 }, (*54->13*) 
	Index{ 33 }, (*55->14*) 
	Index{ 5 }, (*56->3*) 
	Index{ 36 }, (*57->15*) 
	Index{ 40 }, (*58->16*) 
	Index{ 44 }, (*59->17*) 
	Index{ 22 }, (*60->10*) 
	Index{ 28 }, (*61->12*) 
	Index{ 30 }, (*62->13*) 
	Index{ 5 }, (*63->3*) 
	Index{ 36 }, (*64->15*) 
	Index{ 44 }, (*65->17*) 
	Index{ 28 }, (*66->12*) 
	Index{ 5 }, (*67->3*) 
	Index{ 44 }, (*68->17*) 
	Index{ 28 }, (*69->12*) 
	Index{ 5 }, (*70->3*) 
	Index{ 44 }, (*71->17*) 
	Index{ 22 }, (*72->10*) 
	Index{ 25 }, (*73->11*) 
	Index{ 30 }, (*74->13*) 
	Index{ 33 }, (*75->14*) 
	Index{ 36 }, (*76->15*) 
	Index{ 40 }, (*77->16*) 
	Index{ 28 }, (*78->12*) 
	Index{ 28 }, (*79->12*) 
	Index{ 5 }, (*80->3*) 
	Index{ 44 }, (*81->17*) 
	Index{ 22 }, (*82->10*) 
	Index{ 5 }, (*83->3*) 
	Index{ 44 }, (*84->17*) 
	Index{ 22 }, (*85->10*) 
	Index{ 25 }, (*86->11*) 
	Index{ 47 }, (*87->18*) 
	Index{ 50 }, (*88->19*) 
	Index{ 47 }, (*89->18*) 
	Index{ 50 }, (*90->19*) 
	Index{ 47 }, (*91->18*) 
	Index{ 50 }, (*92->19*) 
	Index{ 47 }, (*93->18*) 
	Index{ 50 }, (*94->19*) 
	Index{ 5 }, (*95->3*) 
	Index{ 0 }, (*96->0*) 
	Index{ 47 }, (*97->18*) 
	Index{ 47 }, (*98->18*) 
	Index{ 47 }, (*99->18*) 
	Index{ 47 }, (*100->18*) 
	Index{ 47 }, (*101->18*) 
	Index{ 47 }, (*102->18*) 
	Index{ 47 }, (*103->18*) 
	Index{ 47 }, (*104->18*) 
	Index{ 28 }, (*105->12*) 
	Index{ 28 }, (*106->12*) 
	Index{ 28 }, (*107->12*) 
	Index{ 5 }, (*108->3*) 
	Index{ 5 }, (*109->3*) 
	Index{ 5 }, (*110->3*) 
	Index{ 28 }, (*111->12*) 
	Index{ 28 }, (*112->12*) 
	Index{ 28 }, (*113->12*) 
	Index{ 28 }, (*114->12*) 
	Index{ 28 }, (*115->12*) 
	Index{ 53 }, (*116->20*) 
	Index{ 56 }, (*117->21*) 
	Index{ 59 }, (*118->22*) 
	Index{ 0 }, (*119->0*) 
	Index{ 53 }, (*120->20*) 
	Index{ 56 }, (*121->21*) 
	Index{ 59 }, (*122->22*) 
	Index{ 0 }, (*123->0*) 
	Index{ 0 }, (*124->0*) 
	Index{ 53 }, (*125->20*) 
	Index{ 53 }, (*126->20*) 
	Index{ 0 }, (*127->0*) 
	Index{ 44 }, (*128->17*) 
	Index{ 63 }, (*129->23*) 
	Index{ 22 }, (*130->10*) 
	Index{ 25 }, (*131->11*) 
	Index{ 30 }, (*132->13*) 
	Index{ 33 }, (*133->14*) 
	Index{ 22 }, (*134->10*) 
	Index{ 25 }, (*135->11*) 
	Index{ 30 }, (*136->13*) 
	Index{ 33 }, (*137->14*) 
	Index{ 0 }, (*138->0*) 
	Index{ 66 }, (*139->24*) 
	Index{ 28 }, (*140->12*) 
	Index{ 68 }, (*141->25*) 
	Index{ 3 }, (*142->2*) 
	Index{ 28 }, (*143->12*) 
	Index{ 68 }, (*144->25*) 
	Index{ 72 }, (*145->26*) 
	Index{ 72 }, (*146->26*) 
	Index{ 3 }, (*147->2*) 
	Index{ 68 }, (*148->25*) 
	Index{ 68 }, (*149->25*) 
	Index{ 2 }, (*150->1*) 
	Index{ 22 }, (*151->10*) 
	Index{ 25 }, (*152->11*) 
	Index{ 30 }, (*153->13*) 
	Index{ 33 }, (*154->14*) 
	Index{ 22 }, (*155->10*) 
	Index{ 25 }, (*156->11*) 
	Index{ 30 }, (*157->13*) 
	Index{ 33 }, (*158->14*) 
	Index{ 0 }, (*159->0*) 
	Index{ 5 }, (*160->3*) 
	Index{ 53 }, (*161->20*) 
	Index{ 0 }, (*162->0*) 
	Index{ 75 }, (*163->27*) 
	Index{ 78 }, (*164->28*) 
	Index{ 0 }, (*165->0*) 
	Index{ 2 }, (*166->1*) 
	Index{ 28 }, (*167->12*) 
	Index{ 81 }, (*168->29*) 
	Index{ 2 }, (*169->1*) 
	Index{ 20 }, (*170->9*) 
	Index{ 5 }, (*171->3*) 
	Index{ 5 }, (*172->3*) 
	Index{ 2 }, (*173->1*) 
	Index{ 2 }, (*174->1*) 
	Index{ 83 }, (*175->30*) 
	Index{ 86 }, (*176->31*) 
	Index{ 89 }, (*177->32*) 
	Index{ 86 }, (*178->31*) 
	Index{ 89 }, (*179->32*) 
	Index{ 86 }, (*180->31*) 
	Index{ 89 }, (*181->32*) 
	Index{ 66 }, (*182->24*) 
	Index{ 66 }, (*183->24*) 
	Index{ 66 }, (*184->24*) 
	Index{ 5 }, (*185->3*) 
	Index{ 44 }, (*186->17*) 
	Index{ 66 }, (*187->24*) 
	Index{ 2 }, (*188->1*) 
	Index{ 66 }, (*189->24*) 
	Index{ 66 }, (*190->24*) 
	Index{ 0 }, (*191->0*) 
	Index{ 5 }, (*192->3*) 
	Index{ 66 }, (*193->24*) 
	Index{ 66 }, (*194->24*) 
	Index{ 5 }, (*195->3*) 
	Index{ 5 }, (*196->3*) 
	Index{ 0 }, (*197->0*) 
	Index{ 0 }, (*198->0*) 
	Index{ 66 }, (*199->24*) 
	Index{ 66 }, (*200->24*) 
	Index{ 28 }, (*201->12*) 
	Index{ 92 }, (*202->33*) 
	Index{ 20 }, (*203->9*) 
	Index{ 5 }, (*204->3*) 
	Index{ 2 }, (*205->1*) 
	Index{ 66 }, (*206->24*) 
	Index{ 20 }, (*207->9*) 
	Index{ 83 }, (*208->30*) 
	Index{ 86 }, (*209->31*) 
	Index{ 89 }, (*210->32*) 
	Index{ 86 }, (*211->31*) 
	Index{ 89 }, (*212->32*) 
	Index{ 86 }, (*213->31*) 
	Index{ 89 }, (*214->32*) 
	Index{ 66 }, (*215->24*) 
	Index{ 66 }, (*216->24*) 
	Index{ 83 }, (*217->30*) 
	Index{ 86 }, (*218->31*) 
	Index{ 89 }, (*219->32*) 
	Index{ 86 }, (*220->31*) 
	Index{ 89 }, (*221->32*) 
	Index{ 86 }, (*222->31*) 
	Index{ 89 }, (*223->32*) 
	Index{ 66 }, (*224->24*) 
	Index{ 5 }, (*225->3*) 
	Index{ 5 }, (*226->3*) 
	Index{ 66 }, (*227->24*) 
	Index{ 28 }, (*228->12*) 
	Index{ 22 }, (*229->10*) 
	Index{ 25 }, (*230->11*) 
	Index{ 30 }, (*231->13*) 
	Index{ 33 }, (*232->14*) 
	Index{ 0 }, (*233->0*) 
	Index{ 5 }, (*234->3*) 
	Index{ 53 }, (*235->20*) 
	Index{ 22 }, (*236->10*) 
	Index{ 25 }, (*237->11*) 
	Index{ 30 }, (*238->13*) 
	Index{ 33 }, (*239->14*) 
	Index{ 0 }, (*240->0*) 
	Index{ 5 }, (*241->3*) 
	Index{ 95 }, (*242->34*) 
	Index{ 99 }, (*243->35*) 
	Index{ 36 }, (*244->15*) 
	Index{ 40 }, (*245->16*) 
	Index{ 103 }, (*246->36*) 
	Index{ 44 }, (*247->17*) 
	Index{ 22 }, (*248->10*) 
	Index{ 25 }, (*249->11*) 
	Index{ 30 }, (*250->13*) 
	Index{ 33 }, (*251->14*) 
	Index{ 22 }, (*252->10*) 
	Index{ 25 }, (*253->11*) 
	Index{ 30 }, (*254->13*) 
	Index{ 33 }, (*255->14*) 
	Index{ 95 }, (*256->34*) 
	Index{ 99 }, (*257->35*) 
	Index{ 36 }, (*258->15*) 
	Index{ 40 }, (*259->16*) 
	Index{ 44 }, (*260->17*) 
	Index{ 28 }, (*261->12*) 
	Index{ 3 }, (*262->2*) 
	Index{ 66 }  (*263->24*) 
};
<* IF ~nodebug THEN*>
TYPE RuleNamesType = ARRAY RuleRange OF ARRAY 64 OF CHAR;
CONST RuleNames *= RuleNamesType {
    'zerorule',
    'rc: reg',
    'rc: o_par',
    'mrc: rc',
    'mrc: mem',
    'based: reg',
    'based: o_par',
    'based: o_par',
    'based: o_add(based,o_par)',
    'based: o_sub(based,o_par)',
    'scaled: o_par',
    'scaled: o_mul(reg,o_par)',
    'scaled: o_shift(reg,o_par)',
    'scaled: o_add(scaled,o_par)',
    'scaled: o_sub(scaled,o_par)',
    'addr: based',
    'addr: o_add(based,based)',
    'addr: scaled',
    'addr: o_add(based,scaled)',
    'addr: o_add(scaled,based)',
    'addr: o_par',
    'addr: o_par',
    'addr: o_add(addr,o_par)',
    'addr: o_sub(addr,o_par)',
    'addr: o_mul(reg,o_par)',
    'reg: o_mul(reg,o_par)',
    'reg: o_shift(reg,o_par)',
    'reg: o_mul(reg,o_par)',
    'reg: based',
    'reg: o_add(based,o_par)',
    'reg: o_sub(based,o_par)',
    'reg: scaled',
    'reg: o_add(scaled,o_par)',
    'reg: o_sub(scaled,o_par)',
    'reg: addr',
    'reg: o_add(addr,o_par)',
    'reg: o_sub(addr,o_par)',
    'reg: o_add(based,based)',
    'reg: o_add(based,scaled)',
    'reg: o_add(scaled,based)',
    'reg: o_loadr(addr)',
    'reg: o_par',
    'reg: o_par',
    'reg: mem',
    'mem: o_loadr(addr)',
    'mem: o_par',
    'local: o_loadr(addr)',
    'local: reg',
    'mem: o_par',
    'reg: o_getpar',
    'local: o_getpar',
    'reg: o_add(reg,mrc)',
    'reg: o_add(mrc,reg)',
    'reg: o_add(mrc,o_par)',
    'local: o_add(mem,rc)',
    'local: o_add(rc,mem)',
    'local: o_add(mem,o_par)',
    'stm: o_storer(addr,o_add(mem,rc))',
    'stm: o_storer(addr,o_add(rc,mem))',
    'stm: o_storer(addr,o_add(mem,o_par))',
    'reg: o_sub(reg,mrc)',
    'reg: o_sub(mrc,o_par)',
    'local: o_sub(mem,rc)',
    'local: o_sub(mem,o_par)',
    'stm: o_storer(addr,o_sub(mem,rc))',
    'stm: o_storer(addr,o_sub(mem,o_par))',
    'reg: o_neg(mrc)',
    'local: o_neg(mem)',
    'stm: o_storer(addr,o_neg(mem))',
    'reg: o_logical(mrc,o_par)',
    'local: o_logical(mem,o_par)',
    'stm: o_storer(addr,o_logical(mem,o_par))',
    'reg: o_logical(reg,mrc)',
    'reg: o_logical(mrc,reg)',
    'local: o_logical(mem,rc)',
    'local: o_logical(rc,mem)',
    'stm: o_storer(addr,o_logical(mem,rc))',
    'stm: o_storer(addr,o_logical(rc,mem))',
    'reg: o_not(mrc)',
    'reg: o_cap(mrc)',
    'local: o_not(mem)',
    'stm: o_storer(addr,o_not(mem))',
    'reg: o_shift(reg,mrc)',
    'local: o_shift(mem,o_par)',
    'stm: o_storer(addr,o_shift(mem,o_par))',
    'reg: o_mul(reg,mrc)',
    'reg: o_mul(mrc,reg)',
    'reg: o_mul(rc,mrc)',
    'reg: o_mul(mrc,rc)',
    'local: o_mul(rc,mrc)',
    'local: o_mul(mrc,rc)',
    'reg: o_mulh(rc,mrc)',
    'reg: o_mulh(mrc,rc)',
    'local: o_mulh(rc,mrc)',
    'local: o_mulh(mrc,rc)',
    'reg: o_mul(mem,o_par)',
    'reg: o_mul(reg,o_par)',
    'reg: o_div(rc,mrc)',
    'local: o_div(rc,mrc)',
    'reg: o_dvd(rc,mrc)',
    'local: o_dvd(rc,mrc)',
    'reg: o_rem(rc,mrc)',
    'local: o_rem(rc,mrc)',
    'reg: o_mod(rc,mrc)',
    'local: o_mod(rc,mrc)',
    'reg: o_sgnext(mrc)',
    'reg: o_val(mrc)',
    'reg: o_hiword(mrc)',
    'mem: o_hiword(mem)',
    'mem: o_cast(mem)',
    'mem: o_val(mem)',
    'reg: o_cast(mrc)',
    'reg: o_call(mrc)',
    'local: o_call(mrc)',
    'reg: o_alloca(mrc)',
    'local: o_alloca(mrc)',
    'reg: o_incl(reg,reg)',
    'local: o_incl(mem,reg)',
    'stm: o_storer(addr,o_incl(mem,reg))',
    'reg: o_incl(o_par,reg)',
    'reg: o_excl(reg,reg)',
    'local: o_excl(mem,reg)',
    'stm: o_storer(addr,o_excl(mem,reg))',
    'reg: o_excl(o_par,reg)',
    'reg: o_loset(reg)',
    'reg: o_logical(reg,o_loset(reg))',
    'reg: o_logical(o_loset(reg),reg)',
    'reg: o_hiset(reg)',
    'stm: o_storer(addr,mem)',
    'stm: o_storer(addr,rc)',
    'stm: o_checklo(reg,mrc)',
    'stm: o_checklo(mrc,reg)',
    'stm: o_checklo(mem,rc)',
    'stm: o_checklo(rc,mem)',
    'stm: o_checkhi(reg,mrc)',
    'stm: o_checkhi(mrc,reg)',
    'stm: o_checkhi(mem,rc)',
    'stm: o_checkhi(rc,mem)',
    'stm: o_checknil(reg)',
    'stm: o_checknil(tos)',
    'stm: o_putpar(mrc)',
    'stm: o_error(o_comma(rc,rc),rc)',
    'stm: o_stop(rc)',
    'stm: o_call(mrc)',
    'stm: o_copy(o_comma(rc,rc),rc)',
    'stm: o_copy(o_comma(o_par,rc),rc)',
    'stm: o_copy(o_comma(rc,o_par),rc)',
    'stm: o_copy(o_comma(o_par,o_par),rc)',
    'stm: o_copy(o_comma(rc,rc),o_mul(rc,o_par))',
    'stm: o_copy(o_comma(rc,rc),o_mul(o_par,rc))',
    'stm: o_goto',
    'stm: o_le(reg,mrc)',
    'stm: o_le(mrc,reg)',
    'stm: o_le(mem,rc)',
    'stm: o_le(rc,mem)',
    'stm: o_eq(reg,mrc)',
    'stm: o_eq(mrc,reg)',
    'stm: o_eq(mem,rc)',
    'stm: o_eq(rc,mem)',
    'stm: o_eq(o_logical(reg,o_par),o_par)',
    'stm: o_eq(o_logical(mem,o_par),o_par)',
    'stm: o_in(reg,reg)',
    'stm: o_in(reg,o_par)',
    'stm: o_in(reg,mem)',
    'stm: o_in(reg,addr)',
    'stm: o_case(reg)',
    'stm: o_ret',
    'stm: o_retfun(mrc)',
    'stm: local',
    'tos: o_par',
    'tos: o_loadr(addr)',
    'tos: mem',
    'tos: mem',
    'tos: o_par',
    'tos: o_par',
    'tos: o_fbin(tos,tos)',
    'tos: o_fbin(tos,mem)',
    'tos: o_fbin(mem,tos)',
    'tos: o_fbin(tos,o_val(mem))',
    'tos: o_fbin(o_val(mem),tos)',
    'tos: o_fbin(tos,o_val(mem))',
    'tos: o_fbin(o_val(mem),tos)',
    'tos: o_fbin(tos,o_par)',
    'tos: o_fbin(o_par,tos)',
    'tos: o_funary(tos)',
    'local: o_funary(mem)',
    'stm: o_storer(addr,o_funary(mem))',
    'local: tos',
    'tos: o_getpar',
    'tos: o_val(tos)',
    'tos: o_cast(tos)',
    'tos: o_val(reg)',
    'tos: o_val(mem)',
    'reg: o_val(tos)',
    'local: o_val(tos)',
    'reg: o_val(mem)',
    'local: o_val(mem)',
    'tos: reg',
    'tos: o_cast(reg)',
    'reg: tos',
    'reg: o_cast(tos)',
    'tos: o_call(mrc)',
    'stm: o_storer(addr,tos)',
    'stm: o_storer(addr,o_par)',
    'stm: o_putpar(mem)',
    'stm: o_putpar(o_par)',
    'stm: o_putpar(tos)',
    'stm: o_putpar(addr)',
    'stm: o_fle(tos,tos)',
    'stm: o_fle(tos,mem)',
    'stm: o_fle(mem,tos)',
    'stm: o_fle(tos,o_val(mem))',
    'stm: o_fle(o_val(mem),tos)',
    'stm: o_fle(tos,o_val(mem))',
    'stm: o_fle(o_val(mem),tos)',
    'stm: o_fle(tos,o_par)',
    'stm: o_fle(o_par,tos)',
    'stm: o_feq(tos,tos)',
    'stm: o_feq(tos,mem)',
    'stm: o_feq(mem,tos)',
    'stm: o_feq(tos,o_val(mem))',
    'stm: o_feq(o_val(mem),tos)',
    'stm: o_feq(tos,o_val(mem))',
    'stm: o_feq(o_val(mem),tos)',
    'stm: o_feq(tos,o_par)',
    'stm: o_feq(mem,o_par)',
    'stm: o_fle(mem,o_par)',
    'stm: o_retfun(tos)',
    'stm: o_retfun(o_call(mrc))',
    'reg: o_move_eq(reg,mrc)',
    'reg: o_move_eq(mrc,reg)',
    'reg: o_move_eq(mem,rc)',
    'reg: o_move_eq(rc,mem)',
    'reg: o_move_eq(o_logical(reg,o_par),o_par)',
    'reg: o_move_eq(o_logical(mem,o_par),o_par)',
    'reg: o_move_eq(o_logical(o_incl(o_par,reg),reg),o_par)',
    'local: o_move_eq(reg,mrc)',
    'local: o_move_eq(mrc,reg)',
    'local: o_move_eq(mem,rc)',
    'local: o_move_eq(rc,mem)',
    'local: o_move_eq(o_logical(reg,o_par),o_par)',
    'local: o_move_eq(o_logical(mem,o_par),o_par)',
    'stm: o_storer(addr,o_move_eq(reg,mrc))',
    'stm: o_storer(addr,o_move_eq(mrc,reg))',
    'stm: o_storer(addr,o_move_eq(mem,rc))',
    'stm: o_storer(addr,o_move_eq(rc,mem))',
    'stm: o_storer(addr,o_move_eq(o_logical(reg,o_par),o_par))',
    'stm: o_storer(addr,o_move_eq(o_logical(mem,o_par),o_par))',
    'reg: o_move_le(reg,mrc)',
    'reg: o_move_le(mrc,reg)',
    'reg: o_move_le(mem,rc)',
    'reg: o_move_le(rc,mem)',
    'local: o_move_le(reg,mrc)',
    'local: o_move_le(mrc,reg)',
    'local: o_move_le(mem,rc)',
    'local: o_move_le(rc,mem)',
    'stm: o_storer(addr,o_move_le(reg,mrc))',
    'stm: o_storer(addr,o_move_le(mrc,reg))',
    'stm: o_storer(addr,o_move_le(mem,rc))',
    'stm: o_storer(addr,o_move_le(rc,mem))',
    'stm: o_storer(addr,mem)',
    'reg: o_assign(mrc)',
    'local: o_assign(rc)',
    'tos: o_assign(tos)'
    };
<* END *>
PROCEDURE NTkids* (n: RD.DAGNODE; eruleno: BurgNT.Rule;
		VAR kids: ARRAY OF RD.DAGNODE);
BEGIN
    CASE eruleno OF
	|BurgNT.Rule{ 199 }	(* reg: tos *) ,
	 BurgNT.Rule{ 197 }	(* tos: reg *),
	 BurgNT.Rule{ 187 }	(* local: tos *),
	 BurgNT.Rule{ 172 }	(* tos: mem *),
	 BurgNT.Rule{ 171 }	(* tos: mem *),
	 BurgNT.Rule{ 168 }	(* stm: local *),
	 BurgNT.Rule{ 47 }	(* local: reg *),
	 BurgNT.Rule{ 43 }	(* reg: mem *),
	 BurgNT.Rule{ 34 }	(* reg: addr *),
	 BurgNT.Rule{ 31 }	(* reg: scaled *),
	 BurgNT.Rule{ 28 }	(* reg: based *),
	 BurgNT.Rule{ 17 }	(* addr: scaled *),
	 BurgNT.Rule{ 15 }	(* addr: based *),
	 BurgNT.Rule{ 5 }	(* based: reg *),
	 BurgNT.Rule{ 4 }	(* mrc: mem *),
	 BurgNT.Rule{ 3 }	(* mrc: rc *),
	 BurgNT.Rule{ 1 }	(* rc: reg *):
		kids[0] := n;

	|BurgNT.Rule{ 205 }	(* stm: o_putpar(o_par) *) ,
	 BurgNT.Rule{ 188 }	(* tos: o_getpar *),
	 BurgNT.Rule{ 174 }	(* tos: o_par *),
	 BurgNT.Rule{ 173 }	(* tos: o_par *),
	 BurgNT.Rule{ 169 }	(* tos: o_par *),
	 BurgNT.Rule{ 166 }	(* stm: o_ret *),
	 BurgNT.Rule{ 150 }	(* stm: o_goto *),
	 BurgNT.Rule{ 50 }	(* local: o_getpar *),
	 BurgNT.Rule{ 49 }	(* reg: o_getpar *),
	 BurgNT.Rule{ 48 }	(* mem: o_par *),
	 BurgNT.Rule{ 45 }	(* mem: o_par *),
	 BurgNT.Rule{ 42 }	(* reg: o_par *),
	 BurgNT.Rule{ 41 }	(* reg: o_par *),
	 BurgNT.Rule{ 21 }	(* addr: o_par *),
	 BurgNT.Rule{ 20 }	(* addr: o_par *),
	 BurgNT.Rule{ 10 }	(* scaled: o_par *),
	 BurgNT.Rule{ 7 }	(* based: o_par *),
	 BurgNT.Rule{ 6 }	(* based: o_par *),
	 BurgNT.Rule{ 2 }	(* rc: o_par *):

	|BurgNT.Rule{ 263 }	(* tos: o_assign(tos) *) ,
	 BurgNT.Rule{ 262 }	(* local: o_assign(rc) *),
	 BurgNT.Rule{ 261 }	(* reg: o_assign(mrc) *),
	 BurgNT.Rule{ 227 }	(* stm: o_retfun(tos) *),
	 BurgNT.Rule{ 226 }	(* stm: o_fle(mem,o_par) *),
	 BurgNT.Rule{ 225 }	(* stm: o_feq(mem,o_par) *),
	 BurgNT.Rule{ 224 }	(* stm: o_feq(tos,o_par) *),
	 BurgNT.Rule{ 215 }	(* stm: o_fle(tos,o_par) *),
	 BurgNT.Rule{ 207 }	(* stm: o_putpar(addr) *),
	 BurgNT.Rule{ 206 }	(* stm: o_putpar(tos) *),
	 BurgNT.Rule{ 204 }	(* stm: o_putpar(mem) *),
	 BurgNT.Rule{ 203 }	(* stm: o_storer(addr,o_par) *),
	 BurgNT.Rule{ 201 }	(* tos: o_call(mrc) *),
	 BurgNT.Rule{ 200 }	(* reg: o_cast(tos) *),
	 BurgNT.Rule{ 198 }	(* tos: o_cast(reg) *),
	 BurgNT.Rule{ 196 }	(* local: o_val(mem) *),
	 BurgNT.Rule{ 195 }	(* reg: o_val(mem) *),
	 BurgNT.Rule{ 194 }	(* local: o_val(tos) *),
	 BurgNT.Rule{ 193 }	(* reg: o_val(tos) *),
	 BurgNT.Rule{ 192 }	(* tos: o_val(mem) *),
	 BurgNT.Rule{ 191 }	(* tos: o_val(reg) *),
	 BurgNT.Rule{ 190 }	(* tos: o_cast(tos) *),
	 BurgNT.Rule{ 189 }	(* tos: o_val(tos) *),
	 BurgNT.Rule{ 185 }	(* local: o_funary(mem) *),
	 BurgNT.Rule{ 184 }	(* tos: o_funary(tos) *),
	 BurgNT.Rule{ 182 }	(* tos: o_fbin(tos,o_par) *),
	 BurgNT.Rule{ 170 }	(* tos: o_loadr(addr) *),
	 BurgNT.Rule{ 167 }	(* stm: o_retfun(mrc) *),
	 BurgNT.Rule{ 165 }	(* stm: o_case(reg) *),
	 BurgNT.Rule{ 162 }	(* stm: o_in(reg,o_par) *),
	 BurgNT.Rule{ 143 }	(* stm: o_call(mrc) *),
	 BurgNT.Rule{ 142 }	(* stm: o_stop(rc) *),
	 BurgNT.Rule{ 140 }	(* stm: o_putpar(mrc) *),
	 BurgNT.Rule{ 139 }	(* stm: o_checknil(tos) *),
	 BurgNT.Rule{ 138 }	(* stm: o_checknil(reg) *),
	 BurgNT.Rule{ 127 }	(* reg: o_hiset(reg) *),
	 BurgNT.Rule{ 124 }	(* reg: o_loset(reg) *),
	 BurgNT.Rule{ 115 }	(* local: o_alloca(mrc) *),
	 BurgNT.Rule{ 114 }	(* reg: o_alloca(mrc) *),
	 BurgNT.Rule{ 113 }	(* local: o_call(mrc) *),
	 BurgNT.Rule{ 112 }	(* reg: o_call(mrc) *),
	 BurgNT.Rule{ 111 }	(* reg: o_cast(mrc) *),
	 BurgNT.Rule{ 110 }	(* mem: o_val(mem) *),
	 BurgNT.Rule{ 109 }	(* mem: o_cast(mem) *),
	 BurgNT.Rule{ 108 }	(* mem: o_hiword(mem) *),
	 BurgNT.Rule{ 107 }	(* reg: o_hiword(mrc) *),
	 BurgNT.Rule{ 106 }	(* reg: o_val(mrc) *),
	 BurgNT.Rule{ 105 }	(* reg: o_sgnext(mrc) *),
	 BurgNT.Rule{ 96 }	(* reg: o_mul(reg,o_par) *),
	 BurgNT.Rule{ 95 }	(* reg: o_mul(mem,o_par) *),
	 BurgNT.Rule{ 83 }	(* local: o_shift(mem,o_par) *),
	 BurgNT.Rule{ 80 }	(* local: o_not(mem) *),
	 BurgNT.Rule{ 79 }	(* reg: o_cap(mrc) *),
	 BurgNT.Rule{ 78 }	(* reg: o_not(mrc) *),
	 BurgNT.Rule{ 70 }	(* local: o_logical(mem,o_par) *),
	 BurgNT.Rule{ 69 }	(* reg: o_logical(mrc,o_par) *),
	 BurgNT.Rule{ 67 }	(* local: o_neg(mem) *),
	 BurgNT.Rule{ 66 }	(* reg: o_neg(mrc) *),
	 BurgNT.Rule{ 63 }	(* local: o_sub(mem,o_par) *),
	 BurgNT.Rule{ 61 }	(* reg: o_sub(mrc,o_par) *),
	 BurgNT.Rule{ 56 }	(* local: o_add(mem,o_par) *),
	 BurgNT.Rule{ 53 }	(* reg: o_add(mrc,o_par) *),
	 BurgNT.Rule{ 46 }	(* local: o_loadr(addr) *),
	 BurgNT.Rule{ 44 }	(* mem: o_loadr(addr) *),
	 BurgNT.Rule{ 40 }	(* reg: o_loadr(addr) *),
	 BurgNT.Rule{ 36 }	(* reg: o_sub(addr,o_par) *),
	 BurgNT.Rule{ 35 }	(* reg: o_add(addr,o_par) *),
	 BurgNT.Rule{ 33 }	(* reg: o_sub(scaled,o_par) *),
	 BurgNT.Rule{ 32 }	(* reg: o_add(scaled,o_par) *),
	 BurgNT.Rule{ 30 }	(* reg: o_sub(based,o_par) *),
	 BurgNT.Rule{ 29 }	(* reg: o_add(based,o_par) *),
	 BurgNT.Rule{ 27 }	(* reg: o_mul(reg,o_par) *),
	 BurgNT.Rule{ 26 }	(* reg: o_shift(reg,o_par) *),
	 BurgNT.Rule{ 25 }	(* reg: o_mul(reg,o_par) *),
	 BurgNT.Rule{ 24 }	(* addr: o_mul(reg,o_par) *),
	 BurgNT.Rule{ 23 }	(* addr: o_sub(addr,o_par) *),
	 BurgNT.Rule{ 22 }	(* addr: o_add(addr,o_par) *),
	 BurgNT.Rule{ 14 }	(* scaled: o_sub(scaled,o_par) *),
	 BurgNT.Rule{ 13 }	(* scaled: o_add(scaled,o_par) *),
	 BurgNT.Rule{ 12 }	(* scaled: o_shift(reg,o_par) *),
	 BurgNT.Rule{ 11 }	(* scaled: o_mul(reg,o_par) *),
	 BurgNT.Rule{ 9 }	(* based: o_sub(based,o_par) *),
	 BurgNT.Rule{ 8 }	(* based: o_add(based,o_par) *):
		kids[0] := n.l;

	|BurgNT.Rule{ 260 }	(* stm: o_storer(addr,mem) *) ,
	 BurgNT.Rule{ 255 }	(* local: o_move_le(rc,mem) *),
	 BurgNT.Rule{ 254 }	(* local: o_move_le(mem,rc) *),
	 BurgNT.Rule{ 253 }	(* local: o_move_le(mrc,reg) *),
	 BurgNT.Rule{ 252 }	(* local: o_move_le(reg,mrc) *),
	 BurgNT.Rule{ 251 }	(* reg: o_move_le(rc,mem) *),
	 BurgNT.Rule{ 250 }	(* reg: o_move_le(mem,rc) *),
	 BurgNT.Rule{ 249 }	(* reg: o_move_le(mrc,reg) *),
	 BurgNT.Rule{ 248 }	(* reg: o_move_le(reg,mrc) *),
	 BurgNT.Rule{ 239 }	(* local: o_move_eq(rc,mem) *),
	 BurgNT.Rule{ 238 }	(* local: o_move_eq(mem,rc) *),
	 BurgNT.Rule{ 237 }	(* local: o_move_eq(mrc,reg) *),
	 BurgNT.Rule{ 236 }	(* local: o_move_eq(reg,mrc) *),
	 BurgNT.Rule{ 232 }	(* reg: o_move_eq(rc,mem) *),
	 BurgNT.Rule{ 231 }	(* reg: o_move_eq(mem,rc) *),
	 BurgNT.Rule{ 230 }	(* reg: o_move_eq(mrc,reg) *),
	 BurgNT.Rule{ 229 }	(* reg: o_move_eq(reg,mrc) *),
	 BurgNT.Rule{ 219 }	(* stm: o_feq(mem,tos) *),
	 BurgNT.Rule{ 218 }	(* stm: o_feq(tos,mem) *),
	 BurgNT.Rule{ 217 }	(* stm: o_feq(tos,tos) *),
	 BurgNT.Rule{ 210 }	(* stm: o_fle(mem,tos) *),
	 BurgNT.Rule{ 209 }	(* stm: o_fle(tos,mem) *),
	 BurgNT.Rule{ 208 }	(* stm: o_fle(tos,tos) *),
	 BurgNT.Rule{ 202 }	(* stm: o_storer(addr,tos) *),
	 BurgNT.Rule{ 177 }	(* tos: o_fbin(mem,tos) *),
	 BurgNT.Rule{ 176 }	(* tos: o_fbin(tos,mem) *),
	 BurgNT.Rule{ 175 }	(* tos: o_fbin(tos,tos) *),
	 BurgNT.Rule{ 164 }	(* stm: o_in(reg,addr) *),
	 BurgNT.Rule{ 163 }	(* stm: o_in(reg,mem) *),
	 BurgNT.Rule{ 161 }	(* stm: o_in(reg,reg) *),
	 BurgNT.Rule{ 158 }	(* stm: o_eq(rc,mem) *),
	 BurgNT.Rule{ 157 }	(* stm: o_eq(mem,rc) *),
	 BurgNT.Rule{ 156 }	(* stm: o_eq(mrc,reg) *),
	 BurgNT.Rule{ 155 }	(* stm: o_eq(reg,mrc) *),
	 BurgNT.Rule{ 154 }	(* stm: o_le(rc,mem) *),
	 BurgNT.Rule{ 153 }	(* stm: o_le(mem,rc) *),
	 BurgNT.Rule{ 152 }	(* stm: o_le(mrc,reg) *),
	 BurgNT.Rule{ 151 }	(* stm: o_le(reg,mrc) *),
	 BurgNT.Rule{ 137 }	(* stm: o_checkhi(rc,mem) *),
	 BurgNT.Rule{ 136 }	(* stm: o_checkhi(mem,rc) *),
	 BurgNT.Rule{ 135 }	(* stm: o_checkhi(mrc,reg) *),
	 BurgNT.Rule{ 134 }	(* stm: o_checkhi(reg,mrc) *),
	 BurgNT.Rule{ 133 }	(* stm: o_checklo(rc,mem) *),
	 BurgNT.Rule{ 132 }	(* stm: o_checklo(mem,rc) *),
	 BurgNT.Rule{ 131 }	(* stm: o_checklo(mrc,reg) *),
	 BurgNT.Rule{ 130 }	(* stm: o_checklo(reg,mrc) *),
	 BurgNT.Rule{ 129 }	(* stm: o_storer(addr,rc) *),
	 BurgNT.Rule{ 128 }	(* stm: o_storer(addr,mem) *),
	 BurgNT.Rule{ 121 }	(* local: o_excl(mem,reg) *),
	 BurgNT.Rule{ 120 }	(* reg: o_excl(reg,reg) *),
	 BurgNT.Rule{ 117 }	(* local: o_incl(mem,reg) *),
	 BurgNT.Rule{ 116 }	(* reg: o_incl(reg,reg) *),
	 BurgNT.Rule{ 104 }	(* local: o_mod(rc,mrc) *),
	 BurgNT.Rule{ 103 }	(* reg: o_mod(rc,mrc) *),
	 BurgNT.Rule{ 102 }	(* local: o_rem(rc,mrc) *),
	 BurgNT.Rule{ 101 }	(* reg: o_rem(rc,mrc) *),
	 BurgNT.Rule{ 100 }	(* local: o_dvd(rc,mrc) *),
	 BurgNT.Rule{ 99 }	(* reg: o_dvd(rc,mrc) *),
	 BurgNT.Rule{ 98 }	(* local: o_div(rc,mrc) *),
	 BurgNT.Rule{ 97 }	(* reg: o_div(rc,mrc) *),
	 BurgNT.Rule{ 94 }	(* local: o_mulh(mrc,rc) *),
	 BurgNT.Rule{ 93 }	(* local: o_mulh(rc,mrc) *),
	 BurgNT.Rule{ 92 }	(* reg: o_mulh(mrc,rc) *),
	 BurgNT.Rule{ 91 }	(* reg: o_mulh(rc,mrc) *),
	 BurgNT.Rule{ 90 }	(* local: o_mul(mrc,rc) *),
	 BurgNT.Rule{ 89 }	(* local: o_mul(rc,mrc) *),
	 BurgNT.Rule{ 88 }	(* reg: o_mul(mrc,rc) *),
	 BurgNT.Rule{ 87 }	(* reg: o_mul(rc,mrc) *),
	 BurgNT.Rule{ 86 }	(* reg: o_mul(mrc,reg) *),
	 BurgNT.Rule{ 85 }	(* reg: o_mul(reg,mrc) *),
	 BurgNT.Rule{ 82 }	(* reg: o_shift(reg,mrc) *),
	 BurgNT.Rule{ 75 }	(* local: o_logical(rc,mem) *),
	 BurgNT.Rule{ 74 }	(* local: o_logical(mem,rc) *),
	 BurgNT.Rule{ 73 }	(* reg: o_logical(mrc,reg) *),
	 BurgNT.Rule{ 72 }	(* reg: o_logical(reg,mrc) *),
	 BurgNT.Rule{ 62 }	(* local: o_sub(mem,rc) *),
	 BurgNT.Rule{ 60 }	(* reg: o_sub(reg,mrc) *),
	 BurgNT.Rule{ 55 }	(* local: o_add(rc,mem) *),
	 BurgNT.Rule{ 54 }	(* local: o_add(mem,rc) *),
	 BurgNT.Rule{ 52 }	(* reg: o_add(mrc,reg) *),
	 BurgNT.Rule{ 51 }	(* reg: o_add(reg,mrc) *),
	 BurgNT.Rule{ 39 }	(* reg: o_add(scaled,based) *),
	 BurgNT.Rule{ 38 }	(* reg: o_add(based,scaled) *),
	 BurgNT.Rule{ 37 }	(* reg: o_add(based,based) *),
	 BurgNT.Rule{ 19 }	(* addr: o_add(scaled,based) *),
	 BurgNT.Rule{ 18 }	(* addr: o_add(based,scaled) *),
	 BurgNT.Rule{ 16 }	(* addr: o_add(based,based) *):
		kids[0] := n.l;
		kids[1] := n.r;

	|BurgNT.Rule{ 259 }	(* stm: o_storer(addr,o_move_le(rc,mem)) *) ,
	 BurgNT.Rule{ 258 }	(* stm: o_storer(addr,o_move_le(mem,rc)) *),
	 BurgNT.Rule{ 257 }	(* stm: o_storer(addr,o_move_le(mrc,reg)) *),
	 BurgNT.Rule{ 256 }	(* stm: o_storer(addr,o_move_le(reg,mrc)) *),
	 BurgNT.Rule{ 245 }	(* stm: o_storer(addr,o_move_eq(rc,mem)) *),
	 BurgNT.Rule{ 244 }	(* stm: o_storer(addr,o_move_eq(mem,rc)) *),
	 BurgNT.Rule{ 243 }	(* stm: o_storer(addr,o_move_eq(mrc,reg)) *),
	 BurgNT.Rule{ 242 }	(* stm: o_storer(addr,o_move_eq(reg,mrc)) *),
	 BurgNT.Rule{ 122 }	(* stm: o_storer(addr,o_excl(mem,reg)) *),
	 BurgNT.Rule{ 118 }	(* stm: o_storer(addr,o_incl(mem,reg)) *),
	 BurgNT.Rule{ 77 }	(* stm: o_storer(addr,o_logical(rc,mem)) *),
	 BurgNT.Rule{ 76 }	(* stm: o_storer(addr,o_logical(mem,rc)) *),
	 BurgNT.Rule{ 64 }	(* stm: o_storer(addr,o_sub(mem,rc)) *),
	 BurgNT.Rule{ 58 }	(* stm: o_storer(addr,o_add(rc,mem)) *),
	 BurgNT.Rule{ 57 }	(* stm: o_storer(addr,o_add(mem,rc)) *):
		kids[0] := n.l;
		kids[1] := n.r.l;
		kids[2] := n.r.r;

	|BurgNT.Rule{ 222 }	(* stm: o_feq(tos,o_val(mem)) *) ,
	 BurgNT.Rule{ 220 }	(* stm: o_feq(tos,o_val(mem)) *),
	 BurgNT.Rule{ 213 }	(* stm: o_fle(tos,o_val(mem)) *),
	 BurgNT.Rule{ 211 }	(* stm: o_fle(tos,o_val(mem)) *),
	 BurgNT.Rule{ 186 }	(* stm: o_storer(addr,o_funary(mem)) *),
	 BurgNT.Rule{ 180 }	(* tos: o_fbin(tos,o_val(mem)) *),
	 BurgNT.Rule{ 178 }	(* tos: o_fbin(tos,o_val(mem)) *),
	 BurgNT.Rule{ 125 }	(* reg: o_logical(reg,o_loset(reg)) *),
	 BurgNT.Rule{ 84 }	(* stm: o_storer(addr,o_shift(mem,o_par)) *),
	 BurgNT.Rule{ 81 }	(* stm: o_storer(addr,o_not(mem)) *),
	 BurgNT.Rule{ 71 }	(* stm: o_storer(addr,o_logical(mem,o_par)) *),
	 BurgNT.Rule{ 68 }	(* stm: o_storer(addr,o_neg(mem)) *),
	 BurgNT.Rule{ 65 }	(* stm: o_storer(addr,o_sub(mem,o_par)) *),
	 BurgNT.Rule{ 59 }	(* stm: o_storer(addr,o_add(mem,o_par)) *):
		kids[0] := n.l;
		kids[1] := n.r.l;

	|BurgNT.Rule{ 216 }	(* stm: o_fle(o_par,tos) *) ,
	 BurgNT.Rule{ 183 }	(* tos: o_fbin(o_par,tos) *),
	 BurgNT.Rule{ 147 }	(* stm: o_copy(o_comma(o_par,o_par),rc) *),
	 BurgNT.Rule{ 123 }	(* reg: o_excl(o_par,reg) *),
	 BurgNT.Rule{ 119 }	(* reg: o_incl(o_par,reg) *):
		kids[0] := n.r;

	|BurgNT.Rule{ 223 }	(* stm: o_feq(o_val(mem),tos) *) ,
	 BurgNT.Rule{ 221 }	(* stm: o_feq(o_val(mem),tos) *),
	 BurgNT.Rule{ 214 }	(* stm: o_fle(o_val(mem),tos) *),
	 BurgNT.Rule{ 212 }	(* stm: o_fle(o_val(mem),tos) *),
	 BurgNT.Rule{ 181 }	(* tos: o_fbin(o_val(mem),tos) *),
	 BurgNT.Rule{ 179 }	(* tos: o_fbin(o_val(mem),tos) *),
	 BurgNT.Rule{ 146 }	(* stm: o_copy(o_comma(rc,o_par),rc) *),
	 BurgNT.Rule{ 126 }	(* reg: o_logical(o_loset(reg),reg) *):
		kids[0] := n.l.l;
		kids[1] := n.r;

	|BurgNT.Rule{ 144 }	(* stm: o_copy(o_comma(rc,rc),rc) *) ,
	 BurgNT.Rule{ 141 }	(* stm: o_error(o_comma(rc,rc),rc) *):
		kids[0] := n.l.l;
		kids[1] := n.l.r;
		kids[2] := n.r;

	|BurgNT.Rule{ 145 }	(* stm: o_copy(o_comma(o_par,rc),rc) *) :
		kids[0] := n.l.r;
		kids[1] := n.r;

	|BurgNT.Rule{ 148 }	(* stm: o_copy(o_comma(rc,rc),o_mul(rc,o_par)) *) :
		kids[0] := n.l.l;
		kids[1] := n.l.r;
		kids[2] := n.r.l;

	|BurgNT.Rule{ 149 }	(* stm: o_copy(o_comma(rc,rc),o_mul(o_par,rc)) *) :
		kids[0] := n.l.l;
		kids[1] := n.l.r;
		kids[2] := n.r.r;

	|BurgNT.Rule{ 241 }	(* local: o_move_eq(o_logical(mem,o_par),o_par) *) ,
	 BurgNT.Rule{ 240 }	(* local: o_move_eq(o_logical(reg,o_par),o_par) *),
	 BurgNT.Rule{ 234 }	(* reg: o_move_eq(o_logical(mem,o_par),o_par) *),
	 BurgNT.Rule{ 233 }	(* reg: o_move_eq(o_logical(reg,o_par),o_par) *),
	 BurgNT.Rule{ 228 }	(* stm: o_retfun(o_call(mrc)) *),
	 BurgNT.Rule{ 160 }	(* stm: o_eq(o_logical(mem,o_par),o_par) *),
	 BurgNT.Rule{ 159 }	(* stm: o_eq(o_logical(reg,o_par),o_par) *):
		kids[0] := n.l.l;

	|BurgNT.Rule{ 235 }	(* reg: o_move_eq(o_logical(o_incl(o_par,reg),reg),o_par) *) :
		kids[0] := n.l.l.r;
		kids[1] := n.l.r;

	|BurgNT.Rule{ 247 }	(* stm: o_storer(addr,o_move_eq(o_logical(mem,o_par),o_par)) *) ,
	 BurgNT.Rule{ 246 }	(* stm: o_storer(addr,o_move_eq(o_logical(reg,o_par),o_par)) *):
		kids[0] := n.l;
		kids[1] := n.r.l.l;

    END;
END NTkids;
CONST MAXNKIDS *= 3;

TYPE NTarity_type = ARRAY BurgNT.OpRange OF SHORTINT;
CONST NTarity*= NTarity_type {
	0,	(* 0 *)
	0,	(* 1 *)
	1,	(* 2=o_assign *)
	2,	(* 3=o_copy *)
	1,	(* 4=o_val *)
	1,	(* 5=o_cast *)
	1,	(* 6=o_cap *)
	0,	(* 7 *)
	0,	(* 8 *)
	0,	(* 9 *)
	0,	(* 10 *)
	2,	(* 11=o_add *)
	2,	(* 12=o_mul *)
	2,	(* 13=o_mulh *)
	2,	(* 14=o_div *)
	2,	(* 15=o_dvd *)
	2,	(* 16=o_mod *)
	2,	(* 17=o_rem *)
	0,	(* 18 *)
	0,	(* 19 *)
	0,	(* 20 *)
	0,	(* 21 *)
	0,	(* 22 *)
	1,	(* 23=o_not *)
	2,	(* 24=o_incl *)
	2,	(* 25=o_excl *)
	1,	(* 26=o_loset *)
	1,	(* 27=o_hiset *)
	0,	(* 28 *)
	0,	(* 29 *)
	0,	(* 30 *)
	0,	(* 31 *)
	0,	(* 32 *)
	1,	(* 33=o_sgnext *)
	0,	(* 34 *)
	0,	(* 35 *)
	1,	(* 36=o_call *)
	0,	(* 37=o_ret *)
	1,	(* 38=o_checknil *)
	2,	(* 39=o_checklo *)
	2,	(* 40=o_checkhi *)
	1,	(* 41=o_stop *)
	2,	(* 42=o_error *)
	0,	(* 43 *)
	0,	(* 44 *)
	1,	(* 45=o_loadr *)
	2,	(* 46=o_storer *)
	0,	(* 47 *)
	0,	(* 48 *)
	2,	(* 49=o_eq *)
	2,	(* 50=o_le *)
	0,	(* 51 *)
	2,	(* 52=o_in *)
	0,	(* 53 *)
	1,	(* 54=o_case *)
	0,	(* 55=o_goto *)
	0,	(* 56=o_getpar *)
	2,	(* 57=o_move_eq *)
	2,	(* 58=o_move_le *)
	1,	(* 59=o_alloca *)
	1,	(* 60=o_neg *)
	2,	(* 61=o_sub *)
	1,	(* 62=o_putpar *)
	2,	(* 63=o_comma *)
	0,	(* 64=o_par *)
	1,	(* 65=o_retfun *)
	2,	(* 66=o_shift *)
	2,	(* 67=o_logical *)
	2,	(* 68=o_fbin *)
	1,	(* 69=o_funary *)
	2,	(* 70=o_fle *)
	2,	(* 71=o_feq *)
	0,	(* 72 *)
	0,	(* 73 *)
	0,	(* 74 *)
	0,	(* 75 *)
	0,	(* 76 *)
	0,	(* 77 *)
	0,	(* 78 *)
	0,	(* 79 *)
	0,	(* 80 *)
	0,	(* 81 *)
	0,	(* 82 *)
	0,	(* 83 *)
	1 	(* 84=o_hiword *)
};

END BurgTables.
