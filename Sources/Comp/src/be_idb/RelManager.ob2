MODULE RelManager;
<* +o2addkwd *>
FROM SYSTEM IMPORT PRED,SUCC;
IMPORT
 ic:=ivConst,
 iv:=IVERAS,
 pc:=pcK,
 ir,
 pcO,
 xfs:=xiFiles,
 env:=xiEnv,
 BitVect,
 str:=Strings,
 as := Attributes,
 at := opAttrs,
 ls := lists;

VAR
  global_e : iv.STRUCT_ENTITY;
  global_toe: iv.STRUCT_ENTITY;
PROCEDURE get_declaration(o : pc.OBJECT): iv.STRUCT_ENTITY;
VAR
  toe : iv.STRUCT_ENTITY;
BEGIN
  IF (o <> NIL) AND
     (o.mno >= pc.ZEROMno) AND(*non-system object*)
     (iv.mods[ o.mno ] <> NIL)  (* was processed by iv *)
                               THEN
      toe := NIL;
      IF (pcO.imp) AND (iv.def_emf <> NIL) THEN
         toe:=iv.def_emf.find_entity(iv.get_ext_name(o));
      END;
      IF toe = NIL THEN
        IF o.mno = env.info.mod_no THEN
          toe := iv.mods[o.mno](iv.EMODULE).get_entity(o.eno);
          IF ls.compare_string_lists(toe.ext_obj_name, iv.get_ext_name(o)) THEN
            RETURN toe;
          END;
        END;
        RETURN iv.mods[o.mno].find_entity( iv.get_ext_name(o) );
      END;
      RETURN toe;
  ELSE
      RETURN NIL;
  END;
END get_declaration;

PROCEDURE find_parent*( em : iv.EMODULE; e : iv.ENTITY; ent_type : SET) : iv.ENTITY;
VAR ent:iv.ENTITY;
BEGIN
  IF e = NIL THEN RETURN NIL;  END;
  ent := e;
  WHILE NOT(ent.type IN ent_type) AND  (ent.parent # ic.no_parent)DO
    ent := em.get_entity(ent.parent);
  END;
  IF ent.type IN ent_type THEN  RETURN ent;
  ELSE  RETURN NIL;  END;
END find_parent;

PROCEDURE IsAncestor(em : iv.EMODULE; anc, desc : iv.STRUCT_ENTITY ):BOOLEAN;
(* TRUE if anc is an ancestor of desc*)
VAR
  e    : iv.STRUCT_ENTITY;
BEGIN
  e := desc;
  WHILE e.nest_lev > anc.nest_lev DO
    e := em.get_entity(e.parent);
  END;
  RETURN (e = anc)
END IsAncestor;

PROCEDURE entity_from_lvalue*(em:iv.EMODULE; n : pc.NODE) : iv.ENTITY;
(*returns et_var, OR et_field if succeeded
 NIL if failed*)
VAR
  e : iv.ENTITY;
BEGIN
  IF n = NIL THEN RETURN NIL; END;
  ASSERT(n.mode IN pc.LVALUEs);
  WHILE (n # NIL) AND (NOT (n.mode IN pc.NODE_SET{pc.nd_field, pc.nd_var} )  ) DO
    n := n.l;
  END;
  e := em.entity_by_node(n);
  RETURN e;
END entity_from_lvalue;

PROCEDURE vardecl_from_lvalue*(em:iv.EMODULE; n : pc.NODE) : iv.STRUCT_ENTITY;
(*returns et_vardecl if succeeded; NIL if failed*)
VAR
  e : iv.STRUCT_ENTITY;
BEGIN
  IF n = NIL THEN RETURN NIL; END;
  ASSERT(n.mode IN pc.LVALUEs);
  WHILE (n # NIL) AND (NOT (n.mode IN pc.NODE_SET{ pc.nd_var} )  ) DO
    n := n.l;
  END;
  IF (n.obj.mno>= pc.ZEROMno) AND (iv.mods[n.obj.mno] <> NIL) THEN
    e := iv.mods[n.obj.mno].find_entity( iv.get_ext_name(n.obj) );
  ELSE
    e := NIL;
  END;
  RETURN e;
END vardecl_from_lvalue;


PROCEDURE add_name(e : iv.ENTITY; name : xfs.String);
VAR
  at : as.ATTR_REC;
BEGIN
  at.type:= ic.at_name;
  at.set_str_val(name^);
  e.attrs.add(at);
END add_name;

PROCEDURE add_modified(em : iv.EMODULE;e : iv.ENTITY);-- this procedure is called for usage
VAR
  at : as.ATTR_REC;
BEGIN
  IF e = NIL THEN RETURN ; END;
  ASSERT(e.type IN {ic.et_varusage, ic.et_fieldusage});
  at.type:= ic.at_modified;
  at.set_bool_val(TRUE);
  e.attrs.add(at);
END add_modified;

PROCEDURE handle_imported_objects(em : iv.EMODULE);
VAR
  at : as.ATTR_REC;
  I  : pc.ImportedObject;
  inx: INTEGER;
  e  : iv.ENTITY;
  toe: iv.STRUCT_ENTITY;
BEGIN
  I:= pc.import.objects;
  WHILE I # NIL DO
    toe := em.find_entity( iv.get_ext_name( I.obj ) );
    e := toe(iv.ENTITY);
    at.type:=ic.at_never_used;
    inx := e.rels.index_of(ic.rt_usage);
    at.set_bool_val( inx = -1 );
    e.attrs.add(at);
    IF (e.obj.mno > pc.ZEROMno) AND (iv.mods[e.obj.mno] # NIL) THEN
      toe := iv.mods[e.obj.mno].find_entity( iv.get_ext_name(e.obj) );
      e.add_rel( ic.rt_impdef , toe);
    END;
    I := I.next;
  END;
END handle_imported_objects;


PROCEDURE find_rels*(em:iv.EMODULE);
 VAR
  i, inx : LONGINT;
  e, ate : iv.ENTITY;
  toe    : iv.STRUCT_ENTITY;
  o      : pc.OBJECT;
  no     : pc.NODE;
  at     : as.ATTR_REC;
  m      : pc.Mno;
 BEGIN
   FOR i:=0 TO em.len()-1 DO
     e:=em.get_entity(i);
     global_e := e;
     toe := NIL;
     CASE  e.type OF
     | ic.et_varusage:
       toe := get_declaration(e.obj);
       global_toe := toe;
       e.add_rel(ic.rt_DUR, toe );

       ate := find_parent(em, e, {ic.et_procdecl});
       IF (ate # NIL) AND (toe <> NIL) AND
         (ate.nest_lev >= toe.nest_lev) AND
         (toe.nest_lev > 2 ) AND (e.obj.mno = env.info.mod_no) THEN
           inx := toe(iv.ENTITY).attrs.index_of( ic.at_nest_used );	
           at.type:=ic.at_nest_used;
           at.set_bool_val( TRUE );
           toe(iv.ENTITY).attrs.set_elem(inx, at);
       END;

     | ic.et_constusage:
       toe := get_declaration(e.obj);
       e.add_rel(ic.rt_DUR, toe );

     | ic.et_fieldusage :
       toe := get_declaration(e.obj);
       e.add_rel(ic.rt_DUR, toe );

       no:=e.node;
       REPEAT
         no:=no.l;
       UNTIL  (no = NIL) OR (no.mode IN pc.NODE_SET{ pc.nd_var, pc.nd_field });
       IF no # NIL THEN
         toe := get_declaration( no.obj );
         e.add_rel(ic.rt_hostvar, toe );
       END;

     | ic.et_deref:
       no:=e.node;
       REPEAT
         no:=no.l;
       UNTIL  (no = NIL) OR (no.mode IN pc.NODE_SET{ pc.nd_var, pc.nd_field, pc.nd_call });
       IF no # NIL THEN
         toe := get_declaration( no.obj );
         e.add_rel(ic.rt_hostvar, toe );
       END;

     | ic.et_index:
       no:=e.node;
       REPEAT
         no:=no.l;
	 IF (no.mode = pc.nd_value) THEN
	   no := no.val.expr;
	 END;
       UNTIL  (no = NIL) OR (no.mode IN pc.NODE_SET{ pc.nd_var, pc.nd_field });
       IF no # NIL THEN
         toe := get_declaration( no.obj );
         e.add_rel(ic.rt_hostvar, toe );
       END;

     | ic.et_proccall,ic.et_methodcall :
       toe := get_declaration(e.obj);
       e.add_rel(ic.rt_DCR, toe );

       IF (toe # NIL ) AND IsAncestor(em, toe, e) THEN (*procdecl is an ancestor of proccall*)
         inx := toe(iv.ENTITY).attrs.index_of( ic.at_self_recursive );	
         at.type:=ic.at_self_recursive;
         at.set_bool_val( TRUE );
         ASSERT (inx > -1);
         toe(iv.ENTITY).attrs.set_elem(inx, at);
       END;

       IF e.type = ic.et_proccall THEN --rt_write
         o := e.obj.type.prof; --formal params (!!!NOT the same with e.struct);
         no:= e.node.r;  --actual params
       ELSE
         o := e.obj.type.prof.next; --formal params (first param is this, so it is skipped);
         no:= e.node.r;  --actual params
         ate := entity_from_lvalue(em, e.node.l.l); --this
         add_modified(em, ate);
         toe := vardecl_from_lvalue(em, e.node.l.l); --this
--         e.add_rel(ic.rt_write, toe );
         IF (toe <> NIL) AND (toe.type = ic.et_paramdecl) AND (toe(iv.ENTITY).obj.mode = pc.ob_var) THEN (*Not val param*)
           ASSERT(toe(iv.ENTITY).obj.mno = env.info.mod_no);
           at.type:=ic.at_modified_vpar;
           at.set_bool_val( TRUE );
           toe(iv.ENTITY).attrs.add(at);
         END;

       END;
       WHILE (o <> NIL) AND (no <> NIL) DO
         IF o.mode = pc.ob_varpar THEN
           ate := entity_from_lvalue(em, no);
           add_modified(em, ate);
           toe := vardecl_from_lvalue(em, no);
           --e.add_rel(ic.rt_write, toe );
           IF (toe <> NIL) AND (toe.type = ic.et_paramdecl) AND (toe(iv.ENTITY).obj.mode = pc.ob_var) THEN (*Not val param*)
             ASSERT(toe(iv.ENTITY).obj.mno = env.info.mod_no);
             at.type:=ic.at_modified_vpar;
             at.set_bool_val( TRUE );
             toe(iv.ENTITY).attrs.add(at);
           END;
         END;
         o := o.next;
         no:= no.next;
       END;

     | ic.et_return :
       toe:= find_parent(em, e, {ic.et_procdecl} );
       toe(iv.ENTITY).add_rel(ic.rt_return, e );

     | ic.et_typedecl :

       IF (e.struct.mode IN pc.TY_SET{pc.ty_record, pc.ty_pointer, pc.ty_array} ) AND(e.struct.base#NIL) THEN
          toe := get_declaration( e.struct.base.obj );
          e.add_rel(ic.rt_BASE, toe );
       END;

       IF (e.struct.mode=pc.ty_record)THEN
         o:=e.struct.mem;
         WHILE o#NIL DO
           toe := get_declaration( o );
           e.add_rel(ic.rt_METHOD, toe );
	       o:=o.next;
         END;
       END;


     | ic.et_procdecl :
       IF e.nest_lev > 1 THEN
         toe:=find_parent( em, e, {ic.et_procdecl} );
         e.add_rel(ic.rt_LOCALPROC, toe );
       END;

      IF (pcO.imp)AND (iv.def_emf <> NIL)  THEN
        toe:=iv.def_emf.find_entity(iv.get_ext_name(e.obj));
        e.add_rel( ic.rt_header, toe );
      END;

      IF e.obj.type.inx <> NIL THEN
        toe := get_declaration(e.obj.type.inx.obj);
        e.add_rel( ic.rt_override, toe );
      END;

       toe := get_declaration(e.obj.type.base.obj);
       e.add_rel( ic.rt_TYPE_OF, toe );


     | ic.et_fielddecl :
 	   toe:=find_parent(em, e, {ic.et_typedecl} );
       ASSERT( toe <> NIL );
       e.add_rel(ic.rt_host, toe );

       toe := get_declaration(e.obj.type.obj);
       e.add_rel( ic.rt_TYPE_OF, toe );

     | ic.et_paramdecl:
       IF     (e.obj.type.mode IN pc.TY_SET{pc.ty_array, pc.ty_record})
          AND (e.obj.mode = pc.ob_var)(*not var param *)
	  AND ~(pc.otag_RO IN e.obj.tags)(*not read only param *) THEN
         at.type:=ic.at_struct_param;
         at.set_bool_val( TRUE );
         e.attrs.add(at);
       END;

       toe := get_declaration(e.obj.type.obj);
       e.add_rel( ic.rt_TYPE_OF, toe );

     | ic.et_vardecl,
       ic.et_constdecl:
       toe := get_declaration(e.obj.type.obj);
       e.add_rel( ic.rt_TYPE_OF, toe );

     | ic.et_exit :
 	   toe:=find_parent(em, e, { ic.et_loop } );
       ASSERT( toe <> NIL );
       toe(iv.ENTITY).add_rel(ic.rt_exit, e );

     | ic.et_assign :
       IF e.node.obj # NIL THEN
         ate := em.get_entity( e.children.get_elem(0) ); --see model2.establish_assign;
       ELSE
         ate := entity_from_lvalue(em, e.node.l);
       END;

       add_modified(em, ate );
       ate := find_parent(em, ate, {ic.et_varusage} );
       IF ( ate <> NIL )THEN --FIXME (must be an ASSERT)
         toe := get_declaration( ate.obj );
--         e.add_rel(ic.rt_write, toe );
         IF (toe <> NIL) AND(toe.type = ic.et_paramdecl) AND (toe(iv.ENTITY).obj.mode = pc.ob_var) THEN (*Not val param*)
            ASSERT(toe(iv.ENTITY).obj.mno = env.info.mod_no);
            at.type:=ic.at_modified_vpar;
            at.set_bool_val( TRUE );
            toe(iv.ENTITY).attrs.add(at);
         END;
       END;


     | ic.et_for:
       ate := em.get_entity( e.children.get_elem(0) ); --see model2.establish_for;

       add_modified(em, ate );
       ate := find_parent(em, ate, {ic.et_varusage});
       IF ( ate <> NIL )THEN --FIXME (must be an ASSERT)
         toe := get_declaration( ate.obj );
--         e.add_rel(ic.rt_write, toe );
         IF (toe <> NIL) AND (toe.type = ic.et_paramdecl) AND (toe(iv.ENTITY).obj.mode = pc.ob_var) THEN (*Not val param*)
           ASSERT(toe(iv.ENTITY).obj.mno = env.info.mod_no);
           at.type:=ic.at_modified_vpar;
           at.set_bool_val( TRUE );
           toe(iv.ENTITY).attrs.add(at);
         END;
       END;


     | ic.et_sproc:
       CASE e.node.mode OF
         | pc.nd_sproc:
           CASE e.node.sub OF
             pc.sp_new,
             pc.sp_dispose,
             pc.sp_incl,
             pc.sp_excl :
              ate := entity_from_lvalue(em, e.node.r  );
              add_modified(em, ate);
 --             toe := vardecl_from_lvalue(em, e.node.r );
 --             e.add_rel( ic.rt_write , toe );	     	
	   ELSE
	   END;
	 | pc.nd_eval: (* INC & DEC (see model2.parse_statement) *)
              ate := entity_from_lvalue(em, e.node.l.l  );
              add_modified(em, ate);
 --             toe := vardecl_from_lvalue(em, e.node.l.l );
 --             e.add_rel( ic.rt_write , toe );	     	

       END;

     ELSE
      (*Nothing*)
     END; --CASE

     IF (e.type IN ic.et_DECLS + {ic.et_imported_object} ) AND
        (e.obj <> NIL) AND
        (e.obj.name <> NIL) THEN
       add_name(e, e.obj.name );
     END;

     IF (e.type IN ic.et_IMPORTABLES )AND (e.obj.mno > pc.ZEROMno )  THEN
        toe := NIL;
        IF env.info.language = pc.flag_m2 THEN
	   toe := em.find_entity( iv.get_ext_name(e.obj) ); (* !!! it is unusual usage of find entity
                                                            because e.obj refers not to em but to other emodule.
                                                            However, it's correct since imported_object.ext_obj_name is
                                                            properly initialized (see establish_imported_object) *)
	END;						

        IF (toe = NIL) THEN (* look for import of the whole module*)
          toe := em.find_entity( iv.get_ext_name(pc.mods[e.obj.mno]) );  (* see comment above*)
        END;
        IF toe # NIL THEN
          ASSERT( toe.type = ic.et_imported_object);
          toe(iv.ENTITY).add_rel(ic.rt_usage, e);
        END;
     END;

     IF (e.type IN {ic.et_fielddecl, ic.et_vardecl, ic.et_paramdecl})
        AND ( (pc.otag_RO IN e.obj.tags) OR (pc.otag_RO_public IN e.obj.tags) ) THEN
        at.type:=ic.at_readonly;
        at.set_bool_val( TRUE );
        e.attrs.add(at);
     END;




   END; --FOR

   e := em.get_entity(0);
   FOR m := pc.Mno{1} TO PRED(pc.mod_cnt) DO
     IF iv.mods[m] # NIL THEN
        e.add_rel(ic.rt_IMPORT,  iv.mods[m].entity_by_id(0) );
     END;	
   END;
   handle_imported_objects(em);
 END find_rels;






BEGIN
 NEW(iv.rel_type_names,ic.rt_count);
 NEW(iv.rel_type_ext_left_names,ic.rt_count);
 NEW(iv.rel_type_ext_right_names,ic.rt_count);

 str.Assign('UseDeclR',iv.rel_type_names[ic.rt_DUR]);
 str.Assign('CallDecR',iv.rel_type_names[ic.rt_DCR]);
 str.Assign('Imports ',iv.rel_type_names[ic.rt_IMPORT]);
 str.Assign('TYPE OF ',iv.rel_type_names[ic.rt_TYPE_OF]);
 str.Assign('BASE    ',iv.rel_type_names[ic.rt_BASE]);
 str.Assign('METHOD  ',iv.rel_type_names[ic.rt_METHOD]);
 str.Assign('LOCALPRO',iv.rel_type_names[ic.rt_LOCALPROC]);
 str.Assign('HOST    ',iv.rel_type_names[ic.rt_host]);
 str.Assign('Exits   ',iv.rel_type_names[ic.rt_exit]);
 str.Assign('Returns ',iv.rel_type_names[ic.rt_return]);
 str.Assign('HOSTVAR ',iv.rel_type_names[ic.rt_hostvar]);
 str.Assign('USAGES  ',iv.rel_type_names[ic.rt_usage]);
 str.Assign('IMPDEF  ',iv.rel_type_names[ic.rt_impdef]);
 str.Assign('prochead',iv.rel_type_names[ic.rt_header]);
 str.Assign('override',iv.rel_type_names[ic.rt_override]);
 str.Assign('deref',iv.rel_type_names[ic.rt_deref]);
 str.Assign('index',iv.rel_type_names[ic.rt_index]);

 str.Assign('Declaration',iv.rel_type_ext_left_names[ic.rt_DUR]);
 str.Assign('Declaration',iv.rel_type_ext_left_names[ic.rt_DCR]);
 str.Assign('Imports',iv.rel_type_ext_left_names[ic.rt_IMPORT]);
 str.Assign('Type definition',iv.rel_type_ext_left_names[ic.rt_TYPE_OF]);
 str.Assign('Base type',iv.rel_type_ext_left_names[ic.rt_BASE]);
 str.Assign('Methods',iv.rel_type_ext_left_names[ic.rt_METHOD]);
 str.Assign('Local procedures',iv.rel_type_ext_left_names[ic.rt_LOCALPROC]);
 str.Assign('Host record',iv.rel_type_ext_left_names[ic.rt_host]);
 str.Assign('Exits',iv.rel_type_ext_left_names[ic.rt_exit]);
 str.Assign('Returns', iv.rel_type_ext_left_names[ic.rt_return]);
 str.Assign('Host variable',iv.rel_type_ext_left_names[ic.rt_hostvar]);
 str.Assign('Usages',iv.rel_type_ext_left_names[ic.rt_usage]);
 str.Assign('Declaration',iv.rel_type_ext_left_names[ic.rt_impdef]);
 str.Assign('Procedure header',iv.rel_type_ext_left_names[ic.rt_header]);
 str.Assign('Overridden method',iv.rel_type_ext_left_names[ic.rt_override]);
 str.Assign('Pointer declaration',iv.rel_type_ext_left_names[ic.rt_deref]);
 str.Assign('Array declaration',iv.rel_type_ext_left_names[ic.rt_index]);

 str.Assign('Usages',iv.rel_type_ext_right_names[ic.rt_DUR]);
 str.Assign('Calls',iv.rel_type_ext_right_names[ic.rt_DCR]);
 str.Assign('Exported to',iv.rel_type_ext_right_names[ic.rt_IMPORT]);
 str.Assign('Objects of this type',iv.rel_type_ext_right_names[ic.rt_TYPE_OF]);
 str.Assign('Extensions',iv.rel_type_ext_right_names[ic.rt_BASE]);
 str.Assign('Class definition',iv.rel_type_ext_right_names[ic.rt_METHOD]);
 str.Assign('Host procedure',iv.rel_type_ext_right_names[ic.rt_LOCALPROC]);
 str.Assign('Fields',iv.rel_type_ext_right_names[ic.rt_host]);
 str.Assign('Loop',iv.rel_type_ext_right_names[ic.rt_exit]);
 str.Assign('Procedure',iv.rel_type_ext_right_names[ic.rt_return]);
 str.Assign('Field usages',iv.rel_type_ext_right_names[ic.rt_hostvar]);
 str.Assign('Where imported',iv.rel_type_ext_right_names[ic.rt_usage]);
 str.Assign('Aliases',iv.rel_type_ext_right_names[ic.rt_impdef]);
 str.Assign('Implementation',iv.rel_type_ext_right_names[ic.rt_header]);
 str.Assign('Overriding methods',iv.rel_type_ext_right_names[ic.rt_override]);
 str.Assign('Dereferenced',iv.rel_type_ext_right_names[ic.rt_deref]);
 str.Assign('Indexed',iv.rel_type_ext_right_names[ic.rt_index]);
END RelManager.