MODULE Options;

CONST

 magic * = 1C + 0C;

(* Option names *)
  list * = "l";

(* Options *)
  Options * = '-' + list + '=' + ';';

(* Default option values *)
  Values * = '-' + list + '=' + magic + ';';

END Options.
