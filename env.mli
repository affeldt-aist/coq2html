type t = {
    type_lookup: Type_lookup.conn option;
    definition_blacklist: Index_blacklist.t option;
  }

val default : t
