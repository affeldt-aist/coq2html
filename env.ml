type t = {
    type_lookup: Type_lookup.conn option;
    definition_blacklist: Index_blacklist.t option;
  }
let default = {
    type_lookup = None;
    definition_blacklist = None;
}
