type t = {
    type_lookup: Type_lookup.conn option;
    definition_blacklist: Index_blacklist.t option;
    repository_root_url: string option;
    directory_mappings: Directory_mappings.t;
    usedby_table: UsedByTable.t;
  }
let default = {
    type_lookup = None;
    definition_blacklist = None;
    repository_root_url = None;
    directory_mappings = Directory_mappings.empty;
    usedby_table = UsedByTable.empty;
}
