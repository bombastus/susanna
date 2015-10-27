
val get    : ?headers:(string * string) list -> string -> int * string
val delete : ?headers:(string * string) list -> string -> int * string
val post   : ?headers:(string * string) list -> url:string -> string -> int * string
val put    : ?headers:(string * string) list -> url:string -> string -> int * string

