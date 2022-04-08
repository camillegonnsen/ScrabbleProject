
module internal Dictionary 

type internal Dict = D of (Map<char, Dict> * bool)

let empty = fun() -> D(Map.empty, false)

let rec insert (s:string) (D(c,d)) =
    match s with
    | "" -> D(c,true)
    | s ->
        let index0 = s.Chars 0
        let found = Map.tryFind index0 c
        match found with
        | None ->
            let hgf =  insert s.[1..] (empty ())
            D(Map.add index0 hgf c, d)

        | Some (D(map, bool) as mappet)->
            let hgf =  insert s.[1..] mappet
            D(Map.add index0 hgf map, bool)

let rec lookup (s:string) (D (map,bool)) =
    match s with
    | "" -> bool
    | s ->
        let key = s.[0]
        let finding =  Map.tryFind key map
        match finding with
            | None -> false
            | Some value -> lookup s.[1..] value


let step (c:char) (D(map,bool)) =
    let value = Map.tryFind c map
    match value with
    | None -> false
    | Some value ->