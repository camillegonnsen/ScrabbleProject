
module internal Dictionary 

type internal Dict = D of (Map<char, Dict> * bool)

let empty = fun() -> D(Map.empty, false)

let rec insert (s:string) (D(map,bool)) =
    match s with
    | "" -> D(map,true)
    | s ->
        let index0 = s.Chars 0
        let found = Map.tryFind index0 map
        match found with
        | None ->
            let nDict =  insert s.[1..] (empty ())
            D(Map.add index0 nDict map, bool)

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


let step (c:char) (D(map,_)) =
    match Map.tryFind c map with
    | None -> None
    | Some (D (m', b')) -> Some(b', (D(m',b')))