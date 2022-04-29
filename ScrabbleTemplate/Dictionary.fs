module internal Dictionary

//type internal Dict = D of (Map<char, Dict> * bool)
type Dict =
    | Leaf of bool
    | Node of bool * System.Collections.Generic.Dictionary<char, Dict>

type scDict = System.Collections.Generic.Dictionary<char, Dict>    
let empty () = Leaf false

let rec insert (word:string)  =
    function 
    | Leaf _ when word.Length = 0 -> Leaf true
    | Node (_, scDict) when word.Length = 0 -> Node(true, scDict)
    | Leaf b -> //convert leaf to node
        let newScDict = scDict ()
        let c = word.[0]
        newScDict.[c] <- insert word.[1..] (empty ())
        Node(b, newScDict)
        
    | Node (b, scDict) ->
        let firstCharacter = word.[0]
        match scDict.TryGetValue firstCharacter with
            | (true, scDict') ->
                scDict.[firstCharacter] <- insert word.[1..] scDict'
                Node(b, scDict)
            | (false, _) ->
                scDict.[firstCharacter] <- insert word.[1..] (empty())
                Node(b, scDict)
            

                      
let rec lookup (word:string) (dict: Dict)  = false

let step (c:char) (dict: Dict) =
    match dict with
    | Leaf _ -> None
    | Node (b, dict') ->
        match dict'.TryGetValue c with (*true og false er her C#'s forsøg på en option type*)
        | (true, value) ->
            match value with
            | Leaf b -> Some (b, value)
            | Node (b,_) -> Some(b, value)
        | (false, _ ) -> None