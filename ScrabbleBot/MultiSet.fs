module internal MultiSet

    type internal MultiSet<'a> when 'a: comparison = M of Map<'a, uint32>
    
    let empty = M(Map.empty)
    
    let isEmpty (M s) = Map.isEmpty s
    
    let size (M s) = Map.fold(fun (acc) _ v -> acc + v) 0u s
    
    let contains a (M s)  = Map.containsKey a s
    
    let numItems a (M s) =
        match Map.tryFind a s with
        |None -> uint32(0)
        |Some v -> uint32(v)
    
    let add a n (M s) =
        let value = numItems a (M s)
        Map.add a (value + n) s |> M
        
    let addSingle a ms = add a 1u ms
    
    let remove a n (M s) =
        let value = numItems a (M s)
        match value with 
        | value when value < n -> Map.remove a s |> M
        | _ -> M(Map.add a (value-n) s)
        
    let removeSingle a ms = remove a 1u ms
                                   
    let fold f a (M s) = Map.fold(f) a s
    
    let foldBack f (M s) a = Map.foldBack(f) s a