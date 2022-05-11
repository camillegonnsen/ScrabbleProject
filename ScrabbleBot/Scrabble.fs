namespace pebbernuts

open ScrabbleUtil
open ScrabbleUtil.ServerCommunication

open System.IO

open ScrabbleUtil.DebugPrint

// The RegEx module is only used to parse human input. It is not used for the final product.

module RegEx =
    open System.Text.RegularExpressions

    let (|Regex|_|) pattern input =
        let m = Regex.Match(input, pattern)
        if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
        else None

    let parseMove ts =
        let pattern = @"([-]?[0-9]+[ ])([-]?[0-9]+[ ])([0-9]+)([A-Z]{1})([0-9]+)[ ]?" 
        Regex.Matches(ts, pattern) |>
        Seq.cast<Match> |> 
        Seq.map 
            (fun t -> 
                match t.Value with
                | Regex pattern [x; y; id; c; p] ->
                    ((x |> int, y |> int), (id |> uint32, (c |> char, p |> int)))
                | _ -> failwith "Failed (should never happen)") |>
        Seq.toList

 module Print =

    let printHand pieces hand =
        hand |>
        MultiSet.fold (fun _ x i -> forcePrint (sprintf "%d -> (%A, %d)\n" x (Map.find x pieces) i)) ()

module State =
    open System
    // Make sure to keep your state localised in this module. It makes your life a whole lot easier.
    // Currently, it only keeps track of your hand, your player numer, your board, and your dictionary,
    // but it could, potentially, keep track of other useful
    // information, such as number of players, player turn, etc.

    type state = {
        board         : Parser.board
        dict          : ScrabbleUtil.Dictionary.Dict
        playerNumber  : uint32
        hand          : MultiSet.MultiSet<uint32>
        boardLayout   : Map<coord, char>
        tiles         : Map<uint32, tile>
        tilesLeft     : uint32
    }

    let mkState b d pn h m t tl = {board = b; dict = d;  playerNumber = pn; hand = h; boardLayout = m; tiles = t; tilesLeft = tl}

    let board st         = st.board
    let dict st          = st.dict
    let playerNumber st  = st.playerNumber
    let hand st          = st.hand
    let boardLayout st = st.boardLayout
    let tiles st = st.tiles
    let tilesLeft st = st.tilesLeft

module Scrabble =
    open System.Threading

    let compareTwoWords (word1 : 'a list) (word2 : 'a list) =
        if List.length word1 < List.length word2
        then word2
        else word1
    
    type dir =
        Right | Down
    
    let back ((x,y):coord) dir =
        match dir with
        | Right -> (x-1,y)
        | Down -> (x, y-1)
    
    let forward ((x,y):coord) dir =
        match dir with
        | Right -> (x+1,y)
        | Down -> (x, y+1)
        
    let findBestWord w1 w2 =
        if List.length w1 > List.length w2 then w1
        else w2
    
    let canWeBuildWordHere (st: State.state) ((x,y): coord) dir =
        match dir with
        | Right ->
            match Map.tryFind (x-1,y) st.boardLayout with 
            | None -> true
            | Some _ -> false
        | Down ->
            match Map.tryFind (x, y-1) st.boardLayout with 
            | None -> true
            | Some _ -> false


    let hasAdjacent (st: State.state) ((x,y) : coord) dir  =
        match dir with 
        | Right -> 
            st.boardLayout.ContainsKey((x,y-1)) || st.boardLayout.ContainsKey((x,y+1)) || st.boardLayout.ContainsKey((x+1,y))
        | Down ->
             st.boardLayout.ContainsKey((x-1,y)) || st.boardLayout.ContainsKey((x+1,y)) || st.boardLayout.ContainsKey((x, y+1))

    
    let rec findWord (st: State.state) (dir : dir) (positionToPlay : coord) (finalWord : (coord * (uint32 * (char * int))) list) (wordSoFar: (coord * (uint32 * (char * int))) list) =
        
        match Map.tryFind positionToPlay st.boardLayout with
        
        | None -> 
        
            MultiSet.fold (fun (acc : 'a list) key _ ->
                
                let wildCard = Map.find key st.tiles
                
                Set.fold(fun (acc' : 'a list) (charVal, pointVal) ->
                    
                    match Dictionary.step charVal st.dict with
                    | _ when hasAdjacent st positionToPlay dir ->
                        acc'
                    
                    | None -> acc'
            
                    | Some(true, nDict)-> (* Hvis den rammer en node med true, men den stadig skal søge videre *)
                        let bestWord = findBestWord ((positionToPlay, (key, (charVal, pointVal)))::wordSoFar) acc'
                        
                        findWord { st with hand = MultiSet.removeSingle key st.hand; dict = nDict} dir (forward positionToPlay dir) bestWord ((positionToPlay, (key, (charVal, pointVal)))::wordSoFar)
            
                    | Some(false, nDict) -> (* Hvis den rammer en node, som ikke er et komplet ord *)
                        findWord { st with hand = MultiSet.removeSingle key st.hand; dict = nDict} dir (forward positionToPlay dir) acc ((positionToPlay, (key, (charVal, pointVal)))::wordSoFar)
         
                ) acc wildCard
                
            ) finalWord st.hand
        
        | Some charVal ->
            //videre med bestword og ned i dics men charen skal ikke tilføjes til vores accumulator
            match Dictionary.step charVal st.dict with
            
            | None -> finalWord
            
            | Some (true, nDict) ->
                let bestWord = findBestWord wordSoFar finalWord
                findWord {st with dict = nDict} dir (forward positionToPlay dir) bestWord (wordSoFar)
                
            | Some (false, nDict) ->
                findWord {st with dict = nDict} dir (forward positionToPlay dir) finalWord (wordSoFar)
                            
    let findWordDirection (st : State.state) dir =
        Map.fold(fun (acc : 'a list) (positiontoplay: (coord)) _ ->
            if canWeBuildWordHere st positiontoplay dir then
                let wordSoFar = findWord st dir positiontoplay [] []
                findBestWord wordSoFar acc
            else
               acc 
                
        ) [] st.boardLayout
    
    let findWordOnEntireBoard (st : State.state) =
        
        if Map.isEmpty st.boardLayout then
            findWord st Right (0,0) [] []
        
        else
            //check her om vi skal kører down eller op eller hvordan
            findBestWord (findWordDirection st Right) (findWordDirection st Down)
            
    let playGame cstream pieces (st : State.state) =
        
        let rec aux (st : State.state) =
            Print.printHand pieces (State.hand st)
            
            let move = findWordOnEntireBoard st
            
            match move with
            | [] ->
                let listHand = MultiSet.toList st.hand
                send cstream (SMChange listHand)

            | _ ->
                debugPrint (sprintf "Player %d -> Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.
                send cstream (SMPlay move)

            let msg = recv cstream
            
            debugPrint (sprintf "Player %d <- Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.

            match msg with
            | RCM (CMPlaySuccess(ms, points, newPieces)) ->
                  
                (*-------------------------Updates the board layout-------------------------*)
                let newBoardLayout = List.foldBack(fun (a,(_,y)) -> Map.add a (fst y)) ms (State.boardLayout st) 
                 
                (*-------------------------Updates hand-------------------------*)  
                let tilesToRemove = List.foldBack(fun (_,y) acc -> fst y :: acc) ms []
                
                let upTilesLeft = st.tilesLeft - uint32(List.length tilesToRemove)
                
                (*printfn "------TILES TO BE REMOVED------"
                tilesToRemove |> Seq.iter (printfn "New letter: %d")
                printfn "-------------------------------"*)
                
                let tempHand = List.foldBack(fun a -> MultiSet.removeSingle a) tilesToRemove st.hand
                
                (*printfn "------INCOMING TILES------"
                newPieces |> Seq.iter (printfn "New letter: %A")
                printfn "-------------------------"*)
                
                let newHand = List.foldBack (fun (letter,numOfTimes) -> MultiSet.add letter numOfTimes) newPieces tempHand
                
                let st' = State.mkState (State.board st) (State.dict st) (State.playerNumber st) newHand newBoardLayout st.tiles upTilesLeft
                
                
                aux st'
            | RCM (CMPlayed (pid, ms, points)) ->
                (* Successful play by other player. Update your state *)
 
                let st' = st
                aux st'
            | RCM (CMPlayFailed (pid, ms)) ->
                (* Failed play. Update your state *)
                let st' = st
                aux st'
            | RCM (CMGameOver _) -> ()
            | RCM (CMChangeSuccess newTiles) ->
                let newHand = List.fold(fun acc (id, numOfTimes) -> MultiSet.add id numOfTimes acc) MultiSet.empty newTiles
                
                let st' = State. mkState (State.board st) (State.dict st) (State.playerNumber st) newHand st.boardLayout st.tiles st.tilesLeft
                aux st'
                
            | RCM a -> failwith (sprintf "not implmented: %A" a)
            | RGPE err -> printfn "Gameplay Error:\n%A" err; aux st


        aux st
       
    let startGame 
            (boardP : boardProg) 
            (dictf : bool -> Dictionary.Dict) 
            (numPlayers : uint32) 
            (playerNumber : uint32) 
            (playerTurn  : uint32) 
            (hand : (uint32 * uint32) list)
            (tiles : Map<uint32, tile>)
            (timeout : uint32 option) 
            (cstream : Stream) =
        debugPrint 
            (sprintf "Starting game!
                      number of players = %d
                      player id = %d
                      player turn = %d
                      hand =  %A
                      timeout = %A\n\n" numPlayers playerNumber playerTurn hand timeout)

        //let dict = dictf true // Uncomment if using a gaddag for your dictionary
        let dict = dictf false // Uncomment if using a trie for your dictionary
        let board = Parser.mkBoard boardP
                  
        let handSet = List.fold (fun acc (x, k) -> MultiSet.add x k acc) MultiSet.empty hand

        fun () -> playGame cstream tiles (State.mkState board dict playerNumber handSet Map.empty tiles 100u)
        