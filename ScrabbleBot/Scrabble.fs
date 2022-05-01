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
    }

    let mkState b d pn h m t = {board = b; dict = d;  playerNumber = pn; hand = h; boardLayout = m; tiles = t}

    let board st         = st.board
    let dict st          = st.dict
    let playerNumber st  = st.playerNumber
    let hand st          = st.hand
    let boardLayout st = st.boardLayout
    let tiles st = st.tiles

module Scrabble =
    open System.Threading

    let compareTwoWords (word1 : 'a list) (word2 : 'a list) =
        if List.length word1 < List.length word2
        then word2
        else word1

    let nextPosition (x,y):coord = 
        (x+1, y)
    
    let rec findWords (st: State.state) (positionToPlay : coord) (finalWordsList : ((coord * (uint32 * (char * int))) list) list) (wordSoFar: (coord * (uint32 * (char * int))) list) =
        
        MultiSet.fold (fun (acc : 'a list) key value ->
            let (charVal, pointVal) = Map.find key st.tiles |> Set.maxElement
            
            match Dictionary.step charVal st.dict with
            | None -> acc
            
            | Some(true, nDict)-> (* Hvis den rammer en node med true, men den stadig skal søge videre *)
                let newWordList = ((positionToPlay, (key, (charVal, pointVal)))::wordSoFar) :: acc
                findWords { st with hand = MultiSet.removeSingle key st.hand; dict = nDict} (nextPosition positionToPlay) newWordList ((positionToPlay, (key, (charVal, pointVal)))::wordSoFar)
            
            | Some(false, nDict) -> (* Hvis den rammer en node, som ikke er et komplet ord *)
                findWords { st with hand = MultiSet.removeSingle key st.hand; dict = nDict} (nextPosition positionToPlay) acc ((positionToPlay, (key, (charVal, pointVal)))::wordSoFar)
         
        ) finalWordsList st.hand
    
    let findBestPlay words =
        words |> List.maxBy (fun s -> List.length s)
        
    let playGame cstream pieces (st : State.state) =
        
        let rec aux (st : State.state) =
            Print.printHand pieces (State.hand st)
            
            let words = findWords st (0,0) [] []
            
            let move = findBestPlay words
            
            // remove the force print when you move on from manual input (or when you have learnt the format)
            //forcePrint "Input move (format '(<x-coordinate> <y-coordinate> <piece id><character><point-value> )*', note the absence of space between the last inputs)\n\n"
            (*let input = ""//getStringFromWord st wordToPlay
            let move = RegEx.parseMove input*)
                        
            debugPrint (sprintf "Player %d -> Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.
            send cstream (SMPlay move)

            let msg = recv cstream
            
            debugPrint (sprintf "Player %d <- Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.

            match msg with
            | RCM (CMPlaySuccess(ms, points, newPieces)) ->
                (* Successful play by you. Update your state (remove old tiles, add the new ones, change turn, etc) *)
                  
                (*-------------------------Updates the board layout-------------------------*)
                
                let newBoardLayout = List.foldBack(fun (a,(_,y)) -> Map.add a (fst y)) ms (State.boardLayout st) 
                 
                (*-------------------------Updates hand-------------------------*)  
                let tilesToRemove = List.foldBack(fun (_,y) acc -> fst y :: acc) ms []
                
                printfn "------TILES TO BE REMOVED------"
                tilesToRemove |> Seq.iter (printfn "New letter: %d")
                printfn "-------------------------------"
                
                //tempHand removes the played tiles
                let tempHand = List.foldBack(fun a -> MultiSet.removeSingle a) tilesToRemove st.hand
                     
                //Adds newPieces to the hand
                printfn "------INCOMING TILES------"
                newPieces |> Seq.iter (printfn "New letter: %A")
                printfn "-------------------------"
                
                let newHand = List.foldBack (fun (letter,numOfTimes) -> MultiSet.add letter numOfTimes) newPieces tempHand
                
                let st' = State.mkState (State.board st) (State.dict st) (State.playerNumber st) newHand newBoardLayout st.tiles
                
                
                aux st'
            | RCM (CMPlayed (pid, ms, points)) ->
                (* Successful play by other player. Update your state *)
                //let st' = st // This state needs to be updated
                (*let tilesToBeRemoved = List.foldBack(fun (x,y) acc -> fst y :: acc) move []
                
                let newHand = List.foldBack(fun a -> MultiSet.removeSingle(a st.hand)) tilesToBeRemoved st.hand
                let st' = mkState ((board st) (dict st) (playerNumber st) ())*)
                let st' = st
                aux st'
            | RCM (CMPlayFailed (pid, ms)) ->
                (* Failed play. Update your state *)
                let st' = st // This state needs to be updated
                aux st'
            | RCM (CMGameOver _) -> ()
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

        fun () -> playGame cstream tiles (State.mkState board dict playerNumber handSet Map.empty tiles)
        