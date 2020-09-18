open System

type Suits =
    | Spade = 1
    | Heart = 2
    | Diamond = 3
    | Club = 4
    | Joker = 99

type PlayingCard =
    { Number : int
      Suit   : Suits }

type Result =
    | PlayerWon
    | DealerWon
    | DrawGame

let (|InputYes|InputNo|InvalidInput|) (s:string) =
    match s with
    | "y" | "Y" | "Yes" | "yes" | "YES" -> InputYes
    | "n" | "N" | "No" | "no" | "NO" -> InputNo
    | _ -> InvalidInput

let rand = Random()

let cardToString card =
    match card.Suit with
    | Suits.Spade -> sprintf "S%2d" card.Number   
    | Suits.Club -> sprintf "C%2d" card.Number
    | Suits.Diamond -> sprintf "D%2d" card.Number
    | Suits.Heart -> sprintf "H%2d" card.Number
    | Suits.Joker -> sprintf "JOKER"
    | _ -> "" 

let drawHand hand =
    hand |> List.map cardToString |> List.iter (fun s -> printf "%s " s)
    printfn ""

let isCardExist card (deck:List<PlayingCard>) =
    deck |> List.exists (fun x -> x.Number = card.Number && x.Suit = card.Suit)

let rec pickCard hand deck =
    let card = { PlayingCard.Number = rand.Next(1,13); 
                 PlayingCard.Suit = rand.Next(1,4) |> enum<Suits> }
    if isCardExist card deck then pickCard hand deck
    else hand @ [card], deck @ [card]
    
let initGame =
    let pickCardTwice hand deck =
        let h',d' = pickCard hand deck
        pickCard h' d'
    let playerHand, deck' = pickCardTwice [] [] 
    let dealerHand, deck'' = pickCardTwice [] deck'
    playerHand,dealerHand,deck''

let calcScore hand =
    let temp = hand |> List.sumBy(fun x -> if x.Number > 10 then 10 else x.Number)
    hand |> List.fold (fun x y ->
        if y.Number = 1 && temp <= 11 then temp + 10 else temp) 0

let showResult playerhand dealerhand result =
    printfn "----- Result -----"
    printf "Player's hand : "
    playerhand |> drawHand
    printfn "Score : %d" (playerhand |> calcScore)
    printf "Dealer's hand : "
    dealerhand |> drawHand
    printfn "Score : %d" (dealerhand |> calcScore)
    match result with
    | PlayerWon -> printfn "You Won!"
    | DealerWon -> printfn "You Lose..."
    | DrawGame -> printfn "Draw game"

let judgeGame playerHand dealerHand =
    let playerScore = playerHand |> calcScore
    let dealerScore = dealerHand |> calcScore
    match playerScore > dealerScore, playerScore > 21 ,dealerScore > 21 , playerScore = dealerScore with
    | _,_,_,true -> showResult playerHand dealerHand DrawGame
    | true,false,_,_ | _,false,true,_ -> showResult playerHand dealerHand PlayerWon 
    | false,_,false,_ | _,true,_,_ -> showResult playerHand dealerHand DealerWon
    

let gameStart =
    let playerHand,dealerHand,deck = initGame
    
    printf "Player's hand : "
    playerHand |> drawHand
    printf "Dealer's hand : "
    [dealerHand.Head] |> drawHand

    let rec playerTurn playerHand deck =
        if (calcScore playerHand) > 21 then playerHand,deck 
        else
            printfn "Do you draw a card? [y/n]"
            match stdin.ReadLine() with
            | InputYes -> 
                
                    let h',d' = pickCard playerHand deck
                    printf "Player's hand : "
                    h' |> drawHand
                    playerTurn h' d'
            | InputNo -> playerHand, deck
            | _ -> 
                printfn @"y または n を入力してください！"
                playerTurn playerHand deck

    let rec dealerTurn dealerHand deck =
        if (calcScore dealerHand) < 17 then 
            let h',d' = pickCard dealerHand deck
            dealerTurn h' d'
        else dealerHand,deck

    let playerHand', deck' = playerTurn playerHand deck
    if playerHand' |> calcScore > 21 then 
        showResult playerHand' dealerHand DealerWon
    else
        let dealerHand', _ = dealerTurn dealerHand deck'
        judgeGame playerHand' dealerHand'
    
[<EntryPoint>]
let main argv =
    gameStart
    0
