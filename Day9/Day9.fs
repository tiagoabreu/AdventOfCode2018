module Day9

type Item =
    {
    Value : int
    mutable Next : Item option
    mutable Previous : Item option
    }

let initializeList value =
    let initialValue = { Value = value; Next = None; Previous = None}
    initialValue.Next <- Some initialValue
    initialValue.Previous <- Some initialValue
    initialValue

let insertAt pos value head = 
    let rec insertAtRec idx curItem =
        if (idx = pos) then
            curItem.Next <- Some { Value = value; Next = curItem.Next; Previous = Some curItem}
            curItem.Next.Value.Next.Value.Previous <- curItem.Next 
            curItem.Next.Value
        else 
            insertAtRec (idx + 1) curItem.Next.Value
    insertAtRec 0 head

let getItemAt pos head =
    let rec getItemAtRec idx curItem =
        if (idx = pos) then
            curItem.Value
        elif (pos > 0) then
            getItemAtRec (idx + 1) curItem.Next.Value
        else
            getItemAtRec (idx - 1) curItem.Previous.Value
    
    getItemAtRec 0 head

let removeItemAt pos head =
    let rec removeItemAtRec idx curItem =
        if (idx = pos) then
            curItem.Previous.Value.Next <- curItem.Next
            curItem.Next.Value.Previous <- curItem.Previous
            curItem.Next.Value
        elif (pos > 0) then
            removeItemAtRec (idx + 1) curItem.Next.Value
        else
            removeItemAtRec (idx - 1) curItem.Previous.Value
    
    removeItemAtRec 0 head

let playMarbles numPlayers lastMarble =
    let playerPoints = Array.init numPlayers (fun _ -> 0 |> int64)
    let mutable currentMarble = initializeList 0
    let mutable playerIdx = 0

    for marbleToPlay = 1 to lastMarble do
        if (marbleToPlay % 23 = 0) then
            let marblePoint = getItemAt (-7) currentMarble
            Array.set playerPoints playerIdx (playerPoints.[playerIdx] + int64 (marblePoint + marbleToPlay))
            currentMarble <- removeItemAt (-7) currentMarble
        else
            currentMarble <- insertAt 1 marbleToPlay currentMarble
        playerIdx <- (playerIdx + 1) % numPlayers
        
    Array.max playerPoints