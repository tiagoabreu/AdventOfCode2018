module Day1
open CommonLib.Files

type Leaf = {value:int; left:Leaf option; right:Leaf option}

let rec insertLeaf (tree:Leaf option) (newValue:int) =
    match tree with 
    | Some t -> if newValue < t.value then {t with left=Some (insertLeaf t.left newValue)}
                else if newValue > t.value then {t with right=Some (insertLeaf t.right newValue)}
                else t
    | None -> {value=newValue; left=None; right=None}

let rec searchForLeaf (tree:Leaf option) (searchFor:int) : Leaf option = 
    match tree with 
    | Some t -> if searchFor = t.value then Some t
                else if searchFor < t.value then searchForLeaf t.left searchFor
                else if searchFor > t.value then searchForLeaf t.right searchFor
                else None
    | None -> None

// Credit to https://lukemerrett.com/fsharp-binary-search-tree/

let getIntFromStringWithSignal s =
    System.Int32.Parse(s, System.Globalization.NumberStyles.AllowLeadingSign)

let getNewFrequency currentFrequency change =
    currentFrequency + change

let getIntValuesFromFile fileName =
    getLineValuesFromFilePath fileName
    |> Array.map getIntFromStringWithSignal 

let getInitialResultingFrequency (fileName:string) (initialFrequency:int) =
    getIntValuesFromFile fileName
    |> Array.fold getNewFrequency initialFrequency

let getFirstDuplicateFrequency (fileName:string) (initialFrequency:int) =
    let values = getIntValuesFromFile fileName
    let mutable currentFrequency = initialFrequency
    let mutable tree = Some { value=0; left=None; right=None}
    let mutable doubleFrequency = 0
    while (doubleFrequency = 0) do
        for i in values do
            currentFrequency <- getNewFrequency currentFrequency i
            let duplicateValue = searchForLeaf tree currentFrequency
            if (duplicateValue <> None && doubleFrequency = 0)
            then doubleFrequency <- currentFrequency
            else tree <- Some (insertLeaf tree currentFrequency)
    doubleFrequency